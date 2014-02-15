(define-module (thales seal)
    #:export (sealed))
(use-modules (ice-9 match))
(use-modules (ice-9 pretty-print))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))

(define-syntax push
    (syntax-rules ()
        ((_ val list)
         (set! list (cons val list)))))

(define (current-module-name)
    (module-name (current-module)))
(define (current-module-load-filename)
    (string-join (map symbol->string (current-module-name)) "/"))

(define (seal:pretty-print obj)
    "Output object on stderr if env THALES_SEAL is set.

This function is used to output valid Guile program to check seals."
    (when (getenv "THALES_SEAL")
	(format (current-error-port) "~a"
		(with-output-to-string [cute pretty-print obj]))))

(define (in-module?)
    (module-filename (current-module)))

(define load-self-once
    (let ((already-loaded '()))
	(lambda ()
	    "Load current module, unless it was not already
loaded with this function."
	    (unless (member (current-module) already-loaded)
		(seal:pretty-print `(use-modules ,(current-module-name)))
		(seal:pretty-print `(use-modules (thales seal)))
		(when (in-module?)
		    (primitive-load-path (current-module-load-filename)))
		(push (current-module) already-loaded)))))

(define (error:broken-seal form expect result)
    (format #t "\nCompilation aborted: seal broken when evaluating\n")
    (pretty-print form)
    (format #t "\nExpect:\n")
    (pretty-print expect)
    (format #t "\nActual:\n")
    (pretty-print result)
    (exit 1))

(define-syntax call-and-catch
    (syntax-rules ()
        ((_ <form> <on-values> <on-throw>)
         (let ((throw-handler-called #f))
             (call-with-values [cute catch #t
                                       (lambda () <form>)
                                       (lambda args
                                           (apply <on-throw> args)
                                           (set! throw-handler-called #t))]
                 (lambda args
                     (unless throw-handler-called
                             (apply <on-values> args))))))))

(define-syntax seal-clause-expect-values
    (syntax-rules ()
	([_ <form> (<val> ...)]
	 (let* ((expected (list 'values <val> ...))
		(error:handle [cute error:broken-seal '<form> expected <>]))
	     (call-and-catch <form>
	         (lambda args
		     (unless (equal? args (cdr expected))
			 (error:handle (cons 'values args))))
		 (lambda throw-args
		     (error:handle (cons 'throw throw-args))))))))

(define-syntax seal-clause-expect-throw
    (syntax-rules ()
	([_ <form> (<val> ...)]
	 (let* ((expected (list 'throw <val> ...))
		(error:handle [cute error:broken-seal '<form>
                                    (append expected '(....)) <>]))
	     (call-and-catch <form>
                 (lambda args
		     (error:handle (cons 'values args)))
		 (lambda throw-args
		     (define (throw-args-match expected-args actual-args)
			 (and (<= (length expected-args) (length actual-args))
			      (every equal? expected-args actual-args)))
		     (unless (throw-args-match (cdr expected) throw-args)
			 (error:handle (cons 'throw throw-args)))))))))

(define-syntax seal-clause
    (syntax-rules (=> !--> *** *+* *!*)
	([_ f <args> ... *** <val>]
	 (seal-clause-expect-values (f <args> ...) (<val>)))
	([_ f <args> ... *+* <val>]
	 (seal-clause-expect-values (f <args> ...) ('<val>)))
	([_ f <form> => <val> ...]
	 (seal-clause-expect-values <form> (<val> ...)))
	([_ f <form> !--> <val> ...]
	 (seal-clause-expect-throw <form> (<val> ...)))
	([_ f <args> ... *!* <val>]
	 (seal-clause-expect-throw (f <args> ...) ('<val>)))
	([_ f <form>]
	 (seal-clause-expect-values <form> (#t)))))

(define-syntax sealed
    (lambda (env)
        (syntax-case env ()
            ((_ f (obj ...) ...)
             (with-syntax ((& (datum->syntax env '&)))
                 #'(eval-when (compile)
		       (load-self-once)
		       (when (module-variable (module-public-interface
					       (current-module))
					      'f)
			   (seal:pretty-print `(sealed ,'f (obj ...) ...)))
		       (let ((& f))
			   (format #t "Checking seals with & = ~a... " 'f)
			   (seal-clause f obj ...) ...
			   (format #t "ok\n"))))))))
