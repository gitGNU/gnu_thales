(define-module (thales seal)
    #:export (sealed))
(use-modules (thales syntax))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))

(define-syntax push
    (syntax-rules ()
        ((_ val list)
         (set! list (cons val list)))))

(define* (module->filename module)
    (string-append (string-join (map symbol->string (module-name module)) "/")))

(define load-self-once
    (let ((already-loaded '()))
        (lambda ()
            "Load current module, unless it was not already
loaded with this function."
            (unless (member (current-module) already-loaded)
                (primitive-load-path (current-module-load-filename))
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
             (call-with-values #[catch #t
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
		(error:handle #[error:broken-seal '<form> expected]))
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
		(error:handle #[error:broken-seal '<form>
                                    (append expected '(....))]))
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
    (syntax-rules (=> !--> ***)
	([_ f <args> ... *** <val>]
	 (seal-clause-expect-values (f <args> ...) (<val>)))
	([_ f <form> => <val> ...]
	 (seal-clause-expect-values <form> (<val> ...)))
	([_ f <form> !--> <val> ...]
	 (seal-clause-expect-throw <form> (<val> ...)))
	([_ f <form>]
	 (seal-clause-expect-values <form> (#t)))))

(define-syntax sealed
    (lambda (env)
        (syntax-case env ()
            ((_ f (obj ...) ...)
             (with-syntax ((& (datum->syntax env '&)))
                 #'(eval-when (compile)
		       (load-self-once)
		       (let ((& f))
			   (format #t "Checking seals with & = ~a... " 'f)
			   (seal-clause f obj ...) ...
			   (format #t "ok\n"))))))))
