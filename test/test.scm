;; (define-module (foo))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))
(use-modules (srfi srfi-26))

(eval-when (eval load compile)
	   (read-hash-extend #\[ (lambda (ch stream)
				     (unread-char ch stream)
				     (cons 'cute (read stream)))))

(define (error:broken-seal form expect result)
    (error 'broken-seal (format #f "Compilation aborted: seal broken.
Eval: ~a\nExpect: ~a\nReceived: ~a\n"
		   form expect result)))

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

(define-syntax seal-clause
    (syntax-rules (=> -->)
	((_ <form>) (seal-clause <form> => #t))
	((_ <form> => <val> <vals> ... )
	 (let ((expected (cons 'values '(<val> <vals> ...))))
	     (call-and-catch <form>
                 (lambda args
		     (unless (equal? args (cdr expected))
			     (error:broken-seal '<form>
						expected
						(cons 'values args))))
		 (lambda throw-args
		     (error:broken-seal '<form> expected (cons 'throw throw-args))))))
	((_ <form> --> <val> ...)
	 (let ((expected (cons 'throw '(<val> ...))))
	     (call-and-catch <form>
                (lambda args
		    (error:broken-seal '<form>
				       (append expected '(....))
				       (cons 'values args)))
		(lambda throw-args
		    (unless (and (<= (length (cdr expected)) (length throw-args))
				 (every equal? (cdr expected) throw-args))
			    (error:broken-seal '<form>
					       (append expected '(....))
					       (cons 'throw throw-args)))))))
	((_ obj ...)
	 (error "Macro seal-clause usage:
Assert, that evaluation of EXPR returns value(s).
    (seal-clause expr => value [values])
Assert, that evaluation of EXPR is #t
    (seal-clause expr)
Assert, that evaluation of EXPR throws with argument list, starting with value(s)
    (seal-clause expr --> value [values])
In particular, (seal-clause expr -->) asserts, that EXPR throws something."))))

(define (frap x)
    (+ (bar x) 8))

(define (bar y)
    (* y y))
