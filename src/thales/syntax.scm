(define-module (thales syntax)
    #:use-module (srfi srfi-26)
    #:use-module (ice-9 match)
    #:re-export (cute)
    #:export (lambda-match for for*))

(eval-when (eval load compile)
	   (read-hash-extend #\[
			     (lambda (ch stream)
				 (unread-char ch stream)
				 (cons 'cute (read stream)))))
(define-syntax nested-match
    (syntax-rules ()
	(( _ (var) (pat) exp exps ...)
	 (match var (pat exp exps ...)))
	(( _ (var vars ...) (pat pats ...) exp exps ...)
	 (match var (pat (nested-match (vars ...) (pats ...) exp exps ...))))))


(define-syntax lambda-match
    (lambda (x)
	(syntax-case x ()
	    ((_ (pat ...) exp exps ...)
	     (with-syntax
	      (((t ...) (generate-temporaries #'(pat ...))))
	      #'(lambda (t ...)
		    (nested-match (t ...) (pat ...) exp exps ... )))))))

(define-syntax for
    (syntax-rules (=>)
	((_ (key => val) in hash exp exps ...)
	 (hash-for-each (lambda-match (key val) exp exps ...) hash))
	((_ (pat in list) exp exps ...)
	 (for-each (lambda-match (pat) exp exps ...) list))))

(define-syntax for*
    (syntax-rules (=>)
	((_ ((key => val) in hash) exp exps ...)
	 (hash-map->list (lambda-match (key val) exp exps ...) hash))
	((_ (pat in list) exp exps ...)
	 (map (lambda-match (pat) exp exps ...) list))))
