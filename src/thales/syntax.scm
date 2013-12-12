(define-module (thales syntax)
    #:use-module (srfi srfi-1)
    #:use-module (srfi srfi-26)
    #:use-module (ice-9 match)
    #:re-export (cute)
    #:export (lambda-match for for* define-match))

(define (cute-reader ch stream)
    (define (append-<...> list)
        (if (eq? (last list) '<...>)
            list
            (append list '(<...>))))
    (unread-char ch stream)
    (cons '(@ (srfi srfi-26) cute)
           (append-<...> (read stream))))

(eval-when (eval load compile)
    (read-hash-extend #\[
        (lambda (ch stream)
            ((@@ (thales syntax) cute-reader) ch stream))))

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

(define-syntax define-match
    (syntax-rules ()
        ((_ (name args ...) expr ...)
         (define name (lambda-match (args ...) expr ...)))))

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
