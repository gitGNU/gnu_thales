(define-module (test)
    #:export (frap))
(use-modules (seal))

(sealed frap
    (frap 1 => 9))
(define (frap x)
    (+ (bar x) 8))



(sealed bar
    (& 5 => 1)
    (& 3 => 9))
(define (bar y)
    (* y y))
