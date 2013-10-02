(define-module (functional prelude))

(define-public (partial-apply fn . args)
  "Return function `fn` with first arguments bound with args."
  (lambda (. rest) (apply fn (append args rest))))

(define-public (uncurry fn)
  "Make function `fn` accept first, dooted pair argument as two arguments.."
  (lambda (x y . rest) (apply fn (cons x y) rest)))

(define-public (curry fn)
  "Make function fn to accept first two arguments as dotted pair."
  (lambda (pair . rest) (apply fn (car pair) (cons pair) rest)))

(define-public (flip fn)
  "Return function `fn` with first two arguments flipped."
  (lambda (x y . rest) (apply fn y x rest)))

(define-public (left-fold fn init val-list)
  "Left fold :: (a -> b -> a) -> a -> [b] -> a"
  (define (left-fold-iter acc vlist)
    (if (null? vlist) acc
        (left-fold-iter (fn acc (car vlist)) (cdr vlist))))
  (left-fold-iter init val-list))

(define-public (right-fold fn init val-list)
  "Right fold :: (a -> b -> b) -> b -> [a] -> b "
  (define (right-fold-iter acc vlist)
    (if (null? vlist) acc
        (right-fold-iter (fn (car vlist) acc) (cdr vlist))))
  (right-fold-iter init val-list))

(define-public (iterate fn init length)
  "Iterate function `fn` over value `init` `length` times to produce list."
  (define (iterate-iter prev vlist count)
    (let ((next (delay (fn prev))))
      (if (zero? count) vlist
          (iterate-iter (force next) (cons (force next) vlist) (1- count)))))
  (reverse (iterate-iter init '() length)))

(define-public (iterate& fn init)
  (let ((value init))
    (lambda () (set! value (fn value)) value)))
