;; For implementation reasons code must be in module and
;; module must be searchable in GUILE_LOAD_PATH.
(define-module (overview-fixed)
    #:export (mystic))

(use-modules (thales seal))

(sealed mystic
	(1 2 *** (- 4 1))	;; Short-cut for most common case. Expected form is evaluated
	(1 2 *+* 3)		;; Same. Expected form is not evaluated.
	((& 5 6) => 11)         ;; Full form. & is bound to function beeing testing.
	((mystic 3 4) => 7)	;; Or it can be written in full
	(1 2 *** 3))		;; This will no more fail

(define (mystic x y)
    "Some mystic function of two arguments."
    (+ x y))

(sealed not-exported
	(1 2 *+* 2)
	(0 2 *!* numerical-overflow)) ;; Expected to throw numerical-overflow

(define (not-exported x y)
    "Some not-exported function"
    (/ y x))

