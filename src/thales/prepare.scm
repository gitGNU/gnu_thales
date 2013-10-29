(define-module (thales prepare)
    #:export (relpath->module-name
	      string-starts-with
	      string-strip-prefix
	      flatten-file-system-tree
	      list-modules
	      check-resolution
	      perform-configure)
    #:use-module (thales core-modules)
    #:use-module (thales syntax)
    #:use-module (ice-9 match)
    #:use-module (ice-9 ftw)
    #:use-module (srfi srfi-1))
(use-modules (thales seal))

(sealed string-starts-with
	(& "foo" "fo" => #t)
	(& "fo" "foo" => #f)
	(& "f" #\c   -->))

(define (string-starts-with str prefix)
    (define prefix-length (string-length prefix))
    (and (>= (string-length str) prefix-length)
	 (equal? prefix (substring str 0 prefix-length))))

(sealed string-strip-prefix
	(& "foo" "f" => "oo")
	(& "bar" "f" => "bar"))

(define (string-strip-prefix str prefix)
    (if (string-starts-with str prefix)
	(substring str (string-length prefix))
	str))

(sealed string-empty?
	(& "fo" => #f)
	(& ""   => #t))
(define (string-empty? str) (equal? str ""))

(sealed relpath->module-name
	(& "/foo/bar/baz.scm" => (foo bar baz))
	(& "foo/bar/baz"      => (foo bar baz)))
(define* (relpath->module-name path)
    (map (compose string->symbol #[basename <> ".scm"])
	 (filter (negate string-empty?) (string-split path #\/))))

(define (stat-directory? st)
    (eq? (stat:type st) 'directory))
(define (stat-regular? st)
    (eq? (stat:type st) 'regular))

(sealed have-extension?
	(& "foo.foo" ".foo" => #t)
	(& "barfoo"  ".foo" => #f))

(define (have-extension? str ext)
    (not (equal? str (basename str ext))))

(define (flatten-file-system-tree tree)
    (define (flatten1 l) (apply append l))
    (map reverse
	 (let recursive ((prefix '()) (branch tree))
	     (match branch
		    ((name st childs ...)
		     (cond
		      ((stat-directory? st)
		       (flatten1
			(map #[recursive (cons (string->symbol name) prefix) <>]
			     childs)))
		      ((stat-regular? st)
		       (if (have-extension? name ".scm")
			   (list (cons (string->symbol (basename name ".scm"))
				       prefix))
			   '()))
		      (else '())))
		    (_ '())))))
(define (list-modules dir)
    (map cdr
	 (flatten-file-system-tree (file-system-tree dir))))


(define (module-resolvable? mod)
    (resolve-module mod #:ensure #f))

(define (find-provider modname)
    (guile-provides? modname))


(define (check-dependency dep)
    (unless (module-resolvable? dep)
	    (throw 'unresolved-dependency dep))
    (find-provider dep))


(define (module-deps modname)
    (map module-name (module-uses
		      (resolve-module modname
				      #:ensure #f))))

(define* (perform-configure modules #:key (unknown-sourced (const #t)))
    (delete-duplicates
     (filter symbol?
	     (apply append
		    (for* (mod in modules)
			  (format #t "Module ~a.\n" mod)
			  (unless (module-resolvable? mod)
				  (throw 'unresolved-module mod))
			  (for* (dep in (module-deps mod))
				(format #t " Checking for ~a... " dep)
				(if (member dep modules)
				    (format #t "self\n")
				    (let ((provider (check-dependency dep)))
					(if provider (format #t "~a\n" provider)
					    (begin
						(unknown-sourced dep)
						(format #t "unknown\n")))
					provider))))))))
