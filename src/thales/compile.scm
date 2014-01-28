(define-module (thales compile)
    #:use-module (thales seal)
    #:use-module (thales syntax)
    #:use-module (system base compile)
    #:use-module (thales prepare)
    #:export (perform-compile mkdir-p with-current-working-directory
			      touch-p))
(define (path-absolute? path)
    (string-starts-with path "/"))

(define-syntax with-current-working-directory
    (syntax-rules ()
	((_ expr ...)
	 (let ((saved-cwd (getcwd)))
	     (catch #t
		    (lambda () expr ... (chdir saved-cwd))
		    (lambda args (chdir saved-cwd) (apply throw args)))))))

(define-syntax with-ignored-exceptions
    (syntax-rules ()
	((_ expr ...)
	 (catch #t (lambda () expr ...)
		   (const #f)))))

(define (mkdir-p path)
    (define (make-and-step dir)
	(unless (file-exists? dir)
		(mkdir dir))
	(chdir dir))
    (with-current-working-directory
        (when (path-absolute? path)
	      (chdir "/"))
	(for-each make-and-step
		  (string-split path #\/))))

;; FIXME: Need more accurate errors handling
(define (touch-p path)
    (with-ignored-exceptions
        (mkdir-p path)
	(rmdir path))
    path)

(sealed modname->relative-filename
	([& '(foo bar)] => "foo/bar.scm")
	([& '(baz baf) #:extension ".go"] => "baz/baf.go"))

(define* (modname->relative-filename module #:key (extension ".scm"))
    (string-append (string-join (map symbol->string module) "/")
		   extension))

(sealed relative-to
	([& "/home/kaction" "thales"] => "/home/kaction/thales"))
(define (relative-to basename fname)
    (format #f "~a/~a" basename fname))

(define* (perform-compile modules #:key (srcdir ".") (outdir "."))
    (define (source-filename modname)
	(relative-to srcdir (modname->relative-filename modname)))
    (define (output-filename modname)
	(relative-to outdir (modname->relative-filename modname
							#:extension ".go")))
    (define (toplevel-eval form) (eval form (resolve-module '(guile))))
    (for (modname in modules)
	 (define source-fname (source-filename modname))
	 (define output-fname (output-filename modname))
	 (toplevel-eval `(define *thales-current-filename* ,source-fname))
	 (compile-file source-fname
		       #:output-file (touch-p output-fname)
		       #:env (current-module))))
