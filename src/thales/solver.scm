;;; solver.scm --- Thales solver --- implementation of core idea

;; Copyright (C) 2013 Dmitry Bogatov <KAction@gnu.org>

;; Author: Dmitry Bogatov <KAction@gnu.org>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Core idea of Thales is packages manager, that eliminates dependencies
;;; hell. In generic case --- when every package version can depend
;;; on arbitary set of packages and their version, solution of dependencies
;;; is exponencial problem. So idea is strictly order package versions
;;; according following rules:
;;;     * Any incompatible change allowed only with major version bump.
;;;     * Adding dependencly requires major version bump.
;;;     * Increase in minor version of dependency is do not require
;;;       major version bump.
;;;     * Relaxing dependencies do not require major version bump.
;;;     * Each package may alternatively depend on several major version
;;;       of package
;;;
;;; Most of these requirements are managed automatically by seals (see
;;; seals.scm) and analysis of modules interfaces. Of course, if programmer
;;; wants to screw things, he will be able to.

;;; Currently, I have no idea how control compability of macroses.
;; Code:
(define-module (thales solver)
    :export (
	     perform-solve
	     generate-r1-contrain-solver
	     resolve-major-versions
	     make-version
	     version-compatible?
	     make-version*))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))
(use-modules (thales seal))
(use-modules (srfi srfi-26))
(use-modules (thales syntax))
(use-modules (srfi srfi-9))


(define-record-type <version>
    (make-version major minor micro)
    version?
    (major major)
    (minor minor)
    (micro micro))
(define* (make-version* #:optional (major 0) (minor 0) (micro 0))
    (make-version major minor micro))

(eval-when (load compile eval)
    (read-hash-extend #\v
        (lambda (ch stream)
	    (cons 'make-version* (read stream)))))


(sealed version-compatible?
	(#v(1 1 3) #v(1 2 2) *** #t)
	(#v(1 2 3) #v(3 2 1) *** #f))

(define-match (version-compatible? [$ <version> major1 minor1 micro1]
				   [$ <version> major2 minor2 micro2])
    (and (=  major1 major2)
	 (<= minor1 minor2)))

(sealed version-interchangeble?
	(#v(1 2 3) #v(1 2 4) *** #t)
	(#v(1 2 3) #v(1 3 0) *** #f))

(define-match (version-interchangeble? [$ <version> major1 minor1]
		                       [$ <version> major2 minor2])
    (and (= major1 major2)
	 (= minor1 minor2)))

(define-record-type <constrain>
    (make-constrain type name versions)
    constrain?
    (type      constrain-type)
    (name      constrain-name)
    (versions  constrain-versions))
(define* (make-constrain* type name #:rest versions)
    (make-constrain type name versions))

(define-record-type <package>
    (make-package name version dependencies)
    package?
    (name         package-name)
    (version      package-version)
    (dependencies package-dependencies))

(define* (make-package* name version #:rest dependencies)
    (make-package name version dependencies))

(sealed satisfy
	[(make-constrain* #:match 'foo #v(1))
	 (make-package* 'foo #v(1 2 0)) *** #t]
	[(& (make-constrain* #:match 'foo #v(2 0))
	    (make-package* 'foo #v(1 2 3)))
	 => #f]
	[(& (make-constrain* #:match 'foo #v(1 0))
	    (make-package* 'foo #v(1 2 3)))
	 => #t]
	[(& (make-constrain* #:match 'foo
			     #v(1 0)
			     #v(2 0))
	    (make-package* 'foo #v(1 2 3)))
	 => #t]
	[(& (make-constrain* #:rigid 'foo #v(1 2 3))
	    (make-package* 'foo #v(1 2 3)))
	 => #t]
	[(& (make-constrain* #:rigid 'foo
			     #v(1 2 4)
			     #v(1 2 7))
            (make-package* 'foo #(1 2 5)))
	 => #f])

(define-match (satisfy [$ <constrain> constr-type constr-name constr-versions]
		       [$ <package>   pkg-name pkg-version _])
    (define version-comparator (case constr-type
				   [(#:rigid) equal?]
				   [(#:match) version-compatible?]))
    (if (eq? constr-name pkg-name)
	(any #[version-comparator <> pkg-version] constr-versions)
	#f))


(define* (generate-r1-contrain-solver installed availible
				      #:key (conservative #f))
    "Return function, that will packages, that satisfy constrain.

Function, given constrain, return list of packages, that satistfy it,
taking only one version from each major version. Order of packages influenced
by list of INSTALLED packages, if CONSERVATIVE is #f.
"
    (let* ((cache (make-hash-table))
	   (cache-get #[hash-ref  cache])
	   (cache-put #[hash-set! cache]))
	(lambda* (constr #:optional major-only)
	    (or (cache-get constr)
		(let ((result (filter #[satisfy constr] availible)))
		    (cache-put constr result)
		    result)))))

(sealed enumerate-list-combinations
	([& '((1 2))]       => '((1) (2)))
	([& '((1 2) (3 4))] => '((1 3) (1 4) (2 3) (2 4))))

(define-match (enumerate-list-combinations (head rest ...))
    "Enumerate list combinations.

FIXME: EXTREMELY INEFFICENT
"
    (if (null? rest)
	(map list head)
	(let ((recursive-processed (enumerate-list-combinations rest)))
	    (concatenate (map (lambda (arg)
				  (map #[cons arg] recursive-processed))
			     head)))))

(sealed package-duplicate?
	([& (make-package* 'foo (make-version* 1 2))
	    (make-package* 'foo (make-version* 1 4))]
	 => #t))

(define-match (package-duplicate? [$ <package> name1 [$ <version> major1]]
				  [$ <package> name2 [$ <version> major2]])
    (if (eq? name1 name2)
	(or (= major1 major2) (throw 'conflict))
	#f))


(eval-when (compile)
	   (define foo-102  (make-package* 'foo  (make-version* 1 0 2)))
	   (define bar-201  (make-package* 'bar  (make-version* 2 0 1)))
	   (define quaz-123 (make-package* 'quaz (make-version* 1 2 3))))

;; (sealed optimize-pkglist
;; 	([& (list foo-102 bar-201)
;; 	    '()]
;; 	 => '())
;; 	([& (list foo-102 bar-201)
;; 	    (list foo-102 quaz-123)]
;; 	 => (list quaz-123)))

;; (sealed optimize-pkglist
;; 	([& '((foo (1 0 2))
;; 	      (bar (2 0 1)))
;; 	    '((quaz (1 2 3))
;; 	      (foo (1 0 3)))]
;; 	 => '((quaz (1 2 3))))

;; 	([& '() '((foo (1 0 0)))]
;; 	 => '((foo (1 0 0))))

;; 	([& '((foo (1 0 2)))
;; 	    '((foo (2 1 2)))]
;; 	 => #f))

;; Return list of packages in NEW, but not in STABLE.  If packages in stable
;; STABLE and NEW in union conflict, return #f.
(define (optimize-pkglist stable new)
    (catch 'conflict
	(let iterate-new-packages ((result '())
				   (used    stable)
				   (new     new))
	    (if (null? new) result
		(let ((current (car new))
		      (rest    (cdr new)))
		    (if (any #[package-duplicate? current] used)
			(iterate-new-packages result used rest)
			(iterate-new-packages (cons current result)
					      (cons current used)
					      rest)))))
	(const #f)))

;; (define-match (iterate-new-packages result used
;; 				    (new:head new:rest ...))
;;     (if (any #[package-duplicate? new:head] used)
;; 	(if (null? new:rest) result
;; 	    (iterate-new-packages result used new:rest))

;; 	)
;;     )

;; (let iterate-new-packages ((result '())
;; 			   (new     new)
;; 			   (used    stable))
;;     (match new

;; 	)
;;     (if (null? new) result
;; 	(if (any package-duplicate?))

;; 	)

;;     )

;; (define-match (duplicate? [$ <package> new-name
;; 			     [$ <version> new-major]])
;;     ;; Return #t if exists packages of same name and major version in STABLE,
;;     ;; throw 'conflict, if major versions do not match and #f otherwise.
;;     (define-match (pair-pkg-duplicate? [$ <package> pkg-name
;; 					  [$ <version> pkg-major]])
;; 	(if (eq? pkg-name new-name)
;; 	    (or (= pkg-major new-major) (throw 'conflict))
;; 	    #f))
;;     (if (null? stable) #f
;; 	(any pair-pkg-duplicate? stable)))
;; (catch 'conflict #[filter (negate duplicate?) new] (const #f))

;; (define (resolve-major-versions major-resolver constrains proposed)
;;     (define enum/list enumerate-list-combinations)
;;     (define possible-pkg-solutions ;; ((pkg))
;; 	(filter-map (compose #[optimize-pkglist proposed] major-resolver)
;; 		    constrains))
;;     (define (pkg->depend-lists pkg)
;; 	    (cddr pkg)) ;; ((constr))
;;     (newline)
;;     (display possible-pkg-solutions)
;;     (newline)
;;     (newline)
;;     (display (car possible-pkg-solutions))
;;     (newline)
;;     (display (enum/list (map pkg->depend-lists (car possible-pkg-solutions))))
;;     (newline)
;;     (for (solution in possible-pkg-solutions)
;; 	(for (next-constrains in (enum/list (map pkg->depend-lists solution)))
;; 	    (if (null? next-constrains)
;; 		(throw 'found-result (cons solution proposed)))))
;;     #f)


;; (define* (perform-solve installed availible constrains
;; 			#:key (conservative #f))
;;     "Resolve CONSTRAINS with AVAILIBLE packages.

;; If CONSERVATIVE if #t, prefer use of INSTALLED packages, otherwise prefer
;; new versions.

;; CONSTRAINS is list of constrains, that have following kinds:

;;     * Ridid request for specified package. To be used, if bug happens
;;       and dependency have to be resolved manually.
;;         (<pkg-name> = (<major> <minor> <micro>) ...)
;;     * Request for package version, no less that specified.
;;         (<pkg-name> ? (<major> <minor>) ... )

;; Both INSTALLED and AVAILIBLE are lists of package in form
;;     (<pkg-name> (<major> <minor> <micro>) <constrains> ... )
;; where <constrains> are never rigid."
;;     #f
;; )
