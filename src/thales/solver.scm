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
    :export (perform-solve generate-r1-contrain-solver))
(use-modules (ice-9 match))
(use-modules (srfi srfi-1))
(use-modules (thales seal))
(use-modules (srfi srfi-26))
(use-modules (thales syntax))

(sealed version-compatible?
	([& '(1 2) '(1 2 0)] => #t)
	([& '(1 2 3) '(1 2 2)] => #t))

(define (version-compatible? v1 v2)
    "Version is compatible, if major versions is equal
and minor is no less."
    (and (=  (car v1)  (car v2))
	 (<= (cadr v1) (cadr v2))))
(define-match (version-interchangeble? [major1 minor1 _ ...]
		                       [major2 minor2 _ ...])
    (and (= major1 major2)
	(= minor1 minor2)))

(sealed satisfy
	[(& '(foo ? (1 0))
	    '(foo (1 0 0)))
	 => #t]
	[(& '(foo ? (2 0))
	    '(foo (1 2 3)))
	 => #f]
	[(& '(foo ? (1 0))
	    '(foo (1 2 3)))
	 => #t]
	[(& '(foo ? (1 0) (2 0))
	    '(foo   (1 2 3)))
	 => #t]
	[(& '(foo = (1 2 3))
	    '(foo   (1 2 3)))
	 => #t]
	[(& '(foo = (1 2 4) (1 2 7))
            '(foo   (1 2 5)))
	 => #f])

(define version-equal? equal?)

(define-match (satisfy [?pkgname *cmp* ?versions ...]
                       [pkgname  pkg-version _ ...])
    (let ((cmp (case *cmp*
		   [(?) version-compatible?]
		   [(=) version-equal? ])))
	(and (eq? pkgname ?pkgname)
	    (any #[cmp <> pkg-version] ?versions))))

(define* (generate-r1-contrain-solver installed availible
				      #:key (conservative #f))
    "Return function, that will packages, that satisfy constrain.

Function, given constrain, return list of packages, that satistfy it,
taking only one version from each major version. Order of packages influenced
by list of INSTALLED packages, if CONSERVATIVE is #f.
"
    (let* ((cache (make-hash-table))
	   (cache-get #[hash-ref  cache <>])
	   (cache-put #[hash-set! cache <> <>]))
	(lambda (constr)
	    (or (cache-get constr)
		(let ((result (filter #[satisfy constr <>] availible)))
		    (cache-put constr result)
		    result)))))

(define* (perform-solve installed availible constrains
			#:key (conservative #f))
    "Resolve CONSTRAINS with AVAILIBLE packages.

If CONSERVATIVE if #t, prefer use of INSTALLED packages, otherwise prefer
new versions.

CONSTRAINS is list of constrains, that have following kinds:

    * Ridid request for specified package. To be used, if bug happens
      and dependency have to be resolved manually.
        (<pkg-name> = (<major> <minor> <micro>) ...)
    * Request for package version, no less that specified.
        (<pkg-name> ? (<major> <minor>) ... )

Both INSTALLED and AVAILIBLE are lists of package in form
    (<pkg-name> (<major> <minor> <micro>) <constrains>)
where <constrains> are never rigid."

#f
)
