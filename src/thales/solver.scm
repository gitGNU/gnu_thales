;;; Thales solver --- implementation of core idea

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

(define* (perform-solve installed availible constrains
			#:key (conservative #f))
    "Resolve CONSTRAINS with AVAILIBLE packages.

If CONSERVATIVE if #t, prefer use of INSTALLED packages, otherwise prefer
new versions.

CONSTRAINS is list of constrains, that have following kinds:

    * Ridid request for specified package. To be used, if bug happens
      and dependency have to be resolved manually.
        (<pkg-name> = (<major> <minor> <micro>))
    * Request for package version, no less that specified.
        (<pkg-name> ? (<major> <minor>) ... )

Both INSTALLED and AVAILIBLE are lists of package in form
    (<pkg-name> (<major> <minor> <micro>))"
#f
)
