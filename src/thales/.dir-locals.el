;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((scheme-mode
     (eval font-lock-add-keywords nil
	 `(("(\\(sealed\\|throw\\|catch\\)\s'?\\(\\w+\\)"
	       (1 font-lock-keyword-face)
	       (2 font-lock-constant-face))
	   ("\\<lambda-match\\>" . font-lock-keyword-face)
           ("(\\(define-match\\)\s+(\\(\\w+\\)"
	       (1 font-lock-keyword-face)
	       (2 font-lock-function-name-face))
           ("\\<for\\*?\\>"   . font-lock-keyword-face)
	   ("\\<lambda\\*\\>" . font-lock-keyword-face)
	   ("error:.*?\\_>"   . 'bold)
)
	 :lowest-priority)))
