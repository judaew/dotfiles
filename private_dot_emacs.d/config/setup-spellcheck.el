;;; setup-spellcheck.el --- Spell checking -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `jinx'    ~ on-the-fly spell checking

;;; Code:

(use-package jinx
  :bind (("M-$" . jinx-correct)
	 ("C-M-$" . jinx-languages))
  :custom
  (jinx-languages "en_US-large uk_UA"))

(provide 'setup-spellcheck)
;;; setup-spellcheck.el ends here
