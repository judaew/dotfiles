;;; init-spell.el --- Spelling and languages -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; - `google-translate' ; interface to Google Translate

;; === Spell checking ===
;; - `jinx'            ; on-the-fly spell checking

;;; Code:

(use-package popup)

(use-package google-translate
  :bind
  (("C-c t" . google-translate-at-point)
   ("C-c T" . google-translate-query-translate))
  :custom
  (google-translate-default-target-language "uk")
  (google-translate-default-source-language "en")
  (google-translate-output-destination 'popup))

;; === Spell checking ===
;; ----------------------

(use-package jinx
  :hook
  ((text-mode . jinx-mode)
   ;;(prog-mode . jinx-mode)
   ;;(conf-mode . jinx-mode)
   )
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :custom
  (jinx-languages "en_US-large"))

(provide 'init-spell)
;;; init-spell.el ends here
