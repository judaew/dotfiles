;;; init-spell.el --- Spelling and languages -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; - `gt.el'           ; translator

;; === Spell checking ===
;; - `jinx'            ; on-the-fly spell checking

;;; Code:

(use-package gt
  :bind
  (("C-c t" . gt-translate)
   ("C-c T" . gt-setup))
  :config
  (setq gt-default-translator
        (gt-translator
         :taker (gt-taker :prompt t :langs '(en uk))
         :engines (gt-google-engine)
         :render (gt-kill-ring-render :then (gt-render)))))

;; === Spell checking ===
;; ----------------------

(use-package jinx
  :ensure nil ;; nix
  :hook
  ((text-mode . jinx-mode)
   (prog-mode . jinx-mode)
   (conf-mode . jinx-mode))
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages))
  :custom
  (jinx-languages "uk en"))

(provide 'init-spell)
;;; init-spell.el ends here
