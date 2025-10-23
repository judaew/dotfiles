;;; setup-editing.el --- Editing configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `ws-butler'    ~ trim whitespace on save
;; - `iedit'        ~ edit multiple regions simultaneously
;; - `saveplace'    ~ remember cursor position in files
;; - `editorconfig' ~ use .editorconfig settings in Emacs


;;; Code:

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode)
  :config
  (setq ws-butler-keep-whitespace-before-point t))

(use-package iedit)

(use-package saveplace
  :straight nil
  :hook (after-init . save-place-mode))

(use-package editorconfig
  :straight nil
  :hook (after-init . editorconfig-mode))

(provide 'setup-editing)
;;; setup-editing.el ends here.
