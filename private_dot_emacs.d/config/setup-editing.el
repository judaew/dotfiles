;;; setup-editing.el --- Editing configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `smart-hungry-delete' ~ smart hungry delete (better delete soft tab space and etc)
;; - `ws-butler'           ~ trim whitespace on save
;; - `iedit'               ~ edit multiple regions simultaneously
;; - `saveplace'           ~ remember cursor position in files
;; - `editorconfig'        ~ use .editorconfig settings in Emacs
;; - `expreg'              ~ next-gen expand-region using Tree-Sitter

;;; Code:

(use-package smart-hungry-delete
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
               ([remap delete-backward-char] . smart-hungry-delete-backward-char)
               ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode)
  :config
  (setopt ws-butler-keep-whitespace-before-point t))

(use-package iedit)

(use-package saveplace
  :straight nil
  :hook (after-init . save-place-mode))

(use-package editorconfig
  :straight nil
  :hook (after-init . editorconfig-mode))

(use-package expreg
  :bind ("C-=" . expreg-expand))

(provide 'setup-editing)
;;; setup-editing.el ends here.
