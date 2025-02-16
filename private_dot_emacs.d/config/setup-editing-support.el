;;; package --- Summary

;;; Commentary:

;;; Code:

(use-package char-fold
  :custom
  (char-fold-symmetric t)
  (search-default-mode #'char-fold-to-regexp))

(use-package reverse-im
  :demand t
  :after char-fold
  :bind ("M-T" . reverse-im-translate-word)
  :hook (after-init . reverse-im-mode)
  :custom
  ;; cache generated keymaps
  (reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
  ;; use lax matching
  (reverse-im-char-fold t)
  (reverse-im-read-char-advice-function #'reverse-im-read-char-include)
  ;; translate these methods
  (reverse-im-input-methods '("ukrainian-computer" "russian-computer")))

(use-package avy
  :bind
  (("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)
   ("M-g c" . avy-goto-char)
   ("M-g t" . avy-goto-char-timer)))

(use-package iedit
  :demand t)

(use-package saveplace
  :hook (after-init . save-place-mode))

(use-package editorconfig
  :hook (after-init . editorconfig-mode))

;;; setup-editing-support.el ends here.
