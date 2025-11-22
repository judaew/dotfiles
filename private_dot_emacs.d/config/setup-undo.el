;;; setup-undo.el --- Undo & redo support -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `undo-fu'         ~ functional undo/redo commands
;; - `undo-fu-session' ~ persistent undo/redo sessions
;; - `vundo'           ~ visual undo tree
;; - `winner'          ~ window layout undo/redo

;;; Code:

(use-package undo-fu
  :bind
  (("C-/" . undo-fu-only-undo)
   ("C-?" . undo-fu-only-redo))
  :custom
  (undo-limit (* 32 1024 1024))
  (undo-strong-limit (* 64 1024 1024))
  (undo-outer-limit (* 128 1024 1024)))

(use-package undo-fu-session
  :hook (after-init . undo-fu-session-global-mode)
  :init
  (setopt undo-fu-session-directory (expand-file-name "undo-fu-session/" user-emacs-directory))
  (unless (file-directory-p undo-fu-session-directory)
    (make-directory undo-fu-session-directory))

  (setopt undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :bind ("C-x u" . vundo))

(use-package winner
  :straight nil
  :hook (after-init . winner-mode))

(provide 'setup-undo)
;;; setup-undo.el ends here
