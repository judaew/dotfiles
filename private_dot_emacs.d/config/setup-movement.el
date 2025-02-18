;;; Package --- Summary

;;; Commentary:

;;; Code:

(use-package treemacs
  :bind
  (("M-0" . treemacs-select-window)
   ("C-x t 1" . treemacs-no-delete-other-windows)
   ("C-x t t" . treemacs)
   ("C-x t d" . treemacs-select-directory)
   ("C-x t B" . treemacs-bookmark)
   ("C-x t C-t" . treemacs-find-file)
   ("C-x t M-t" . treemacs-find-tag))
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :bind ("C-x t p" . treemacs-projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package dired
  :straight nil
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alh"))

(use-package undo-fu
  :bind
  (("C-/" . undo-fu-only-undo)
   ("C-?" . undo-fu-only-redo))
  :custom
  (undo-limit (* 32 1024 1024))
  (undo-strong-limit (* 64 1024 1024))
  (undo-outer-limit (* 128 1024 1024)))

(use-package undo-fu-session
  :after undo-fu
  :hook (after-init . undo-fu-session-global-mode)
  :init
  (setq undo-fu-session-directory (expand-file-name "undo-fu-session/" user-emacs-directory))
  (unless (file-directory-p undo-fu-session-directory)
    (make-directory undo-fu-session-directory))

  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :bind ("C-x u" . vundo))

(use-package winum
  :hook (after-init . winum-mode))

;;; setup-movement.el ends here
