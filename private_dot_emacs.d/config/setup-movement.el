;;; Package --- Summary

;;; Commentary:

;;; Code:

(use-package neotree
  :bind
  (("C-x t" . neotree-toggle)
   ("M-0" . my/neotree-select-window))
  :custom
  (neo-smart-open t)
  (neo-window-width 25)
  (neo-show-hidden-files t)
  (neo-auto-indent-point t)
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  :config
  (defun my/neotree-select-window ()
    "Switch to NeoTree window if it is visible."
    (interactive)
    (if (neo-global--window-exists-p)
	(if (eq major-mode 'neotree-mode)
	    (other-window 1)
	  (select-window (neo-global--get-window)))
      (neo-global--open)))

  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'my/neotree-select-window)))

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
  :hook (after-init . winum-mode)
  :config
  (define-key winum-keymap (kbd "C-`") 'winum-select-window-by-number)
  (define-key winum-keymap (kbd "C-²") 'winum-select-window-by-number)
  ;; (define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)
  (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
  (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
  (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
  (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
  (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
  (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
  (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
  (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
  (define-key winum-keymap (kbd "M-9") 'winum-select-window-9))

;;; setup-movement.el ends here
