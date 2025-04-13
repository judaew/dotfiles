;;; Package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package dired-x
  :straight nil)

;; Default binds:
;; - "C-x C-j" . bind-jump
;;
;; Hide dotfiles: https://github.com/mattiasb/dired-hide-dotfiles/issues/8
(use-package dired
  :straight nil
  :hook
  ((dired-mode . dired-omit-mode)
   (dired-mode . dired-hide-details-mode))
  :bind (:map dired-mode-map
	       ("." . dired-omit-mode))
  :custom
  (dired-omit-files (rx (seq bol ".")))
  (dired-listing-switches "-agho --group-directories-first"))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(defconst my/audio-files-extensions
  '("mp3" "wav" "flac" "ogg" "aac" "wma" "opus" "alac" "dts" "wv" "caf" "mka" "mid")
  "Dired Audio files extensions.")

(defconst my/video-files-extensions
  '("mp4" "avi" "mov" "mkv" "wmv" "webm" "mpeg" "mpg" "m4v" "3gp" "ogv"
    "ts" "tp" "vob" "hevc" "m2ts" "m2v")
  "Dired Video files extensions.")

(defconst my/image-files-extensions
  '("jpg" "jpeg" "png" "gif" "bmp" "tiff" "webp" "svg" "heif" "ico"
    "raw" "cp2" "nef" "orf" "arw" "dng" "psd" "ai" "eps" "pdf"))

(use-package dired-rainbow
  :config
  (dired-rainbow-define audio "#FD971F" my/audio-files-extensions)
  (dired-rainbow-define video "#FD971F" my/video-files-extensions)
  (dired-rainbow-define image "#E6DB74" my/image-files-extensions))

(use-package dired-open
  :config
  (setq dired-open-extensions
	(append
	 (mapcar (lambda (ext) (cons ext "gwenview"))
		 my/image-files-extensions)
	 (mapcar (lambda (ext) (cons ext "vlc"))
                 my/video-files-extensions)
         (mapcar (lambda (ext) (cons ext "vlc"))
                 my/audio-files-extensions))))

;; (use-package dired-hacks-utils)

(use-package dired-preview
  :defer t ; don't access `dired-mode-map' until `dired-preview' is loaded
  :bind (:map dired-mode-map
	      ("P" . dired-preview-mode)))

;; (use-package dired-subtree
;;   :bind (:map dired-mode-map
;;               ("i" . dired-subtree-insert)
;;               (";" . dired-subtree-remove)
;;               ("<tab>" . dired-subtree-toggle)
;;               ("<backtab>" . dired-subtree-cycle)))

(use-package dired-collapse :ensure t)

(use-package dired-filter :ensure t)

;; (use-package dired-narrow
;;   :bind (:map dired-mode-map
;;               ("C-S-f" . dired-narrow)))

(use-package dired-ranger)

;; (use-package dired-quick-sort
;;   :config (dired-quick-sort-setup))

;; (setq dired-listing-switches "-lXGh --group-directories-first"
;;       dired-recursive-copies 'always
;;       dired-recursive-deletes 'top
;;       global-auto-revert-non-file-buffers t
;;       auto-revert-verbose nil
;;       dired-dwim-target t
;;       wdired-allow-to-change-permissions t)

;; (use-package dired-filetype-face
;;   :config (require 'dired-filetype-face))


;; (use-package dirvish
;;   :disabled)

;; Unset F2 from 2C-mode for neotree-toggle
(global-unset-key (kbd "<f2>"))

(use-package neotree
  :bind
  (("<f2>" . neotree-toggle)
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

(use-package winner
  :straight nil)

;;; setup-movement.el ends here
