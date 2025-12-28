;;; package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; General config

;; Set fonts for fixed-pitch and variable-pitch
(let ((font-family-fixed "Iosevka Curly")
      (font-family-pitch "Iosevka Aile")
      (font-size (if (eq system-type 'darwin) 130 110)))
  (when (member font-family-fixed (font-family-list))
    (set-face-attribute 'default nil
                        :font font-family-fixed :height font-size)
    (set-face-attribute 'fixed-pitch nil
                        :font font-family-fixed))
  (when (member font-family-pitch (font-family-list))
    (set-face-attribute 'variable-pitch nil
                        :font font-family-pitch :height font-size)))

;; Disable creating backup and lock files
(setopt make-backup-files nil)
(setopt create-lockfiles nil)

;; Auto-save
(setopt auto-save-default t)

(let ((autosaves-dir "~/.emacs.d/autosaves/"))
  (unless (file-directory-p autosaves-dir)
    (make-directory autosaves-dir t)))

(setopt auto-save-file-name-transforms
        `((".*" "~/.emacs.d/autosaves/" t)))

;; Shortened yes-or-no-p to y-or-n-p
(setopt use-short-answers t)

;; Show current project on the default mode-line
(setopt project-mode-line t)

;; Enable line numbers
(global-display-line-numbers-mode t)

;; Add line and column to modeline
(line-number-mode)
(column-number-mode)

;; Enable smart parens
(electric-pair-mode t)

;; Don't use /anywhere/ tabs
(setq-default indent-tabs-mode nil)

(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; Tab-bar
(setopt tab-bar-show 1) ;; auto-hide
(global-set-key (kbd "C-<next>") 'tab-next)
(global-set-key (kbd "C-<prior>") 'tab-previous)

;; GnuPG pinentry via the Emacs minibuffer
(setopt epg-pinentry-mode 'loopback)
(setopt epa-pinentry-mode 'loopback)

;;; straight.el

;; Bootstrap Straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Packages
(straight-use-package 'use-package)
(setopt straight-use-package-by-default t)
(setopt straight-vc-git-default-clone-depth 3)

;; Re-checks every repo only when is really change something
(setopt straight-cache-autoloads t)

(use-package server
  :straight nil
  :config
  (unless (server-running-p)
    (server-start)))

;; See https://github.com/radian-software/straight.el/issues/551#issuecomment-667540139
;; Add project and flymake to the pseudo-packages variable so straight.el doesn't download
(setopt straight-built-in-pseudo-packages '(emacs project flymake))

;; Load modular configuration files
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'init-completion)
(require 'init-editing)
(require 'init-projects)
(require 'init-ide)
(require 'init-vc)
(require 'init-org)
(require 'init-ui)
(require 'init-langs)
(require 'init-term)
(require 'init-ai)
(require 'init-dired)

;; Set custom-file location
(setopt custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'noerror))

;; Make Flymake see packages from current load-path
(setopt elisp-flymake-byte-compile-load-path load-path)

;;; init.el ends here
