;;; package --- Summary

;;; Commentary:

;;; Code:

;;; Early Init
(setenv "LSP_USE_PLISTS" "true")

;;; General config

;; Adjust gc-cons-threshold and increase the amount of data which
;; Emacs reads from the process
(setq gc-cons-threshold (* 100 1024 1024)    ;; 100mb
      read-process-output-max (* 1024 1024)) ;; 1mb

;; Disable creating backup and lock files
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Disabel auto-save
(setq auto-save-default nil)

;; Create empty buffer *scratch* in org-mode
(setq inhibit-startup-screen t)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "#+TITLE: Scratch Buffer\n\n")

;; Minimal UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

;; Set font
(set-frame-font "VictorMono Nerd Font Mono-10" nil t)
(unless (find-font (font-spec :name "VictorMono Nerd Font"))
  (message "VictorMono Nerd Font not found!"))

;; Enable line numbers
(global-display-line-numbers-mode t)

;; Enable smart parens
(electric-pair-mode t)

(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;;; straight.el

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

;;; Packages

(setq package-enable-at-startup nil)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Load modular configuration files
(add-to-list 'load-path "~/.emacs.d/config")

(load "setup-org.el")             ;; Org-mode enhancement
(load "setup-term.el")            ;; Terminal integration
(load "setup-search.el")          ;; Search enhancement
(load "setup-completion.el")      ;; Auto-completion
(load "setup-snippets.el")        ;; Snippet management
(load "setup-git.el")             ;; Git integration
(load "setup-projects.el")        ;; Project management
(load "setup-lang-support.el")    ;; Misc language support
(load "setup-lsp.el")             ;; LSP (Language Server Protocol)
(load "setup-dap.el")             ;; DAP (Debug Adapter Protocol)
(load "setup-editing-support.el") ;; Editing utilities
(load "setup-movement.el")        ;; Different navigation enhancement
(load "setup-ui.el")              ;; UI tweaks
(load "setup-themes.el")          ;; Theme configuration

;; Set custom-file location
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'noerror))

;;; init.el ends here
