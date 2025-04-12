;;; package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Early Init
(setenv "LSP_USE_PLISTS" "true")

;;; General config

;; Set window size
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(width . 88))
  (add-to-list 'default-frame-alist '(height . 36)))

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
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)

;; Set font
(if (display-graphic-p)
    (set-frame-font "VictorMono Nerd Font Mono-10" nil t)
  (unless (find-font (font-spec :name "VictorMono Nerd Font"))
    (message "VictorMono Nerd Font not found!")))

;; Show current project on the default mode-line
(setq project-mode-line t)

;; Enable line numbers
(global-display-line-numbers-mode t)

;; Enable smart parens
(electric-pair-mode t)

;; The output of Grep is split into sections
;; It is equivalent to the '--heading' option of some tools such as 'git grep' and 'rg'.
(setq grep-use-headings t)

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

(load "setup-org")             ;; Org-mode enhancement
(load "setup-term")            ;; Terminal integration
(load "setup-search")          ;; Search enhancement
(load "setup-completion")      ;; Auto-completion
(load "setup-snippets")        ;; Snippet management
(load "setup-git")             ;; Git integration
(load "setup-projects")        ;; Project management
;; (load "setup-tree-sitter")     ;; Tree-sitter support
(load "setup-lang-support")    ;; Misc language support
(load "setup-lsp")             ;; LSP (Language Server Protocol)
;; (load "setup-dap")             ;; DAP (Debug Adapter Protocol)
(load "setup-llm")             ;; Large language model
(load "setup-editing-support") ;; Editing utilities
(load "setup-movement")        ;; Different navigation enhancement
(load "setup-ui")              ;; UI tweaks
(load "setup-themes")          ;; Theme configuration

;; Set custom-file location
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'noerror))

;;; init.el ends here
