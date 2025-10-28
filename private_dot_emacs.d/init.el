;;; package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; General config

;; Suppress native-comp warnings
(setq native-comp-async-report-warnings-errors nil)

;; Disable creating backup and lock files
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Disabel auto-save
(setq auto-save-default nil)

;; Shortened yes-or-no-p to y-or-n-p
(setopt use-short-answers t)

;; Show current project on the default mode-line
(setq project-mode-line t)

;; Enable line numbers
(global-display-line-numbers-mode t)

;; Add line and column to modeline
(line-number-mode)
(column-number-mode)

;; Enable smart parens
(electric-pair-mode t)

;; The output of Grep is split into sections
;; It is equivalent to the '--heading' option of some tools such as 'git grep' and 'rg'.
(setq grep-use-headings t)

(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; Tab-bar
(setq tab-bar-show 1) ;; auto-hide
(global-set-key (kbd "C-<next>") 'tab-next)
(global-set-key (kbd "C-<prior>") 'tab-previous)

;;; straight.el

;; Bootstrap Straight
(defvar bootstrap-version)
(setq straight-repository-branch "develop")

(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "https://raw.githubusercontent.com/"
                 "radian-software/straight.el/"
                 "develop/install.el")
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Packages
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 3)

;; Re-checks every repo only when is really change something
(setq straight-cache-autoloads t
      straight-check-for-modifications '(find-watching))

(use-package server
  :straight nil
  :config
  (unless (server-running-p)
    (server-start)))

;; Load modular configuration files
(add-to-list 'load-path "~/.emacs.d/config")

;; -----------------------------------
;; Core system & foundational behavior
;; -----------------------------------
(load "setup-editing")  ;; Editing utilities
(load "setup-movement") ;; Different navigation enhancement
(load "setup-dired")    ;; File manager improvements
(load "setup-undo")     ;; Undo/redo system and undo navigation

;; -----------------
;; UI tweaks & theme
;; -----------------
(load "setup-ui")     ;; UI tweaks
(load "setup-themes") ;; Theme configuration

;; ------------------------
;; Completion & UI feedback
;; ------------------------
(load "setup-completion-backend") ;; cape & other capf completion providers
(load "setup-completion-ui")      ;; corfu as completion UI
(load "setup-minibuffer-ui")      ;; vertico/consult and other minibuffer tweaks

;; ---------------------------
;; Spelling or syntax checking
;; ---------------------------
(load "setup-flymake")    ;; syntax/linting via Flymake integration
(load "setup-spellcheck") ;; spell-checker setup

;; ----------------------------------
;; Language support, TreeSitter & LSP
;; ----------------------------------
(load "setup-project")    ;; project management
(load "setup-treesitter") ;; treesitter integration
(load "setup-lang")       ;; misc language support
(load "setup-lsp")        ;; LSP (Language Server Protocol)

;; -----------------
;; Tools & ecosystem
;; -----------------
(load "setup-git")    ;; git integration
(load "setup-docker") ;; docker integration
(load "setup-term")   ;; terminal integration
(load "setup-llm")    ;; large language model integration

;; --------------------
;; Knowledge management
;; --------------------
(load "setup-org") ;; org-mode enhancement

;; Set custom-file location
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'noerror))

;; Make Flymake see packages from current load-path
(setopt elisp-flymake-byte-compile-load-path load-path)

;;; init.el ends here
