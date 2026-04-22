;;; init-ide.el --- IDE func -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; === LSP ===
;; - `eglot'           ; LSP client
;; - `consult-eglot'   ; Jump to workspace symbols with eglot and consult

;; === Tree-sitter ===
;; syntax trees config

;; === DAP ===
;; - `dape'            ; Debug Adapter Protocol
;; - `projection-dape' ; Projection integration for dape

;; === Syntax checking ===
;; - `flymake'         ; on-the-fly syntax checking

;; TODO: restclient.el

;;; Code:

;; === LSP ===
;; -----------

(defun my/eglot-ensure ()
  "Start eglot for the current buffer only if it is inside a project.
Or passes other checks that determine whether eglot should run."
  (when (and buffer-file-name
             (project-current)
             ;; This is useful for me as a MacPorts maintainer
             (not buffer-read-only))
    (eglot-ensure)))

(use-package eglot
  :after cape which-key marginalia
  :hook
  (((bash-ts-mode
     c-ts-mode
     c++-ts-mode
     go-ts-mode
     rust-ts-mode
     java-ts-mode
     lua-ts-mode
     python-ts-mode
     cmake-ts-mode
     dockerfile-ts-mode) . my/eglot-ensure)
   (eglot-managed-mode . eglot-inlay-hints-mode)
   ;; Integrate with `cape'
   (eglot-managed-mode . my/setup-capf-eglot))
  :bind
  (:map eglot-mode-map
        ("C-c l r" . eglot-rename)
        ("C-c l f" . eglot-format)
        ("C-c l F" . eglot-format-buffer)
        ("C-c l a" . eglot-code-actions)
        ("C-c l o" . eglot-code-action-organize-imports)
        ("C-c l d" . eglot-find-declaration)
        ("C-c l i" . eglot-find-implementation)
        ("C-c l t" . eglot-find-typeDefinition)
        ("C-c l C" . eglot-show-call-hierarchy)
        ("C-c l T" . eglot-show-type-hierarchy)
        ("C-c l R" . eglot-reconnect)
        ("C-c l S" . eglot-shutdown))
  :custom
  (eglot-autoshutdown t)       ;; kill server when last managed buffer is closed
  (eglot-events-buffer-size 0) ;; disable noisy logs
  (eglot-sync-connect nil)     ;; connect async
  (eglot-report-progress nil) ;; reduce noise in minibuffer
  :config
  ;; Make CAPF non-blocking
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-noninterruptible)

  ;; Integrate with `which-key'
  (which-key-add-key-based-replacements
    "C-c l" "eglot"
    "C-c l r" "rename"
    "C-c l f" "format"
    "C-c l F" "format buffer"
    "C-c l a" "code actions"
    "C-c l o" "organize imports"
    "C-c l d" "goto declaration"
    "C-c l i" "goto implementation"
    "C-c l t" "goto typeDefinition"
    "C-c l C" "show call hierarchy"
    "C-c l T" "show type hierarchy"
    "C-c l R" "reconnect"
    "C-c l S" "shutdown")

  ;; Integrate with `marginalia'
  (add-to-list 'marginalia-command-categories
               '(eglot-code-actions . eglot)))

(use-package consult-eglot
  :bind ("M-g l" . consult-eglot-symbols)
  :config
  (with-eval-after-load 'embark
    (with-eval-after-load 'consult-eglot
      (require 'consult-eglot-embark)
      (consult-eglot-embark-mode))))

;; === Tree-sitter ===
;; -------------------
(setopt treesit-auto-install-grammar 'ask)
(setopt treesit-enabled-modes t)

;; === DAP ===
;; -----------

;; Left and right side windows occupy full frame height
(setopt window-sides-vertical t)

(use-package dape
  :hook
  (kill-emacs . dape-breakpoint-save) ;; Save breakpoints on quit
  (after-init . dape-breakpoint-load) ;; Load breakpoints on startup
  :custom
  (dape-buffer-window-arrangement 'right)

  :config
  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)
  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))
  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook #'kill-buffer))

;; === Syntax checking ===
;; -----------------------

(use-package flymake
  :defer 1
  :straight (:type built-in)
  :hook (prog-mode . flymake-mode)
  :config
  ;; Trust your own config files for Elisp flymake checks
  (setopt elisp-flymake-byte-compile-load-path load-path)
  ;; Ensure elisp flymake knows where to find libraries
  (setopt trusted-content '("~/.emacs.d/early-init.el" "~/.emacs.d/config/")))

(provide 'init-ide)
;;; init-ide.el ends here
