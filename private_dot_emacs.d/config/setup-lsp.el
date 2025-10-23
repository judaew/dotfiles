;;; setup-lsp.el --- LSP support -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - eglot     ~ LSP client
;; - eldoc-box ~ display documentation in popup
;;
;; TODOs:
;; - emacs-lsp-booster (https://github.com/blahgeek/emacs-lsp-booster)
;; - eglot-booster (https://github.com/jdtsmith/eglot-booster)

;;; Code:

(use-package eglot
  :after cape which-key marginalia
  :hook
  (((c-mode
    c++-mode
    go-mode
    rust-mode
    java-mode
    lua-mode
    python-mode
    cmake-mode
    dockerfile-mode) . eglot-ensure)
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
  ;; (eglot-report-progress nil)
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

;; Floating documentation (like lsp-ui-doc)
(use-package eldoc-box
  :after eglot
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :custom
  (eldoc-box-clear-with-C-g t)
  (eldoc-box-only-multi-line t)
  (eldoc-box-fringe-use-same-bg t))

(provide 'setup-lsp)
;;; setup-lsp.el ends here
