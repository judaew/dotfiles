;;; Package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package lsp-mode
  :hook
  (((c-mode
     c++-mode
     go-mode
     rust-mode
     lua-mode
     python-mode
     cmake-mode
     dhall-mode
     dockerfile-mode) . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration)
   ;; See https://magnus.therning.org/2024-05-04-orderless-completion-in-lsp-mode.html
   (lsp-completion-mode . (lambda ()
				 (setq-local completion-category-defaults
					     (assoc-delete-all 'lsp-capf completion-category-defaults)))))
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-use-plist t)
  (lsp-signature-doc-lines 10)
  (lsp-inlay-hint-enable t)
  (lsp-auto-guess-root t)

  ;; Use corfu and cape as autocomplete
  (lsp-enable-completion-at-point t)
  (lsp-completion-provider :none)

  ;; Kill LSP servers if project closed
  ;; Eg `projectile-kill-buffers' (C-x p k)
  (lsp-keep-workspace-alive nil)
  :commands (lsp lsp-deferred)
  :config
  ;; See https://github.com/minad/corfu/issues/188
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible))

(use-package lsp-ui
  :after lsp-mode)

(use-package lsp-pyright
  :custom (lsp-pyright-langserver-command "pyright")
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package lsp-ltex
  :after lsp-mode
  :hook
  ((org-mode markdown-mode) . (lambda ()
                                (require 'lsp-ltex)
                                (lsp-deferred)))
  :custom (lsp-ltex-version "16.0.0")
  :config
  (defvar my-ltex-languages '("en-US" "ru-RU")
    "List of languages for toggling in lsp-ltex.")

  (defun my-ltex-toggle-language ()
    "Cyclically toggle languages for lsp-ltex."
    (interactive)
    (let* ((current-language lsp-ltex-language)
           (next-language (or (cadr (member current-language my-ltex-languages))
                              (car my-ltex-languages)))
           (workspace (lsp--read-workspace))) ;; get WORKSPACE
      (setq lsp-ltex-language next-language)
      (lsp-workspace-restart workspace)))

  (global-set-key (kbd "C-c o l") 'my-ltex-toggle-language))

;;; setup-lsp.el ends here
