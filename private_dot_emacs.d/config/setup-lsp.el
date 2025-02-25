;;; Package --- Summary

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
     sh-mode
     cmake-mode) . lsp-deferred)
   (lsp-mode . lsp-enable-which-key-integration))
  :bind (("C-c l" . lsp))
  :custom
  (lsp-keymap-prefix "C-l")
  (lsp-auto-guess-root t)
  (lsp-use-plist t)
  (lsp-signature-doc-lines 10)
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :after lsp-mode)

(use-package lsp-ivy
  :after lsp-mode)

(use-package lsp-pyright
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
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
