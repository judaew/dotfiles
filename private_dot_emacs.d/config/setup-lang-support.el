;;; Package --- Summary

;;; Commentary:

;; Don't use flycheck-inline with lsp-mode, as it breaks inline diagnostics.

;;; Code:

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom (flycheck-emacs-lisp-load-path 'inherit))

(use-package eldoc
  :custom (eldoc-minor-mode-string nil))

(use-package cmake-mode)

;; TODO: set :bind ("C-c d" . docker)
(use-package docker)
(use-package dockerfile-mode)
(use-package docker-compose-mode)

(use-package go-mode)

(use-package pyvenv
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (message "Searching for venv in: %s" default-directory)
              (let ((venv-dir (locate-dominating-file default-directory ".venv")))
                (if venv-dir
                    (progn
                      (message "Activating venv: %s" venv-dir)
                      (pyvenv-activate (expand-file-name ".venv" venv-dir)))
                  (message "No .venv found"))))))

;;; setup-lang-support.el ends here
