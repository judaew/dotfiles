;;; Package --- Summary

;;; Commentary:

;; Don't use flycheck-inline with lsp-mode, as it breaks inline diagnostics.

;;; Code:

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom (flycheck-emacs-lisp-load-path 'inherit))

(use-package eldoc
  :custom (eldoc-minor-mode-string nil))

(use-package cmake-mode
  :defer t)

;; TODO: set :bind ("C-c d" . docker)
(use-package docker
  :defer t)
(use-package dockerfile-mode
  :defer t)
(use-package docker-compose-mode
  :defer t)

(use-package go-mode
  :defer t)

(use-package pyvenv
  :defer t
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
