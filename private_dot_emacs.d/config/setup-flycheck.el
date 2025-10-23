;;; setup-flycheck.el --- Syntax checking -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `flycheck'          ~ on-the-fly syntax checking

;; Don't use flycheck-inline with lsp-mode, as it breaks inline diagnostics.

;;; Code:

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-idle-change-delay 0.5)) ;; delay after input, 0.5s it's default

(provide 'setup-flycheck)
;;; setup-flycheck.el ends here
