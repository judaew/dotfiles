;;; Package --- Summary

;;; Commentary:

;;; Code:

(use-package vterm
  :bind ("C-c t" . vterm)
  :custom
  (vterm-max-scrollback 10000))

(use-package direnv
  :hook (after-init . direnv-mode))

;;; setup-term.el ends here
