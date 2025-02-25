;;; Package --- Summary

;;; Commentary:

;;; Code:

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments (list "-l"))
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(use-package vterm
  :bind ("C-c t" . vterm)
  :custom
  (vterm-max-scrollback 10000))

(use-package direnv
  :hook (after-init . direnv-mode))

(use-package mouse
  :straight nil
  :unless (display-graphic-p)
  :hook (after-init . xterm-mouse-mode))

;;; setup-term.el ends here
