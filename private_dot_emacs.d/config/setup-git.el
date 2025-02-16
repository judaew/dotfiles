;;; Package --- Summary

;;; Commentary:

;;; Code:

(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)))

(use-package forge
  :after magit
  :custom
  (forge-add-default-bindings t))

(use-package diff-hl
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (after-init . global-diff-hl-mode))
  :config
  ;; highlighting changes on the fly
  (diff-hl-flydiff-mode))

;;; setup-git.el ends here
