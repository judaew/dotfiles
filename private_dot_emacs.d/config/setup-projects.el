;;; Package --- Summary

;;; Commentary:

;;; Code:

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :hook (after-init . projectile-mode)
  :custom
  (projectile-project-search-path '("~/Workspaces/"))
  (projectile-completion-system 'ivy))

;;; setup-projects.el ends here
