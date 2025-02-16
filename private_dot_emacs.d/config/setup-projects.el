;;; Package --- Summary

;;; Commentary:

;;; Code:

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-project-search-path '("~/Workspaces/"))
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode +1))

;;; setup-projects.el ends here
