;;; Package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package projectile
  :diminish projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :hook (after-init . projectile-mode)
  :custom
  (projectile-switch-project-action #'projectile-dired)
  (projectile-project-search-path '(
                                    "~/Workspaces/local"
                                    "~/Workspaces/github.com/judaew"))
  (projectile-completion-system 'ivy))

;;; setup-projects.el ends here
