;;; Package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; require emacs 27.1
(use-package project
  :straight nil
  :config
  (setq project-switch-use-consult t)

  (setq project-switch-commands
	'((project-find-file "Find file" "f")
	  (project-find-regexp "Find regexp" "r")
	  (project-dired "Dired" "d")
	  (magit-project-status "Magit" "m"))))

(use-package consult-project-extra
  :disabled
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p o" . consult-project-extra-find-other-window)))

;;; setup-projects.el ends here
