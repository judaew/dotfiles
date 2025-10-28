;;; setup-project.el --- Project management -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `project'               ~ project management core
;; - `consult-project-extra' ~ enhanced project navigation
;; - `ottp'                  ~ organize projects in tab groups

;;; Code:

(use-package project
  :config
  (add-hook 'project-find-functions #'project-try-vc)
  (setq project-vc-extra-root-markers
	'(".project"         ;; project.el (default)
	  "Cargo.toml"       ;; Rust
	  "package.json"     ;; Node.js
	  "pyproject.toml"   ;; Python
	  "requirements.txt"
	  "go.mod"           ;; Go
	  "composer.json"    ;; PHP
	  "Makefile"
	  "CMakeLists.txt"))

  (setq project-switch-commands
	'((project-find-file "Find file" "f")
	  (project-find-regexp "Find regexp" "r")
	  (project-dired "Dired" "d")
	  (magit-project-status "Magit" "m")
	  (project-shell "Shell" "s"))))

(use-package consult-project-extra
  :bind
  (("C-x p f" . consult-project-extra-find)
   ("C-x p o" . consult-project-extra-find-other-window)))

(use-package otpp ; one-tab-per-project
  :after project
  :config
  (otpp-mode 1)
  (otpp-override-mode 1))

(provide 'setup-project)
;;; setup-project.el ends here
