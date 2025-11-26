;;; setup-project.el --- Project management -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `project'               ~ project management core
;; - `consult-project-extra' ~ enhanced project navigation

;;; Code:

(use-package project
  :straight (:type built-in)
  :config
  (add-hook 'project-find-functions #'project-try-vc)
  (setopt project-vc-extra-root-markers
        '(".project"         ;; project.el (default)
          "Cargo.toml"       ;; Rust
          "package.json"     ;; Node.js
          "pyproject.toml"   ;; Python
          "requirements.txt"
          "go.mod"           ;; Go
          "composer.json"    ;; PHP
          "Makefile"
          "CMakeLists.txt"))

  (setopt project-switch-commands
        '((project-find-file "Find file" ?f)
          (project-find-regexp "Find regexp" ?r)
          (project-dired "Dired" ?d)
          (magit-project-status "Magit" ?m)
          (project-shell "Shell" ?s))))

(use-package consult-project-extra
  :bind
  (("C-x p f" . consult-project-extra-find)
   ("C-x p o" . consult-project-extra-find-other-window)))

(provide 'setup-project)
;;; setup-project.el ends here
