;;; setup-project.el --- Project management -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `project'                  ~ project management core
;; - `consult-project-extra'    ~ enhanced project navigation
;; - `projection'               ~ project management library
;; - `projection-multi'         ~ compile-multi extension for projection
;; - `projection-multi-embark'  ~ embark extension for projection
;; - `compile-multi'            ~ multi target interface to compile
;; - `consult-compile-multi'    ~ consult extension for compile-multi
;; - `compile-multi-nerd-icons' ~ nerd-icons extension for compile-multi
;; - `compile-multi-embark'     ~ embark extension for compile-multi

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

(use-package projection
  :hook ((after-init . global-projection-hook-mode)
         (compilation-mode . projection-customize-compilation-mode))
  :custom
  (compilation-buffer-name-function
   #'projection-customize-compilation-buffer-name-function)
  :config
  (with-eval-after-load 'project
    (require 'projection))
  :bind-keymap
  ("C-x P" . projection-map))

(use-package projection-multi
  :bind ( :map project-prefix-map
          ("RET" . projection-multi-compile)))

(use-package projection-multi-embark
  :after embark
  :after projection-multi
  :demand t
  :config (projection-multi-embark-setup-command-map))

(use-package compile-multi
  :bind ([remap compile-multi] . projection-multi-compile)
  :config
  (push `(emacs-lisp-mode
          ("emacs:checkdoc" . ,#'checkdoc))
        compile-multi-config))

(use-package consult-compile-multi
  :after compile-multi
  :demand t
  :config (consult-compile-multi-mode))

(use-package compile-multi-nerd-icons
  :after nerd-icons-completion
  :after compile-multi
  :demand t)

(use-package compile-multi-embark
  :after embark
  :after compile-multi
  :demand t
  :config (compile-multi-embark-mode +1))

(provide 'setup-project)
;;; setup-project.el ends here
