;;; init-projects.el --- Projects management -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; - `project'                  ; project management core
;; - `consult-project-extra'    ; enhanced project navigation
;; - `midnight'                 ; kill old buffers

;;; Code:

(use-package project
  :ensure nil
  :config
  (setopt project-switch-commands
          '((project-find-file "Find file" ?f)
            (project-find-regexp "Find regexp" ?r)
            (consult-ripgrep "Find via ripgrep" ?R)
            (project-dired "Dired" ?d)
            (magit-project-status "Magit" ?m)
            (ghostel-project "Shell (ghostel)" ?s))))

(use-package midnight
  :ensure nil
  :config (midnight-mode 1)
  :custom (midnight-period (* 24 60 60))) ;; 1 days

;; Compile extensions

(defun my/project-root ()
  "Returns the project root via project.el or the current directory."
  (if-let* ((proj (project-current t)))
      (project-root proj)
    default-directory))

(defun my/project-compile (cmd)
  "Run CMD in the root of the Go project."
  (let ((default-directory (my/project-root)))
    (compile cmd)))

(use-package go-ts-mode
  :ensure nil
  :after transient
  :bind (:map go-ts-mode-map ("C-x c" . my/go-ts-mode-compile-transient))
  :config
  (defun my/go-ts-mode-build ()
    "Golang: Build all packages in the module. Runs `go build ./...'."
    (interactive) (my/project-compile "go build ./..."))
  (defun my/go-ts-mode-run ()
    "Golang: Compile and run the main package Runs `go run .'."
    (interactive) (my/project-compile "go run ."))
  (defun my/go-ts-mode-test ()
    "Golang: Run tests for all packages. Runs `go test ./...'."
    (interactive) (my/project-compile "go test  ./..."))
  (defun my/go-ts-mode-test-verbose ()
    "Golang: Run tests verbosely without cache. Runs `go test -v -count=1 ./...'."
    (interactive) (my/project-compile "go test -v -count=1  ./..."))
  (defun my/go-ts-mode-vet ()
    "Golang: Check code for suspicious constructs. Runs `go vet ./...'."
    (interactive) (my/project-compile "go vet  ./..."))
  (defun my/go-ts-mode-mod-tidy ()
    "Golang: Sync go.mod and go.sum dependencies. Runs `go mod tidy'."
    (interactive) (my/project-compile "go mod tidy"))
  (defun my/go-ts-mode-clean-cache ()
    "Golang: Clear the build cache. Runs `go clean -cache'."
    (interactive) (my/project-compile "go clean -cache"))
  (defun my/go-ts-mode-generate ()
    "Golang: Run code generation directives. Runs `go generate ./...'."
    (interactive) (my/project-compile "go generate ./..."))

  (transient-define-prefix my/go-ts-mode-compile-transient ()
    "Golang: The compile transient for Go development."
    [["Build & Run"
      ("b" "build" my/go-ts-mode-build)
      ("r" "run" my/go-ts-mode-run)]
     ["Testing"
      ("t" "test" my/go-ts-mode-test)
      ("T" "test -v" my/go-ts-mode-test-verbose)]
     ["Code Quality"
      ("v" "vet" my/go-ts-mode-vet)
      ("m" "mod tidy" my/go-ts-mode-mod-tidy)]
     ["Utilities"
      ;; TODO: -race and -cover
      ("c" "clean cache" my/go-ts-mode-clean-cache)
      ("g" "generate" my/go-ts-mode-generate)]]))

(provide 'init-projects)
;;; init-projects.el ends here
