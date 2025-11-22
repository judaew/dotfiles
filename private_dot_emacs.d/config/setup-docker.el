;;; setup-docker.el --- Docker & Podman integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `docker'              ~ Docker integration
;; - `dockerfile-mode'     ~ major mode for editing Dockerfiles
;; - `docker-compose-mode' ~ major mode for editing docker-compose files

;;; Code:

(defcustom my/emacs-docker-executable 'podman
  "The executable to be used with docker-mode."
  :type '(choice
          (const :tag "docker" docker)
          (const :tag "podman" podman))
  :group 'my/emacs)

(use-package docker
  :defer t
  :bind ("C-c d" . docker)
  :config
  (pcase my/emacs-docker-executable
    ('docker
     (setopt docker-command "docker"
           docker-compose-command "docker-compose"
           docker-container-tramp-method "docker"))
    ('podman
     (setopt docker-command "podman"
           docker-compose-command "podman-compose"
           docker-container-tramp-method "podman"))))

;; NOTE: Keep the package for `dockerfile-build-buffer' and
;; `dockerfile-build-no-cache-buffer', but leave syntax highlighting to Tree-Sitter.
(use-package dockerfile-mode
  :mode (("Dockerfile\\'" . dockerfile-ts-mode)
         ("Containerfile\\'" . dockerfile-ts-mode))
  :config
  (pcase my/emacs-docker-executable
    ('docker
     (setopt dockerfile-mode-command "docker"))
    ('podman
     (setopt dockerfile-mode-command "podman"))))

(use-package docker-compose-mode
  :mode ("docker-compose.*\\.yml\\'" "compose.*\\.yml\\'"))

(provide 'setup-docker)
;;; setup-docker.el ends here
