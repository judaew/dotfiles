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
     (setq docker-command "docker"
	   docker-compose-command "docker-compose"
	   docker-container-tramp-method "docker"))
    ('podman
     (setq docker-command "podman"
	   docker-compose-command "podman-compose"
	   docker-container-tramp-method "podman"))))

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" "Containerfile\\'")
  :config
  (pcase my/emacs-docker-executable
    ('docker
     (setq dockerfile-mode-command "docker"))
    ('podman
     (setq dockerfile-mode-command "podman"))))

(use-package docker-compose-mode
  :mode ("docker-compose.*\\.yml\\'" "compose.*\\.yml\\'"))

(provide 'setup-docker)
;;; setup-docker.el ends here
