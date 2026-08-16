;;; init-term.el --- Terminal and environment -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; === Environment ===
;; - `exec-path-from-shell' ; sync shell environment with Emacs
;; - `envrc'                ; direnv integration
;; - `with-editor'          ; use the emacsclient as the $EDITOR of child processes

;; === Terminal ===
;; - `mouse'                ; mouse support in terminal
;; - `ghostel'              ; libghostty-vt integration

;; === Docker ===
;; - `docker'               ; Docker integration
;; - `dockerfile-mode'      ; major mode for editing Dockerfiles

;; TODO: kubernetes.el

;;; Code:

;; === Environment ===
;; -------------------

(use-package emacs
  :ensure nil
  :config (xterm-mouse-mode 1))

(use-package exec-path-from-shell
  :config
  (setopt exec-path-from-shell-arguments
          (if (eq system-type 'darwin) (list "-l") nil))
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(use-package envrc
  :config (envrc-global-mode 1))

;; === Terminal ===
;; ----------------

(add-to-list 'elpaca-ignored-dependencies 'ghostel)

(use-package ghostel
  :ensure nil ;; nix
  :bind
  (("C-~" . ghostel)))

(use-package ghostel-compile
  :ensure nil ;; nix
  :config (ghostel-compile-global-mode 1))

(use-package ghostel-comint
  :ensure nil ;; nix
  :config (ghostel-comint-global-mode 1))

;; === Docker ===
;; --------------

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

(provide 'init-term)
;;; init-term.el ends here
