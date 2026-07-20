;;; init-term.el --- Terminal and environment -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; === Environment ===
;; - `exec-path-from-shell' ; sync shell environment with Emacs
;; - `direnv'               ; direnv integration
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
  :hook (after-init . xterm-mouse-mode))

(use-package exec-path-from-shell
  :config
  (setopt exec-path-from-shell-arguments
          (if (eq system-type 'darwin) (list "-l") nil))
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(use-package direnv
  :after exec-path-from-shell
  :hook (after-init . direnv-mode))

;; === Terminal ===
;; ----------------

(use-package vterm)

(use-package ghostel
  :bind
  (("C-~" . ghostel))
  :hook
  ((after-init . ghostel-compile-global-mode)
   (after-init . ghostel-comint-global-mode)))

(use-package vtermux
  :straight (vtermux :type git :host github :repo "pcmantz/vtermux")
  :config
  (setopt vtermux-backend 'ghostel)
  (vtermux-define zsh)
  (vtermux-define lf)
  (vtermux-define htop))

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

(provide 'init-term)
;;; init-term.el ends here
