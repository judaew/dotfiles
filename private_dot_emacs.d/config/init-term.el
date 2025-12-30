;;; init-term.el --- Terminal and environment -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; === Environment ===
;; - `exec-path-from-shell' ; sync shell environment with Emacs
;; - `direnv'               ; direnv integration
;; - `with-editor'          ; use the emacsclient as the $EDITOR of child processes

;; === Terminal ===
;; - `mouse'                ; mouse support in terminal
;; - `vterm'                ; libvterm integration
;; - `termclip'             ; clipboard support in terminal

;; === Docker ===
;; - `docker'               ; Docker integration
;; - `dockerfile-mode'      ; major mode for editing Dockerfiles
;; - `docker-compose-mode'  ; major mode for editing docker-compose files

;;; Code:

;; === Environment ===
;; -------------------

(use-package exec-path-from-shell
  :config
  (setopt exec-path-from-shell-arguments (list "-l"))
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(use-package direnv
  :hook (after-init . direnv-mode))

(use-package with-editor
  :bind
  (([remap async-shell-command] . with-editor-async-shell-command)
   ([remap shell-command] . with-editor-shell-command))
  :hook
  ((shell-mode eshell-mode term-mode vterm-mode) . with-editor-export-editor))

;; === Terminal ===
;; ----------------

;; `xterm-mouse-mode'
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (if (display-graphic-p frame)
                  (xterm-mouse-mode 0)
                (xterm-mouse-mode 1)))))

(use-package vterm
  :bind
  (("C-~" . vterm))
  :custom
  (vterm-max-scrollback 10000))

(use-package termclip
  :straight (termclip :type git :local-repo "~/wrk/github.com/judaew/termclip.el")
  :config
  (if (eq system-type 'darwin)
      (setopt termclip-clipboard-tool 'macos)
    (setopt termclip-clipboard-tool 'wayland)))

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

(use-package docker-compose-mode
  :mode ("docker-compose.*\\.yml\\'" "compose.*\\.yml\\'"))

(provide 'init-term)
;;; init-term.el ends here
