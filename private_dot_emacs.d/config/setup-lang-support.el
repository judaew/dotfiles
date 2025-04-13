;;; Package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;; Don't use flycheck-inline with lsp-mode, as it breaks inline diagnostics.

;;; Code:

(defun set-flyspell-language-based-on-content ()
  (interactive)
  (let ((text (thing-at-point 'line t)))
    (cond
     ((string-match-p "\\b[А-Яа-я]" text)
      (setq ispell-dictionary "russian"))
     ((string-match-p "\\b[a-zA-Z]\b" text)
      (setq ispell-dictionary "english"))
     (t
      (setq ispell-dictionary "english")))))

(use-package flyspell
  :hook
  ((text-mode . flyspell-mode)
   (prog-mode . flyspell-prog-mode))
  :init
  ;; (add-hook 'flyspell-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'post-command-hook 'set-flyspell-language-based-on-content nil t)))
  :config
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "english")
  (setq ispell-dictionary-alist
        '(("english" "en_US" "[[:alpha:]]" "[^[:alpha:]]" nil ("-d" "en_US"))
          ("ukrainian" "uk_UA" "[[:alpha:]]" "[^[:alpha:]]" nil ("-d" "uk_U"))
          ("russian" "ru_RU" "[[:alpha:]]" "[^[:alpha:]]" nil ("-d" "ru_RU")))))

(use-package ispell
  :ensure t
  :config
  (global-set-key (kbd "C-c i") 'ispell-change-dictionary))

;; Syntax highlighting for systemd files
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.netdev\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.network\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.link\\'" . conf-unix-mode))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom (flycheck-emacs-lisp-load-path 'inherit))

(use-package eldoc
  :custom (eldoc-minor-mode-string nil))

(use-package cmake-mode
  :straight nil
  :defer t)
(use-package ninja-mode
  :straight nil
  :defer)

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
     (setf docker-command "docker"
	   docker-compose-command "docker-compose"
	   docker-container-tramp-method "docker"))
    ('podman
     (setf docker-command "podman"
	   docker-compose-command "podman-compose"
	   docker-container-tramp-method "podman"))))

(use-package dockerfile-mode
  :defer t
  :config
  (pcase my/emacs-docker-executable
    ('docker
     (setq dockerfile-mode-command "docker"))
    ('podman
     (setq dockerfile-mode-command "podman"))))

(use-package docker-compose-mode
  :defer t)

(use-package go-mode
  :defer t)

(use-package lua-mode
  :defer t)

(use-package protobuf-mode
  :straight nil
  :defer t)

(use-package dhall-mode
  :defer t
  :custom
  ;; header-line is obsoleted by lsp-mode
  (dhall-use-header-line nil))

(use-package nginx-mode
  :defer t)

(use-package pyvenv
  :defer t
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (message "Searching for venv in: %s" default-directory)
              (let ((venv-dir (locate-dominating-file default-directory ".venv")))
                (if venv-dir
                    (progn
                      (message "Activating venv: %s" venv-dir)
                      (pyvenv-activate (expand-file-name ".venv" venv-dir)))
                  (message "No .venv found"))))))

;;; setup-lang-support.el ends here
