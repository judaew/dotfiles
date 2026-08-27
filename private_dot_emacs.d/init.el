;;; init.el --- Init configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defvar elpaca-core-date '(20260724))

;;; Elpaca: An Elisp Package Manager

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Packages
(elpaca elpaca-use-package (elpaca-use-package-mode))
(setopt use-package-always-ensure t)

;;; General config

;; Set fonts for fixed-pitch and variable-pitch
(let ((font-family-fixed "Iosevka Curly")
      (font-family-pitch "Iosevka Aile")
      (font-size (if (eq system-type 'darwin) 130 110)))
  (when (member font-family-fixed (font-family-list))
    (set-face-attribute 'default nil
                        :font font-family-fixed :height font-size)
    (set-face-attribute 'fixed-pitch nil
                        :font font-family-fixed))
  (when (member font-family-pitch (font-family-list))
    (set-face-attribute 'variable-pitch nil
                        :font font-family-pitch :height font-size)))

;; don't  compact font caches during GC
(setopt inhibit-compacting-font-caches t)

;; Disable creating lock files
(setopt create-lockfiles nil)

;; Backup & auto-save
(let ((backup-dir (concat user-emacs-directory "backup/"))
      (autosave-dir (concat user-emacs-directory "autosaves/")))
  (unless (file-directory-p backup-dir)
    (make-directory backup-dir t))
  (unless (file-directory-p autosave-dir)
    (make-directory autosave-dir t))

  (setopt backup-directory-alist `(("." . ,backup-dir))
          backup-by-copying t
          version-control t
          delete-old-versions t
          kept-old-versions 2
          kept-old-versions 5)

  (setopt auto-save-default t)
  (setopt auto-save-file-name-transforms `((".*" ,autosave-dir t))
          auto-save-visited-file-name nil))

;; Shortened yes-or-no-p to y-or-n-p
(setopt use-short-answers t)

;; Show current project on the default mode-line
(setopt project-mode-line t)

;; Enable line numbers
(global-display-line-numbers-mode t)

;; Add line and column to modeline
(line-number-mode)
(column-number-mode)

;; Enable smart parens
(electric-pair-mode t)

;; Don't use /anywhere/ tabs
(setq-default indent-tabs-mode nil)

(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; Tab-bar
(setopt tab-bar-show 1) ;; auto-hide
(global-set-key (kbd "C-<next>") 'tab-next)
(global-set-key (kbd "C-<prior>") 'tab-previous)

;; Security: only save file-local variables
(setopt enable-local-variables :safe)

;; GnuPG pinentry via the Emacs minibuffer
(setopt epg-pinentry-mode 'loopback)
(setopt epa-pinentry-mode 'loopback)

;; Eldoc-based help-at-point
(setopt eldoc-show-help-at-pt t)

;; Smart kill-region behavior; very useful for C-w without active region
(setopt kill-region-dwim 'emacs-word)

;; show match numbers in the search prompt
(setopt isearch-lazy-count t)

;; Useful for tabs (like in Golang)
(setopt x-stretch-cursor t)

;; Eldoc at point
(setopt eldoc-help-at-pt t)

;; KB/MB instead of raw byte counts
(setopt ibuffer-human-readable-size t)

;; Stop native-comp jobs on battery
;; (setopt native-comp-async-on-battery-power t)

;; (use-package server
;;   :ensure nil
;;   :config
;;   (unless (server-running-p)
;;     (server-start)))

;; Load modular configuration files
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'init-completion)
(require 'init-editing)
(require 'init-projects)
(require 'init-ide)
(require 'init-vc)
(require 'init-vc-gh)
(require 'init-org)
(require 'init-ui)
(require 'init-ui-mode-line)
(require 'init-langs)
(require 'init-spell)
(require 'init-term)
(require 'init-term-clipboard)
(require 'init-ai)
(require 'init-dired)

;; Set custom-file location
(setopt custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil 'noerror))

;; Make Flymake see packages from current load-path
(setopt elisp-flymake-byte-compile-load-path load-path)

;;; init.el ends here
