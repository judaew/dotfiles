;;; init-langs.el --- Tools for programming languages -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; === Formatting ===
;; - `apheleia'      ; run code formatter on buffer contents

;; === Tags ===
;; - `citre'         ; advanced Ctags frontend

;; === Python ===
;; - `pyvenv'        ; manage Python virtual environment

;; === Language modes ===
;; - `ninja-mode'    ; major mode for Ninja build files
;; - `protobuf-mode' ; major mode for Protocol Buffers
;; - `nginx-mode'    ; major mode for Nginx config

;; === Systemd ===
;; Syntax highlighting for systemd files

;;; Code:

;; === Formatting ===
;; ------------------

(use-package apheleia
  :hook (after-init . apheleia-global-mode)
  :config
  (setf (alist-get 'go-ts-mode apheleia-mode-alist)
        '(goimports)))

;; === Tags ===
;; ------------

(use-package citre
  :defer t
  :bind (("C-c c j" . citre-jump)
         ("C-c c J" . citre-jump-back)
         ("C-c c p" . citre-peek)
         ("C-c c u" . citre-update-database)
         ("C-c c U" . citre-update-tags-file))

  ;; Avoid using citre-config because it automatically enables `citre-mode'
  ;; in all buffers, adding tags to `completion-at-point' which I want to prevent.
  ;; `citre-peek' and `citre-jump' work perfectly fine without citre-mode.
  ;;
  ;;:init (require 'citre-config)
  :config
  (setopt citre-peek-fill-fringe nil)
  (setopt citre-peek-use-dashes-as-horizontal-border t))

;; === Python ===
;; --------------

(use-package pyvenv
  :defer t)

;; === Language modes ===
;; ----------------------

(use-package ninja-mode
  :defer t)

;; Use emacsmirror version containing only the .el mode file (no protobuf src)
(use-package protobuf-mode
  :straight (protobuf-mode :type git :host github :repo "emacsmirror/protobuf-mode" :branch "master")
  :defer t)

(use-package nginx-mode
  :defer t)

;; === Systemd ===
;; ---------------

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

(provide 'init-langs)
;;; init-langs.el ends here
