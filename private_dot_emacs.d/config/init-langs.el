;;; init-langs.el --- Tools for programming languages -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; === Formatting ===
;; - `apheleia'      ; run code formatter on buffer contents

;; === Tags ===
;; - `citre'         ; advanced Ctags frontend

;; === Rust ===
;; - `rust-mode'     ; major mode and configuration for Rust
;; - `rustic'        ; the steroids for rust-mode

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

;; === Rust ===
;; ------------

;; - C-c C-d     -- easy insetion of dbg!
(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :custom
  (rust-format-on-save t))

;; - C-c C-p     -- rustic-popup
;; - C-c C-c C-u -- rustic-compile
;; - C-c C-c C-i -- rustic-recompile
;; - C-c C-c C-o -- rustic-format-buffer
;; - C-c C-c C-, -- rustic-docstring-dwim
;; - C-c C-c C-b -- rustic-cargo-build
;; - C-c C-c C-k -- rustic-cargo-check
;; - C-c C-c C-r -- rustic-cargo-run
;; - C-c C-c C-f -- rustic-cargo-fmt
;; - C-c C-c C-t -- rustic-cargo-test
;; - C-c C-c C-c -- rustic-cargo-current-test
;; - C-c C-c C-l -- rustic-cargo-clippy
;; - C-c C-c C-n -- rustic-cargo-outdated
;; - C-c C-c n   -- rustic-cargo-new
;; - C-c C-c i   -- rustic-cargo-init
;; - C-c C-c b   -- rustic-cargo-bench
;; - C-c C-c d   -- rustic-cargo-doc
;; - C-c C-c c   -- rustic-cargo-clean
;; - C-c C-c k   -- rustic-cargo-clippy
;; - C-c C-c f   -- rustic-cargo-clippy-fix
;; - C-c C-c a   -- rustic-cargo-add
;; - C-c C-c r   -- rustic-cargo-rm
;; - C-c C-c u   -- rustic-cargo-upgrade
(use-package rustic
  :after rust-mode
  :custom
  (rustic-lsp-client 'eglot))

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

(use-package nix-ts-mode
  :mode "\\.nix\\'")

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
