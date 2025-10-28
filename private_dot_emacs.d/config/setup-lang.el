;;; setup-lang.el --- Programming language modes setup -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `go-mode'       ~ major mode for Go
;; - `python-mode'  ~ major mode for Python
;; - `pyvenv'       ~ manage Python virtual environment
;; - `python-blcak' ~ reformat Python code using black
;; - `lua-mode'      ~ major mode for Lua
;; - `cmake-mode'    ~ major mode for CMake
;; - `ninja-mode'    ~ major mode for Ninja build files
;; - `protobuf-mode' ~ major mode for Protocol Buffers
;; - `nginx-mode'    ~ major mode for Nginx config

;;; Code:

;; Go block
;; ##############################

;; NOTE: Keep the package for go cmds, but leave syntax highlighting to Tree-Sitter.
;; See https://github.com/dominikh/go-mode.el/issues/396#issuecomment-1366716882
(defun my/go-mode-setup ()
  "Setup Go mode to my liking."
  (add-hook 'before-save-hook 'gofmt nil t))

(use-package go-mode
  :mode (("\\.go$" . go-ts-mode)
	 ("go.mod\\'" . go-mod-ts-mode))
  :hook ((go-mode go-ts-mode) . my/go-mode-setup)
  :custom (gofmt-command "goimports"))

;; Python block
;; ##############################

(use-package pyvenv
  :defer t)

(use-package python-black
  :demand t
  :after python
  :hook (python-ts-mode . python-black-on-save-mode-enable-dwim))

;; Misc block
;; ##############################

(use-package ninja-mode
  :defer t)

(use-package protobuf-mode
  :defer t)

(use-package nginx-mode
  :defer t)

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

(provide 'setup-lang)
;;; setup-lang.el ends here
