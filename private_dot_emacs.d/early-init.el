;;; package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Use plists for LSP
(setenv "LSP_USE_PLISTS" "true")

;; Disable GC during startup --- biggest speed win
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Adjust gc-cons-threshold and increase the amount of data which
;; Emacs reads from the process
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024) ;; 100 mb
                  gc-cons-percentage 0.1
                  read-process-output-max (* 1024 1024)))) ;; 1 mb

;; Set a minimal frame size early
(add-to-list 'default-frame-alist '(width . 88))
;;(add-to-list 'default-frame-alist '(height . 36))
(add-to-list 'default-frame-alist '(height . 33))
(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(font . "VictorMono Nerd Font Mono-12"))
  (add-to-list 'default-frame-alist '(font . "VictorMono Nerd Font Mono-11")))
;; Fix background-color on startup before theme loads
(add-to-list 'default-frame-alist '(background-color . "#1c1e1f"))

;; Turn off UI chrome **immediately**
(push '(menu-bar-lines . 1) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Create empty buffer *scratch* in org-mode
(setopt inhibit-startup-screen t)
(setopt initial-major-mode 'org-mode)
(setopt initial-scratch-message "#+TITLE: Scratch Buffer\n\n")

;;; Disable package.el
(setopt package-enable-at-startup nil)

;;; early-init.el ends here
