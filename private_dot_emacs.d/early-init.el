;;; package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Disable GC during startup --- biggest speed win
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Schedule garbage collection sensible defauls for after booting
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024) ;; 100 mb
                  gc-cons-percentage 0.1
                  read-process-output-max (* 2 1024 1024)))) ;; 2 mb

;; Set a minimal frame size early
(add-to-list 'default-frame-alist '(width . 88))
;;(add-to-list 'default-frame-alist '(height . 36))
(add-to-list 'default-frame-alist '(height . 33))

;; Set font
(add-to-list 'default-frame-alist
             `(font . ,(format "Iosevka Curly-%d"
                               (if (eq system-type 'darwin) 13 11))))

;; Fix background-color on startup before theme loads
(add-to-list 'default-frame-alist '(background-color . "#1c1e1f"))

;; Single VC backend inscreases booting speed
(setq vc-handled-backends '(Git))

;; Do not native compile if on battery power
(setopt native-comp-async-on-battery-power nil)

;; Always start Emacs and new frames maximized
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Turn off UI chrome **immediately**
(push '(menu-bar-lines . 1) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Create empty buffer *scratch* in org-mode
(setopt inhibit-startup-screen t)
(setopt initial-major-mode 'org-mode)
(setopt initial-scratch-message "#+TITLE: Scratch Buffer\n\n")

;; Avoid raising the *Messages* buffer if anything is still without
;; lexical bindings
(setopt warning-minimum-level :error)
(setopt warning-suppress-types '((lexical-binding)))

;;; Disable package.el
(setopt package-enable-at-startup nil)

;;; early-init.el ends here
