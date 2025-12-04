;;; setup-dap.el --- DAP support -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - dape            ~ Debug Adapter Protocol for Emacs
;; - projection-dape ~ Projection integration for `dape'

;;; Code:

(use-package emacs
  :straight nil
  :custom
  ;; Left and right side windows occupy full frame height
  (window-sides-vertical t))

(use-package dape
  :hook
  (kill-emacs . dape-breakpoint-save) ;; Save breakpoints on quit
  (after-init . dape-breakpoint-load) ;; Load breakpoints on startup
  :custom
  (dape-buffer-window-arrangement 'right)

  :config
  ;; Pulse source line (performance hit)
  (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)
  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))
  ;; Kill compile buffer on build success
  (add-hook 'dape-compile-hook #'kill-buffer))

(use-package projection-dape
  :after projection dape)

(provide 'setup-dap)
;;; setup-dap.el ends here
