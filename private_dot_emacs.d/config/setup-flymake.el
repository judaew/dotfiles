;;; setup-flymake.el --- Syntax checking -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `flymake' ~ on-the-fly syntax checking

;;; Code:

(use-package emacs
  :defer 1
  :straight nil
  :hook (prog-mode . flymake-mode)
  :config
  (setopt elisp-flymake-byte-compile-load-path load-path)
  (setopt trusted-content '("~/.emacs.d/early-init.el" "~/.emacs.d/config/")))

(provide 'setup-flymake)
;;; setup-flymake.el ends here
