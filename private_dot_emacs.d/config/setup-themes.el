;;; Package --- Summary

;;; Commentary:

;;; Code:

(use-package monokai-theme
  :config
  (load-theme 'monokai t)

  ;; Use italic for comments
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic))

;;; setup-themes.el ends here
