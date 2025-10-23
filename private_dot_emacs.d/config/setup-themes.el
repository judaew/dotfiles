;;; setup-themes.el --- Themes and appearance -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `doom-themes' ~ collection of Doom Emacs themes

;;; Code:

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-molokai t)
  (doom-themes-org-config)

  ;; Use italic for comments
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic))

(provide 'setup-themes)
;;; setup-themes.el ends here
