;;; Package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package monokai-theme
  :disabled
  :straight (monokai-theme :host github :repo "oneKelvinSmith/monokai-emacs"
                           :fork (:host github :repo "bohonghuang/monokai-emacs" :branch "tab-bar"))
  ;; :straight (monokai-theme :host github :repo "bohonghuang/monokai-emacs" :branch "tab-bar")
  :config
  (load-theme 'monokai t)

  ;; Use italic for comments
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-molokai t)
  (doom-themes-org-config))

;;; setup-themes.el ends here
