;;; Package --- Summary

;;; Commentary:

;;; Code:

(use-package ivy
  :diminish
  :bind ("C-c C-r" . ivy-resume)
  :hook (after-init . ivy-mode))

(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c C-f" . counsel-rg))
  :hook (after-init . counsel-mode))

(use-package swiper
  :bind ("C-s" . swiper))

(use-package ivy-rich
  :diminish
  :hook (after-init . ivy-rich-mode))

;;; setup-search.el ends here
