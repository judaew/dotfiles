;;; Package --- Summary

;;; Commentary:

;;; Code:

(use-package company
  :diminish company-mode
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-section-wrap-around t)
  (company-backends '((company-capf :with company-yasnippet))))

;;; setup-completion.el ends here
