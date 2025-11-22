;;; setup-completion-backend.el --- Completion-at-point backends -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `cape'              ~ completion at point extension
;; - `dabbrev'           ~ dynamic abbrev
;; - `prescient'         ~ better sorting and filtering for completion
;; - `corfu-prescient'   ~ prescient integration for corfu
;; - `vertico-prescient' ~ prescient integration for vertico
;; - `tempel'            ~ template/snippet expansion engine

;;; Code:

;; Core Emacs completion UI enhancements
(use-package emacs
  :straight nil
  :custom
  ;; Use TAB for indentation and completion
  (tab-always-indent 'complete)
  ;; Disable Ispell in text completion (#emacs30) for speed
  ;; And use `cape-dict' and `cape-abbrev' as an alternative
  (text-mode-ispell-word-completion nil))

(use-package cape
  :after prescient
  :bind ("M-p" . cape-prefix-map)
  :config
  (defun my/capf-super-unsorted (&rest capfs)
    "Wrapper around `cape-capf-super' that disables sorting of completion candidates."
    (let ((capf (apply #'cape-capf-super capfs)))
      (cape-capf-properties
       capf
       :display-sort-function nil
       :cycle-sort-function nil)))

  (defun my/setup-capf (functions)
    "Setup completion-at-point-functions with given FUNCTIONS list."
    (setq-local completion-at-point-functions
                (mapcar (lambda (f) (if (function f) f (symbol-function f)))
                        functions)))

  (defconst my/capf-common-prog
    (list
     #'tempel-complete
     (cape-capf-inside-string #'cape-rfc1345)
     (cape-capf-inside-comment #'cape-rfc1345)
     (cape-capf-inside-string #'cape-dabbrev)
     (cape-capf-inside-comment #'cape-dabbrev)
     #'cape-keyword)
    "Completion list for prog-based modes (non-LSP)")
  (defun my/setup-capf-common-prog ()
    (my/setup-capf my/capf-common-prog))

  (defconst my/capf-eglot
    (list
     #'tempel-complete
     #'eglot-completion-at-point
     (cape-capf-inside-string #'cape-dabbrev)
     (cape-capf-inside-comment #'cape-dabbrev))
    "Completion list for eglot-based modes")
  (defun my/setup-capf-eglot ()
    (my/setup-capf my/capf-eglot))

  (defconst my/capf-elisp
    (list
     #'tempel-complete
     #'cape-elisp-symbol
     (cape-capf-inside-string #'cape-dabbrev)
     (cape-capf-inside-comment #'cape-dabbrev))
    "Completion list for `emacs-lisp-mode'")
  (defun my/setup-capf-elisp ()
    (my/setup-capf my/capf-elisp))

  (defconst my/capf-common-text
    (list
     #'tempel-complete
     #'cape-rfc1345
     #'cape-dabbrev
     #'cape-keyword)
    "Completion list for text-based modes")
  (defun my/setup-capf-common-text ()
    (my/setup-capf my/capf-common-text))

  (defun my/setup-capf-org ()
    (my/setup-capf
     (cons #'cape-elisp-block my/capf-common-text)))

  :hook
  ((prog-mode . my/setup-capf-common-prog)
   (emacs-lisp-mode . my/setup-capf-elisp)
   (text-mode . my/setup-capf-common-text)
   (org-mode . my/setup-capf-org)))

(use-package dabbrev
  :straight nil
  ;; Default binds: M-/ -- `dabbrev-completion'; C-M-/ -- `dabbrev-expand'
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package prescient
  :config
  (prescient-persist-mode 1)
  (setopt prescient-aggressive-file-save t))

(use-package corfu-prescient
  :after corfu prescient
  :config
  (corfu-prescient-mode 1))

(use-package vertico-prescient
  :after vertico prescient
  :config
  (vertico-prescient-mode 1)

  ;; Disable prescient sorting for file completions (e.g. C-x C-f)
  ;; to preserve natural filesystem order and allow creating new files
  ;; like "test2.go" even if "test.go" exists.
  ;; See https://www.reddit.com/r/emacs/comments/109ryp7/comment/j4excag
  (setf vertico-prescient-completion-category-overrides
        (assoc-delete-all 'file
                          vertico-prescient-completion-category-overrides)))

(use-package tempel
  :after cape
  :bind
  (("M-+" . tempel-complete)
   (:map tempel-map
         ("TAB" . tempel-next)
         ("[tab]" . tempel-next)
         ("S-TAB" . tempel-prev)
         ("[backtab]" . tempel-prev))))

(provide 'setup-completion-backend)
;;; setup-completion-backend.el ends here
