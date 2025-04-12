;;; Package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments (list "-l"))
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(use-package vterm
  :bind
  (("C-~" . vterm)
   ("C-c t" . vterm))
  :custom
  (vterm-max-scrollback 10000))

(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

(use-package direnv
  :hook (after-init . direnv-mode))

(use-package mouse
  :straight nil
  :unless (display-graphic-p)
  :hook (after-init . xterm-mouse-mode))

;;; setup-term.el ends here
