;;; setup-term.el --- Terminal support -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `exec-path-from-shell' ~ sync shell environment with Emacs
;; - `eat'                  ~ emulate a terminal
;; - `vterm'                ~ libvterm integration
;; - `direnv'               ~ direnv integration
;; - `termclip'             ~ clipboard support in terminal
;; - `mouse'                ~ mouse support in terminal

;;; Code:

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-arguments (list "-l"))
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(use-package eat
  :straight (eat :type git
                 :host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :bind
  (("C-~" . eat)
   ("C-c t" . eat)))

(use-package vterm
  :custom
  (vterm-max-scrollback 10000))

(use-package direnv
  :hook (after-init . direnv-mode))

(use-package termclip
  :straight (termclip :type git :local-repo "~/wrk/github.com/judaew/termclip.el")
  :custom
  (termclip-clipboard-tool 'wayland))

(use-package mouse
  :straight nil
  :config
  (add-hook 'after-make-frame-functions
	    (lambda (frame)
	      (with-selected-frame frame
		(if (display-graphic-p frame)
		    (xterm-mouse-mode 0)
		  (xterm-mouse-mode 1))))))

(provide 'setup-term)
;;; setup-term.el ends here
