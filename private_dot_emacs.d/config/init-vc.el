;;; init-vc.el --- VC tools -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `magit'           ; Git porcelain inside Emacs
;; - `magit-delta'     ; use delta when viewing diffs in Magit
;; - `magit-todos'     ; show source files' TODOs (and FIXMEs etc) in Magit
;; - `git-modes'       ; major modes for Git configuration files
;; - `git-timemachine' ; walk through git revisions of a file
;; - `forge'           ; integrate Git forges (like GitHub/GitLab)
;; - `diff-hl'         ; highlight changes in `fringe-mode'

;;; Code:

(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)))

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

(use-package magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package git-modes
  :config
  (add-to-list 'auto-mode-alist
               (cons "/.dockerignore\\'" 'gitignore-mode)))

(use-package git-timemachine
  :after magit
  :commands (git-timemachine))

(use-package forge
  :after magit
  :custom
  (forge-add-default-bindings t))

(use-package diff-hl
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (after-init . global-diff-hl-mode))
  :config
  (setopt diff-hl-side 'right)
  ;; highlighting changes on the fly
  (diff-hl-flydiff-mode 1))

(provide 'init-vc)
;;; init-vc.el ends here
