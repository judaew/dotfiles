;;; init-vc.el --- VC tools -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `magit'           ; Git porcelain inside Emacs
;; - `git-modes'       ; major modes for Git configuration files
;; - `git-timemachine' ; walk through git revisions of a file
;; - `forge'           ; integrate Git forges (like GitHub/GitLab)
;; - `diff-hl'         ; highlight changes in `fringe-mode'

;;; Code:

(use-package magit
  :bind
  (("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)))

(use-package git-modes
  :config
  (add-to-list 'auto-mode-alist
               (cons "/\\.dockerignore\\'" 'gitignore-mode)))

(use-package git-timemachine
  :commands (git-timemachine))

(use-package forge
  :custom
  (forge-add-default-bindings t))

(use-package diff-hl
  :hook
  ((magit-post-refresh . diff-hl-magit-post-refresh)
   (dired-mode . diff-hl-dired-mode))
  :config
  (global-diff-hl-mode 1)
  (setopt diff-hl-side 'right)
  ;; highlighting changes on the fly
  (diff-hl-flydiff-mode 1))

(provide 'init-vc)
;;; init-vc.el ends here
