;;; init-dired.el --- Dired -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; === Built-in Dired ===
;; - `dired'        ; build-in file manager core

;; === Enhancements ===
;; - `dired-filter'     ; filter stack
;; - `diredfl'          ; nicer faces for Dired entries
;; - `dired-open'       ; open with external apps
;; - `dired-subtree'    ; show subdirectories by <TAB>
;; - `dired-collapse'   ; show single file or directory in directories
;; - `dired-sidebar'    ; sidebar
;; - `nerd-icons-dired' ; use nerd-icons for Dired

;;; Code:

;; === Built-in Dired ===
;; ----------------------

;; Default binds:
;; - "C-x C-j" . dired-jump
;; - "C-x C-q" . wdired for make buffer editable and "C-c C-c" for save
(use-package dired
  :straight nil
  :hook (dired-mode . dired-hide-details-mode) ;; hide details by default
  :custom
  (dired-dwim-target t) ;; guess target directory for copy/move
  (dired-listing-switches "--almost-all --human-readable --format=long --group-directories-first --no-group")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  (dired-kill-when-opening-new-dired-buffer t) ;; reuse buffer
  ;; mouse integration
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program t)
  :config
  (put 'dired-find-alternate-file 'disabled nil))

;; === Enhancements ===
;; --------------------

(use-package dired-filter
  :hook
  (dired-mode . dired-filter-mode)
  :custom
  (dired-omit-files (rx (seq bol ".")))) ;; hide dotfiles

(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  :config (set-face-attribute 'diredfl-dir-name nil :bold t))

;; for macOS see dired-launch
(use-package dired-open
  :bind ("S-<return>" . dired-open-xdg))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))

(use-package dired-collapse
  :hook (dired-mode . dired-collapse-mode)
  :config
  (defun my/dired-collapse-fix-subtree (&rest _)
    "Remove dired-collapse from subtree hook to prevent visual breakage."
    (remove-hook 'dired-subtree-after-insert-hook #'dired-collapse 'local))

  (advice-add 'dired-collapse-mode :after #'my/dired-collapse-fix-subtree))

(use-package dired-quick-sort
  :after dired
  :hook (dired-mode . dired-quick-sort-setup)
  :custom (dired-quick-sort-suppress-setup-warning t))

;; Unset F2 from 2C-mode for `dired-sidebar'
;; Only unset if the key is bound to a command whose name starts with "2C"
(let ((key-def (lookup-key global-map (kbd "<f2>"))))
  (when (and (not (keymapp key-def))
             (symbolp key-def)
             (string-match-p "\\`2C" (symbol-name key-def)))
    (global-unset-key (kbd "<f2>"))))

(use-package dired-sidebar
  :bind ("<f2>" . dired-sidebar-toggle-sidebar)
  :hook (dired-sidebar-mode . (lambda () (display-line-numbers-mode -1)))
  :custom
  (dired-sidebar-width 30))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
