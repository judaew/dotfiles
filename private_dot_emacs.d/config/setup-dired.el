;;; setup-dired.el --- Dired -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `dired'        ~ build-in file manager core
;; - `dired-x'      ~ extra Dired commands
;; - `dired-launch' ~ open files by external apps (like xdg-open)
;; - `dirvish'      ~ modern Dired UI with preview panel, icons, async ops
;; - `diredfl'      ~ nicer faces for Dired entries

;;; Code:

;; Default binds:
;; - "C-x C-j" . dired-jump
(use-package dired
  :straight nil
  :hook (dired-mode . dired-hide-details-mode)
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "--almost-all --human-readable --format=long --group-directories-first --no-group")
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)
  ;; enabling reuse of dired buffer on open #emacs28
  (dired-kill-when-opening-new-dired-buffer t)
  ;; mouse integration #emacs29
  (dired-mouse-drag-files t)
  (mouse-drag-and-drop-region-cross-program t)
  :config
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dired-x
  :straight nil
  :after dired
  :hook (dired-mode . dired-omit-mode)
  :bind (:map dired-mode-map
	      ("." . dired-omit-mode))
  :custom
  (dired-omit-files (rx (seq bol "."))))

;; binds: 'J' for laucnh, 'K' for launch with prompt
(use-package dired-launch
  :after dired
  :config
  (dired-launch-enable)
  (setq dired-launch-default-launcher '("xdg-open"))
  (setq dired-launch-extensions-map
        '(;; fix for png extensions
          ("png" ("gwenview")))))

;; Unset F2 from 2C-mode for `dirvish-side'
(when (keymapp (lookup-key global-map (kbd "<f2>")))
  (global-unset-key (kbd "<f2>")))

(use-package dirvish
  :after dired
  :init (dirvish-override-dired-mode)
  :bind
  (("C-c f" . dirvish)
   ("<f2>" . dirvish-side)
   :map dirvish-mode-map
   (";" . dired-up-directory)
   ("?" . dirvish-dispatch)            ; [?] a helpful cheatsheet
   ("a" . dirvish-setup-menu)          ; [a]ttributes settings
   ("f" . dirvish-file-info-menu)      ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu))
  :custom
  (dirvish-side-width 30)
  (dirvish-collapse-separator "/")
  (dirvish-quick-access-entries
   '(("h" "~/" "home")
     ("d" "~/dl/" "dl")
     ("w" "~/wrk/" "wrk")
     ("o" "~/org/" "org")
     ("m" "/mnt/" "mnt")
     ("t" "~/.local/share/Trash/files/" "trash")))
  :config
  (dirvish-side-follow-mode t)
  (setq dirvish-attributes
      (append
       ;; The order of these attributes is insignificant, they are always
       ;; displayed in the same position.
       '(vc-state subtree-state nerd-icons collapse)
       ;; FAIL: See https://github.com/alexluigit/dirvish/issues/356
       ;; Other attributes are displayed in the order they appear in this list.
       ;; '(git-msg file-size)))
       ))
  ;; Also FAIL: see prev comment
  (setq dirvish-side-attributes
	(append
	 '(vc-state subtree-state nerd-icons collapse)))

  (setq dirvish-header-line-format
	'(:left (path) :right (free-space))
	dirvish-mode-line-format
        '(:left (sort file-time " " file-size symlink) :right (omit yank index))))

(use-package diredfl
  :hook
  ((dired-mode . diredfl-mode)
   ;; highlight parent and directory preview as well
   (dirvish-directory-view-mode . diredfl-mode))
  :config
  (set-face-attribute 'diredfl-dir-name nil :bold t))

(provide 'setup-dired)
;;; setup-dired.el ends here
