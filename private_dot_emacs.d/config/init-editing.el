;;; init-editing.el --- Editing and navigation -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; === Editing ===
;; - `smart-hungry-delete' ; smart hungry delete
;; - `multiple-cursors'    ; multiple cursors
;; - `iedit'               ; edit multiple regions
;; - `saveplace'           ; remember cursor position
;; - `editorconfig'        ; .editorconfig support
;; - `expreg'              ; expand-region using Tree-Sitter

;; === Movement and navigation ===
;; - `ace-window'          ; window numbering and navigation
;; - `avy'                 ; jump to visible text
;; - `windresize'          ; resize windows
;; - `repeat'              ; repeating a command

;; === Undo & redo support ===
;; - `undo-fu'             ; functional undo/redo
;; - `undo-fu-session'     ; persistent undo sessions
;; - `vundo'               ; visual undo tree
;; - `winner'              ; window layout undo/redo

;;; Code:

;; === Editing ===
;; ---------------

;; sudo-edit
;; From Emacs-31 onwards this wont be necessary, as C-x x @ will call
;; `tramp-revert-buffer-with-sudo'

;; Trim whitespace on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(use-package smart-hungry-delete
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
         ([remap delete-backward-char] . smart-hungry-delete-backward-char)
         ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)))

;; By default binds:
;; - C-; -- iedit-mode
(use-package iedit)

(use-package saveplace
  :ensure nil
  :config (save-place-mode 1))

(use-package editorconfig
  :ensure nil
  :config (editorconfig-mode 1))

(use-package expreg
  :bind ("C-=" . expreg-expand))

;; === Movement and navigation ===
;; -------------------------------

(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package avy
  ;; By default, M-j is bound to `default-indent-new-line',
  ;; but Avy is more useful.
  :bind ("M-j" . avy-goto-char-timer))

(use-package windresize
  :bind ("C-c r" . windresize))

;; For a more ergonomic Emacs and `dape' experience
;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Repeating.html
;; like C-x-left-left-left-right and etc
(use-package repeat
  :ensure nil
  :init (repeat-mode 1))

;; === Undo & redo support ===
;; ---------------------------

(use-package undo-fu
  :bind
  (("C-/" . undo-fu-only-undo)
   ("C-?" . undo-fu-only-redo))
  :custom
  (undo-limit (* 32 1024 1024))
  (undo-strong-limit (* 64 1024 1024))
  (undo-outer-limit (* 128 1024 1024)))

(use-package undo-fu-session
  :init
  (setopt undo-fu-session-directory (expand-file-name "undo-fu-session/" user-emacs-directory))
  (unless (file-directory-p undo-fu-session-directory)
    (make-directory undo-fu-session-directory))

  (setopt undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :config
  (undo-fu-session-global-mode 1))

(use-package vundo
  :bind ("C-x u" . vundo))

(use-package winner
  :ensure nil
  :config (winner-mode 1))

(provide 'init-editing)
;;; init-editing.el ends here.
