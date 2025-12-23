;;; init-editing.el --- Editing and navigation -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; === Editing ===
;; - `smart-hungry-delete' ; smart hungry delete
;; - `ws-butler'           ; trim whitespace on save
;; - `iedit'               ; edit multiple regions
;; - `saveplace'           ; remember cursor position
;; - `editorconfig'        ; .editorconfig support
;; - `expreg'              ; expand-region using Tree-Sitter

;; === Movement and navigation ===
;; - `ibuffer'
;; -
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

(use-package smart-hungry-delete
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
         ([remap delete-backward-char] . smart-hungry-delete-backward-char)
         ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode)
  :config
  (setopt ws-butler-keep-whitespace-before-point t))

(use-package iedit)

(use-package saveplace
  :straight nil
  :hook (after-init . save-place-mode))

(use-package editorconfig
  :straight nil
  :hook (after-init . editorconfig-mode))

(use-package expreg
  :bind ("C-=" . expreg-expand))

;; === Movement and navigation ===
;; -------------------------------

;; (use-package all-the-icons-ibuffer
;;   :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package ibuffer
  :bind ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-expert t)
  (ibuffer-display-summary nil)
  ;;(ibuffer-use-other-window nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-use-header-line t)
  ;;(ibuffer-default-shrink-to-minimum-size nil)
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-formats
   '((mark modified read-only locked " "
           (name 18 18 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " project-file-relative))))

;; See https://github.com/muffinmad/emacs-ibuffer-project
(use-package ibuffer-project
  :config
  (add-hook
   'ibuffer-hook
   (lambda ()
     (setq ibuffer-filter-groups (ibuffer-project-generate-filter-groups))
     (unless (eq ibuffer-sorting-mode 'project-file-relative)
       (ibuffer-do-sort-by-project-file-relative)))))

(use-package ace-window
  :demand t ;; for modeline
  :bind ("M-o" . ace-window))

(use-package avy
  :bind
  (("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)
   ("M-g c" . avy-goto-char)
   ("M-g t" . avy-goto-char-timer)))

(use-package windresize
  :bind ("C-c r" . windresize))

;; For a more ergonomic Emacs and `dape' experience
;; See https://www.gnu.org/software/emacs/manual/html_node/emacs/Repeating.html
;; like C-x-left-left-left-right and etc
(use-package repeat
  :custom
  (repeat-mode +1))

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
  :hook (after-init . undo-fu-session-global-mode)
  :init
  (setopt undo-fu-session-directory (expand-file-name "undo-fu-session/" user-emacs-directory))
  (unless (file-directory-p undo-fu-session-directory)
    (make-directory undo-fu-session-directory))

  (setopt undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package vundo
  :bind ("C-x u" . vundo))

(use-package winner
  :straight nil
  :hook (after-init . winner-mode))

(provide 'init-editing)
;;; init-editing.el ends here.
