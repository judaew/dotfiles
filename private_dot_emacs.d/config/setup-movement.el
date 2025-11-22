;;; setup-movement.el --- Window & navigation -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `windresize' ~ resize windows easily
;; - `winum'      ~ window numbering and navigation
;; - `avy'        ~ jump to visible text quickly

;;; Code:

(use-package windresize
  :bind ("C-c r" . windresize))

(use-package winum
  :hook (after-init . winum-mode)
  :config
  (define-key winum-keymap (kbd "C-`") 'winum-select-window-by-number)
  (define-key winum-keymap (kbd "C-²") 'winum-select-window-by-number)
  (define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)
  (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
  (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
  (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
  (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
  (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
  (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
  (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
  (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
  (define-key winum-keymap (kbd "M-9") 'winum-select-window-9)
  ;; `winum-auto-setup-mode-line' breaks mode-line.el package
  (setopt winum-auto-setup-mode-line nil))


(use-package avy
  :bind
  (("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)
   ("M-g c" . avy-goto-char)
   ("M-g t" . avy-goto-char-timer)))

(provide 'setup-movement)
;;; setup-movement.el ends here
