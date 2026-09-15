;;; init-ui.el --- UI enhancements -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; === Interface ===
;; `custom-css'            ; style Emacs's GTK widgets with custom CSS
;; `ondemand-scroll-bar'   ; show scroll bars on demand in Emacs

;; === Icons ===
;; `nerd-icons'            ; alternative icon set

;; === Visual enhancements ===
;; `hl-todo'               ; highlight TODO keywords
;; `ligature'              ; show typographical ligatures
;; `goggles'               ; show changes inline
;; `indent-bars'           ; display indentation bars
;; `colorful-mode'         ; add color to buffers
;; `posframe'              ; pop a posframe at point

;; === Folding ===
;; ---------------
;; `hs-minor-mode'         ; minor mode to selectively hide/show code and comment
;; `kirigami'              ; a unified method to fold and unfold text

;; === Themes ===
;; - `ronny'

;;; Code:

(setopt pixel-scroll-precision-mode t)
(setopt pixel-scroll-precision-interpolation-factor 1.0)

;; === Icons ===
;; -------------

;; The `window-system' and `display-graphic-p' are bad checks for
;; Emacs with multiples frames or in `daemonp' mode.
(use-package nerd-icons)

;; === Visual enhancements ===
;; ---------------------------

(use-package custom-css
  :ensure nil ;; nix
  :config
  (setopt custom-css-scroll-bar-mode t))

(use-package on-demand-scroll-bar
  :ensure (:host github :repo "florommel/on-demand-scroll-bar")
  :config
  (setopt on-demand-scroll-bar-mode t))

(use-package hl-todo
  :config (global-hl-todo-mode 1))

(use-package ligature
  :config
  (global-ligature-mode 1)
  (ligature-set-ligatures ;; Iosevka
   'prog-mode
   '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->"
     "<---->" "<!--" "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">="
     "<=>" "<==>" "<===>" "<====>" "<!---" "<~~" "<~" "~>" "~~>" "::"
     ":::" "==" "!=" "===" "!==" ":=" ":-" ":+" "<*" "<*>" "*>" "<|"
     "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++")))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :custom (goggles-pulse t))

(use-package indent-bars
  :hook
  ((prog-mode . indent-bars-mode)
   (emacs-lisp-mode . (lambda () (indent-bars-mode -1))))
  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t))

(use-package colorful-mode
  :hook (css-ts-mode
         html-ts-mode
         json-ts-mode
         yaml-ts-mode))

(use-package posframe
  :defer t)

;; === Folding ===
;; ---------------

(add-hook 'prog-mode-hook #'hs-minor-mode)

(use-package kirigami
  :init
  (kirigami-global-mode 1)
  :custom
  (kirigami-show-menu-bar t)
  (kirigami-show-context-menu t)

  (push
   '((hs-minor-mode)
     :open-all    hs-show-all
     :close-all   hs-hide-all
     :toggle      hs-toggle-hiding
     :open        hs-show-block
     :open-rec    nil
     :close       hs-hide-block)
   kirigami-fold-list)
  :bind
  (("C-c z o" . kirigami-open-fold)
   ("C-c z O" . kirigami-open-fold-rec)
   ("C-c z r" . kirigami-open-folds)
   ("C-c z c" . kirigami-close-fold)
   ("C-c z m" . kirigami-close-folds)
   ("C-c z a" . kirigami-toggle-fold)))

;; === Themes ===
;; --------------

(use-package ronny-theme
  :ensure nil
  :if (file-directory-p "~/wrk/github.com/judaew/ronny.el/")
  :load-path "~/wrk/github.com/judaew/ronny.el/"
  :config (load-theme 'ronny t))

(provide 'init-ui)
;;; init-ui.el ends here
