;;; init-ui.el --- UI enhancements -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; === Icons ===
;; `nerd-icons'        ; alternative icon set

;; === Visual enhancements ===
;; `ligature'              ; show typographical ligatures
;; `goggles'               ; show changes inline
;; `indent-bars'           ; display indentation bars
;; `colorful-mode'         ; add color to buffers
;; `posframe'              ; pop a posframe at point

;; === Themes ===
;; - `ronny'

;;; Code:

(use-package emacs
  :straight nil
  :hook (after-init . pixel-scroll-precision-mode)
  :custom (pixel-scroll-precision-interpolation-factor 1.0))

;; === Icons ===
;; -------------

;; The `window-system' and `display-graphic-p' are bad checks for
;; Emacs with multiples frames or in `daemonp' mode.
(use-package nerd-icons
  :defer t)

;; === Visual enhancements ===
;; ---------------------------

(use-package ligature
  :hook (after-init . global-ligature-mode)
  :config
  (defvar ligatures-Iosevka
    '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->"
      "<---->" "<!--" "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">="
      "<=>" "<==>" "<===>" "<====>" "<!---" "<~~" "<~" "~>" "~~>" "::"
      ":::" "==" "!=" "===" "!==" ":=" ":-" ":+" "<*" "<*>" "*>" "<|"
      "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  (ligature-set-ligatures 'prog-mode ligatures-Iosevka))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package indent-bars
  :hook ((bash-ts-mode
          c-ts-mode
          c++-ts-mode
          csharp-ts-mode
          css-ts-mode
          go-ts-mode
          java-ts-mode
          javascript-ts-mode
          lua-ts-mode
          rust-ts-mode
          python-ts-mode
          ruby-ts-mode
          tsx-ts-mode
          typescript-ts-mode
          yaml-ts-mode) . indent-bars-mode)
  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t))

(use-package colorful-mode
  :defer t
  :hook (css-ts-mode
         html-ts-mode
         json-ts-mode
         yaml-ts-mode))

(use-package posframe)

;; === Themes ===
;; --------------

(use-package ronny-theme
  ;; :straight (:host github :repo "judaew/ronny.el" )
  :straight (ronny-theme :type git :local-repo "~/wrk/github.com/judaew/ronny.el")
  :config
  (load-theme 'ronny t))

(provide 'init-ui)
;;; init-ui.el ends here
