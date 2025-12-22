;;; init-ui.el --- UI enhancements -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; === Icons ===
;; `all-the-icons'         ~ file and buffer icons
;; `nerd-the-icons'        ~ alternative icon set

;; === Modeline ===
;; `mood-line'             ~ fancy modeline
;; `diminish'              ~ hide minor modes in modeline

;; === Visual enhancements ===
;; `hl-todo'               ~ highlight TODO/FIXME comments
;; `highlight-parentheses' ~ highlight matching parentheses
;; `goggles'               ~ show changes inline
;; `indent-bars'           ~ display indentation bars
;; `colorful-mode'         ~ add color to buffers
;; `posframe'              ~ pop a posframe at point

;; === Themes ===
;; - `doom-themes' ~ collection of themes

;;; Code:

;; `pixel-scroll-precision-mode'
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (if (display-graphic-p frame)
                  (pixel-scroll-precision-mode 1)
                (pixel-scroll-precision-mode 0)))))

;; === Icons ===
;; -------------

;; Use M-x `all-the-icons-install-fonts' for install fonts.
;; The `window-system' and `display-graphic-p' are bad checks for
;; Emacs with multiples frames or in `daemonp' mode.
(use-package all-the-icons
  :defer t)
(use-package nerd-icons
  :defer t)

;; === Modeline ===
;; ----------------

;; mood-line
(require 'init-modeline)

(use-package diminish)

;; === Visual enhancements ===
;; ---------------------------

(use-package hl-todo
  :hook (after-init . global-hl-todo-mode))

(use-package ligature
  :hook (after-init . global-ligature-mode)
  :config
  (defvar ligatures-VictorMono
    '("</" "/>" "~-" "-~" "~@" "<~" "<~>" "<~~" "~>" "~~" "~~>"
      ">=" "<=" "<!--" "##" "###" "####" "|-" "-|" "|->" "<-|" ">-|"
      "|-<" "|=" "|=>" ">-" "<-" "<--" "-->" "->" "-<" ">->" ">>-"
      "<<-" "<->" "->>" "-<<" "<-<" "==>" "=>" "=/=" "!==" "!=" "<=="
      ">>=" "=>>" ">=>" "<=>" "<=<" "=<=" "=>=" "<<=" "=<<" ".-" ".="
      "=:=" "=!=" "==" "===" "::" ":=" ":>" ":<" ">:" "<:" ";;" "=~"
      "!~" "::<" "<|" "<|>" "|>" "<>" "<$" "<$>" "$>" "<+" "<+>" "+>"
      "?=" "/=" "/==" "__" "/\\" "\\/" "&&" "++" "+++"))
  (ligature-set-ligatures 'prog-mode ligatures-VictorMono))

(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :hook (prog-mode . highlight-parentheses-mode))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package indent-bars
  :hook ((c-ts-mode
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
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module")))

(use-package colorful-mode
  :defer t
  :hook (css-ts-mode
         html-ts-mode
         dhall-mode
         json-ts-mode
         yaml-ts-mode))

(use-package posframe)

;; === Themes ===
;; --------------

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-molokai t)
  (doom-themes-org-config)

  ;; Use italic for comments
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic))

(provide 'init-ui)
;;; init-ui.el ends here
