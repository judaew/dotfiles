;;; Package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;; TODOs UI pkg:
;; - hydra
;; - posframe
;; - popup-el
;; - casual https://github.com/kickingvegas/casual

;;; Code:

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-bar-color "#2D2E2E"))

;; Build-in from Emacs 30 (#emacs30)
(use-package which-key
  :straight nil
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package diminish)

(use-package hl-todo
  :hook (after-init . global-hl-todo-mode))

(use-package ligature
  :hook (after-init . global-ligature-mode)
  :config
  (defvar ligatures-VictorMono
    '("</" "</" "/>" "~-" "-~" "~@" "<~" "<~>" "<~~" "~>" "~~" "~~>"
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
  :hook (emacs-lisp-mode . highlight-parentheses-mode))

(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

(use-package indent-bars
  :hook ((c-mode
	  c++-mode
	  go-mode
	  rust-mode
	  python-mode
	  lua-mode
	  yaml-mode) . indent-bars-mode)
  :custom
  (indent-bars-no-descend-lists t) ; no extra bars in continued func arg lists
  (indent-bars-treesit-support t)
  (indent-bars-treesit-ignore-blank-lines-types '("module")))

(use-package colorful-mode
  :defer t
  :hook (css-mode
	 html-mode
	 dhall-mode
	 json-mode
	 yaml-mode))

(use-package emacs
  :straight nil
  :if (display-graphic-p)
  :hook (after-init . pixel-scroll-precision-mode))

;;; setup-ui.el ends here
