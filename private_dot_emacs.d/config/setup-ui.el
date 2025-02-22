;;; Package --- Summary

;;; Commentary:

;; TODOs UI pkg:
;; - hydra
;; - posframe
;; - popup-el

;;; Code:

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(use-package which-key
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

;;; setup-ui.el ends here
