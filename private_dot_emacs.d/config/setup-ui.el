;;; Package --- Summary

;;; Commentary:

;; TODOs UI pkg:
;; - hydra
;; - posframe
;; - popup-el

;;; Code:

(use-package all-the-icons
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

(use-package highlight-indent-guides
  :disabled
  :diminish highlight-indent-guides-mode
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character))

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

(use-package prism
  :disabled
  :straight (:host github :repo "alphapapa/prism.el")
  :hook (after-init . prism-mode))

;;; setup-ui.el ends here
