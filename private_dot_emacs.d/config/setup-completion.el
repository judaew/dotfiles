;;; Package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package emacs
  :custom
  ;; Enable Completion Preview mode in code buffers (#emacs30)
  ;; https://eshelyaron.com/posts/2023-11-17-completion-preview-in-emacs.html
  (global-completion-preview-mode +1))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  ;; (corfu-quit-no-match 'separator)
  ;; (corfu-preselect 'prompt)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  :bind
  (:map corfu-map
	("<escape>" . corfu-quit))
  :init
  (global-corfu-mode))

(use-package emacs
  :straight nil
  :custom
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

(use-package dabbrev
  :straight nil
  :bind (("M-/" . dabbrev-completion)
	 ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex))
  (completion-styles '(orderless partial-completion basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package cape
  :bind ("M-p" . cape-prefix-map)
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package corfu-terminal
  :straight (corfu-terminal
	     :type git
	     :repo "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :init
  (unless (display-graphic-p)
    (corfu-terminal-mode +1)))

(use-package kind-icon
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (plist-put kind-icon-default-style :height 0.8)

  ;; Using VS Code icons
  (setq kind-icon-mapping
      '((array          "a"   :icon "symbol-array"       :face font-lock-type-face              :collection "vscode")
        (boolean        "b"   :icon "symbol-boolean"     :face font-lock-builtin-face           :collection "vscode")
        (color          "#"   :icon "symbol-color"       :face success                          :collection "vscode")
        (command        "cm"  :icon "chevron-right"      :face default                          :collection "vscode")
        (constant       "co"  :icon "symbol-constant"    :face font-lock-constant-face          :collection "vscode")
        (class          "c"   :icon "symbol-class"       :face font-lock-type-face              :collection "vscode")
        (constructor    "cn"  :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
        (enum           "e"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
        (enummember     "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
        (enum-member    "em"  :icon "symbol-enum-member" :face font-lock-builtin-face           :collection "vscode")
        (event          "ev"  :icon "symbol-event"       :face font-lock-warning-face           :collection "vscode")
        (field          "fd"  :icon "symbol-field"       :face font-lock-variable-name-face     :collection "vscode")
        (file           "f"   :icon "symbol-file"        :face font-lock-string-face            :collection "vscode")
        (folder         "d"   :icon "folder"             :face font-lock-doc-face               :collection "vscode")
        (function       "f"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
        (interface      "if"  :icon "symbol-interface"   :face font-lock-type-face              :collection "vscode")
        (keyword        "kw"  :icon "symbol-keyword"     :face font-lock-keyword-face           :collection "vscode")
        (macro          "mc"  :icon "lambda"             :face font-lock-keyword-face)
        (magic          "ma"  :icon "lightbulb-autofix"  :face font-lock-builtin-face           :collection "vscode")
        (method         "m"   :icon "symbol-method"      :face font-lock-function-name-face     :collection "vscode")
        (module         "{"   :icon "file-code-outline"  :face font-lock-preprocessor-face)
        (numeric        "nu"  :icon "symbol-numeric"     :face font-lock-builtin-face           :collection "vscode")
        (operator       "op"  :icon "symbol-operator"    :face font-lock-comment-delimiter-face :collection "vscode")
        (param          "pa"  :icon "gear"               :face default                          :collection "vscode")
        (property       "pr"  :icon "symbol-property"    :face font-lock-variable-name-face     :collection "vscode")
        (reference      "rf"  :icon "library"            :face font-lock-variable-name-face     :collection "vscode")
        (snippet        "S"   :icon "symbol-snippet"     :face font-lock-string-face            :collection "vscode")
        (string         "s"   :icon "symbol-string"      :face font-lock-string-face            :collection "vscode")
        (struct         "%"   :icon "symbol-structure"   :face font-lock-variable-name-face     :collection "vscode")
        (text           "tx"  :icon "symbol-key"         :face font-lock-doc-face               :collection "vscode")
        (typeparameter  "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
        (type-parameter "tp"  :icon "symbol-parameter"   :face font-lock-type-face              :collection "vscode")
        (unit           "u"   :icon "symbol-ruler"       :face font-lock-constant-face          :collection "vscode")
        (value          "v"   :icon "symbol-enum"        :face font-lock-builtin-face           :collection "vscode")
        (variable       "va"  :icon "symbol-variable"    :face font-lock-variable-name-face     :collection "vscode")
        (t              "."   :icon "question"           :face font-lock-warning-face           :collection "vscode"))))

;;; setup-completion.el ends here
