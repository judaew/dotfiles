;;; init-completion.el --- Completion-at-point -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; === Backend ==
;; - `cape'              ; completion at point extension
;; - `dabbrev'           ; dynamic abbrev
;; - `prescient'         ; better sorting and filtering
;; - `corfu-prescient'   ; prescient integration for corfu
;; - `vertico-prescient' ; prescient integration for vertico
;; - `tempel'            ; tmpl/snippet expansion

;; === UI ===
;; - `corfu'             ; completion UI
;; - `kind-icon'         ; icons for completion kinds

;; === Minibuffer ===
;; - `vertico'           ; vertical completion UI
;; - `consult'           ; search and navigation enhanced commands
;; - `consult-flycheck'  ; flycheck integration for consult
;; - `marginalia',       ; rich annotations (M-x)
;; - `embark'            ; contextual actions
;; - `embark-consult'    ; embark integration for consult
;; - `nerd-icons-completion' ; nerd-icons in vertico + marginalia
;; - `which-key'         ; show keybindings

;; === Misc ===
;; - `char-fold'         ; flexible character folding
;; - `reverse-im'        ; reverse input method support

;;; Code:

;; === Backend ===
;; ---------------

;; Use TAB for indentation and completion
(setopt tab-always-indent 'complete)
;; Disable Ispell in text completion for speed
;; And use `cape-dict' and `cape-abbrev' as an alternative
(setopt text-mode-ispell-word-completion nil)

(use-package cape
  :after prescient
  :bind ("M-p" . cape-prefix-map)
  :config
  (defun my/capf-super-unsorted (&rest capfs)
    "Wrapper around `cape-capf-super' that disables sorting of completion candidates."
    (let ((capf (apply #'cape-capf-super capfs)))
      (cape-capf-properties
       capf
       :display-sort-function nil
       :cycle-sort-function nil)))

  (defun my/setup-capf (functions)
    "Setup `completion-at-point-functions' with given FUNCTIONS list."
    (setq-local completion-at-point-functions
                (mapcar (lambda (f) (if (function f) f (symbol-function f)))
                        functions)))

  (defconst my/capf-common-prog
    (list
     #'tempel-complete
     (cape-capf-inside-string #'cape-rfc1345)
     (cape-capf-inside-comment #'cape-rfc1345)
     (cape-capf-inside-string #'cape-dabbrev)
     (cape-capf-inside-comment #'cape-dabbrev)
     #'cape-keyword)
    "Completion list for prog-based modes (non-LSP).")
  (defun my/setup-capf-common-prog ()
    (my/setup-capf my/capf-common-prog))

  (defconst my/capf-eglot
    (list
     #'tempel-complete
     #'eglot-completion-at-point
     (cape-capf-inside-string #'cape-dabbrev)
     (cape-capf-inside-comment #'cape-dabbrev))
    "Completion list for eglot-based modes.")
  (defun my/setup-capf-eglot ()
    (my/setup-capf my/capf-eglot))

  (defconst my/capf-elisp
    (list
     #'tempel-complete
     #'cape-elisp-symbol
     (cape-capf-inside-string #'cape-dabbrev)
     (cape-capf-inside-comment #'cape-dabbrev))
    "Completion list for `emacs-lisp-mode'")
  (defun my/setup-capf-elisp ()
    (my/setup-capf my/capf-elisp))

  (defconst my/capf-common-text
    (list
     #'tempel-complete
     #'cape-rfc1345
     #'cape-dabbrev
     #'cape-keyword)
    "Completion list for text-based modes.")
  (defun my/setup-capf-common-text ()
    (my/setup-capf my/capf-common-text))

  (defun my/setup-capf-org ()
    (my/setup-capf
     (cons #'cape-elisp-block my/capf-common-text)))
  :hook
  ((prog-mode . my/setup-capf-common-prog)
   (emacs-lisp-mode . my/setup-capf-elisp)
   (text-mode . my/setup-capf-common-text)
   (org-mode . my/setup-capf-org)))

(use-package dabbrev
  :straight nil
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` "))

(use-package prescient
  :config
  (prescient-persist-mode 1)
  (setopt prescient-aggressive-file-save t))

(use-package corfu-prescient
  :after corfu prescient
  :config
  (corfu-prescient-mode 1))

(use-package vertico-prescient
  :after vertico prescient
  :config
  (vertico-prescient-mode 1)

  ;; Disable prescient sorting for file completions (e.g. C-x C-f)
  ;; to preserve natural filesystem order and allow creating new files
  ;; like "test2.go" even if "test.go" exists.
  ;; See https://www.reddit.com/r/emacs/comments/109ryp7/comment/j4excag
  (setf vertico-prescient-completion-category-overrides
        (assoc-delete-all 'file
                          vertico-prescient-completion-category-overrides)))

(use-package tempel
  :after cape
  :bind
  (("M-+" . tempel-complete)
   (:map tempel-map
         ("TAB" . tempel-next)
         ("[tab]" . tempel-next)
         ("S-TAB" . tempel-prev)
         ("[backtab]" . tempel-prev))))

;; === UI ===
;; ----------

;; Enable Completion Preview mode in code buffers (#emacs30)
;; https://eshelyaron.com/posts/2023-11-17-completion-preview-in-emacs.html
(global-completion-preview-mode +1)

(use-package corfu
  :bind
  (:map corfu-map
        ("<escape>" . corfu-quit))
  :hook (after-init . global-corfu-mode)
  :init
  (corfu-popupinfo-mode +1)
  (corfu-history-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-separator ?\s)
  (corfu-echo-documentation 0.25)
  :config
  (define-key corfu-map (kbd "RET") nil))

(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (plist-put kind-icon-default-style :height 0.8)

  ;; Using VS Code icons
  (setopt kind-icon-mapping
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

;; === Minibuffer ===
;; ------------------

;;Allow recursive minibuffers
(setopt enable-recursive-minibuffers t)
;; Hide invalid commands in M-x
(setopt read-extended-command-predicate #'command-completion-default-include-p)
;; Do not allow the cursor in the minibuffer prompt
(setopt minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

(use-package vertico
  :init
  (vertico-mode)
  (vertico-mouse-mode)
  :bind (:map vertico-map ("M-R" . vertico-repeat))
  :custom
  (vertico-cycle t))

(use-package consult
  :bind (;; C-c bindings in ""`mode-specific-map'
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-fd)
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)   ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history) ;; orig. isearch-edit-string
         ("M-s l" . consult-line)            ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)      ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)           ;; orig. next-matching-history-element
         ("M-r" . consult-history))          ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  ;; Better register preview
  (advice-add #'register-preview :override #'consult-register-window)
  (setopt register-preview-delay 0.5)
  ;; Xref integration
  (setopt xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

  ;; preview customizations
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))

  (setopt consult-narrow-key "<"))

(use-package consult-flycheck
  :after (consult flycheck)
  :bind ("M-g f" . consult-flycheck))

(use-package marginalia
  :init (marginalia-mode))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setopt prefix-help-command #'embark-prefix-help-command)

  ;; TODO: It's cool to show hints in Eldoc, but I think it's better
  ;; to display in the modeline. However, firsth I need move away from
  ;; doom-modeline and create myself solutin Maybe add a marker like
  ;; Act: 1 or with icon.
  ;;
  ;;(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;;(setopt eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Icons for vartico + marginalia
(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Build-in from Emacs 30
(use-package which-key
  :hook (after-init . which-key-mode))

;; === Misc ===
;; ------------

(use-package char-fold
  :custom
  (char-fold-symmetric t)
  (search-default-mode #'char-fold-to-regexp))

(use-package reverse-im
  :bind ("M-T" . reverse-im-translate-word)
  :hook (after-init . reverse-im-mode)
  :custom
  ;; cache generated keymaps
  (reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
  ;; use lax matching
  (reverse-im-char-fold t)
  (reverse-im-read-char-advice-function #'reverse-im-read-char-include)
  ;; translate these methods
  (reverse-im-input-methods '("ukrainian-computer" "russian-computer")))

(provide 'init-completion)
;;; init-completion.el ends here
