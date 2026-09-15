;;; init-completion.el --- Completion-at-point -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;; === Backend ==
;; - `cape'              ; completion at point extension
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
;; - `savehist'          ; saving of minibuffer history
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
  :bind ("M-p" . cape-prefix-map)
  :config
  (defun my/setup-capf (&rest capfs)
    "Install CAPFS as local completion-at-point-functions."
    (setq-local completion-at-point-functions capfs))

  (defun my/setup-capf-common-prog ()
    (my/setup-capf
     (cape-capf-super
      #'tempel-complete
      (cape-capf-inside-string #'cape-rfc1345)
      (cape-capf-inside-comment #'cape-rfc1345)
      (cape-capf-inside-string #'cape-dabbrev)
      (cape-capf-inside-comment #'cape-dabbrev)
      #'cape-keyword)))

  (defun my/setup-capf-eglot ()
    (my/setup-capf
     (cape-capf-super
      #'tempel-complete
      #'eglot-completion-at-point
      (cape-capf-inside-string #'cape-dabbrev)
      (cape-capf-inside-comment #'cape-dabbrev))))

  (defun my/setup-capf-elisp ()
    (my/setup-capf
     (cape-capf-super
      #'tempel-complete
      #'cape-elisp-symbol
      (cape-capf-inside-string #'cape-dabbrev)
      (cape-capf-inside-comment #'cape-dabbrev))))

  (defun my/setup-capf-common-text ()
    (my/setup-capf
     (cape-capf-super
      #'tempel-complete
      #'cape-rfc1345
      #'cape-dabbrev
      #'cape-keyword)))

  (defun my/setup-capf-org ()
    (my/setup-capf
     (cape-capf-super
      #'tempel-complete
      #'cape-rfc1345
      #'cape-dabbrev
      #'cape-keyword
      #'cape-elisp-block)))

  (defun my/setup-capf-portfile ()
    (my/setup-capf
     (cape-capf-super
      #'portfile-ts-mode-completion-at-point)))

  :hook
  ((prog-mode . my/setup-capf-common-prog)
   (emacs-lisp-mode . my/setup-capf-elisp)
   (portfile-ts-mode . my/setup-capf-portfile)
   (text-mode . my/setup-capf-common-text)
   (org-mode . my/setup-capf-org)))

(use-package prescient
  :config
  (prescient-persist-mode 1)
  (setopt prescient-aggressive-file-save t))

(use-package corfu-prescient
  :config
  (corfu-prescient-mode 1))

(use-package vertico-prescient
  :config
  (vertico-prescient-mode 1))

(use-package tempel
  :demand t
  :bind
  (("M-+" . tempel-complete)
   (:map tempel-map
         ("TAB" . tempel-next)
         ("[tab]" . tempel-next)
         ("S-TAB" . tempel-prev)
         ("[backtab]" . tempel-prev))))

;; === UI ===
;; ----------

(use-package corfu
  :bind
  (:map corfu-map
        ("<escape>" . corfu-quit))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-cycle t)
  (corfu-auto-prefix 2)
  (corfu-separator ?\s)
  (corfu-echo-documentation 0.25)
  :config
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  (define-key corfu-map (kbd "RET") nil))

(use-package kind-icon
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (plist-put kind-icon-default-style :height 0.8))

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
  (vertico-mode 1)
  (vertico-mouse-mode 1)
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

  ;; Narrow Key, like "<b" (=b=ufers, =f=iles, =r=ecent)
  ;; in consult-buffer and etc.
  (setopt consult-narrow-key "<"))

(use-package consult-flycheck
  :bind ("M-s f" . consult-flycheck))

(use-package marginalia
  :init (marginalia-mode 1))

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
  :config
  (nerd-icons-completion-mode 1)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; Build-in from Emacs 30
(use-package which-key
  :config (which-key-mode 1))

;; === Misc ===
;; ------------

(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

(use-package char-fold
  :ensure nil
  :custom
  (char-fold-symmetric t)
  (search-default-mode #'char-fold-to-regexp))

(use-package reverse-im
  :config (reverse-im-mode 1)
  :custom
  ;; cache generated keymaps
  (reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
  ;; use lax matching
  (reverse-im-char-fold t)
  (reverse-im-read-char-advice-function #'reverse-im-read-char-include)
  ;; translate these methods
  (reverse-im-input-methods '("ukrainian-computer")))

(provide 'init-completion)
;;; init-completion.el ends here
