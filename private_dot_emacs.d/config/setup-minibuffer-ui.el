;;; setup-minibuffer-ui.el --- Minibuffer & completion UI -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `char-fold'        ~ flexible character folding
;; - `reverse-im'       ~ reverse input method support
;; - `which-key'        ~ show available keybindings
;; - `vertico'          ~ vertical completion UI
;; - `consult'          ~ search and navigation enhanced commands with Vertico
;; - `consult-flycheck' ~ flycheck integration for consult
;; - `marginalia',      ~ rich annotations for M-x completion
;; - `embark'           ~ contextual action framework
;; - `embark-consult'   ~ embark integration for consult
;; - `nerd-icons-completion' ~ icons completion in vertico + marginalia

;;; Code:

(require 'cl-lib)

(use-package emacs
  :custom
  ;;Allow recursive minibuffers
  (enable-recursive-minibuffers t)
  ;; Hide invalid commands in M-x
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

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

;; Build-in from Emacs 30 (#emacs30)
(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

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

  (setopt consult-narrow-key "<")

  (defun my/consult-buffer-list ()
    "Return list of buffers excluding system and temporary buffers."
    (cl-remove-if (lambda (buf)
                    (string-match-p "\\*\\(straight-process\\|Async-native-compile-log\\|direnv\\)\\*"
                                    (buffer-name buf)))
                  (buffer-list)))

  (setopt consult-buffer-list-function #'my/consult-buffer-list))

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

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(provide 'setup-minibuffer-ui)
;;; setup-minibuffer-ui.el ends here
