;;; init-org.el --- Org mode enhancements -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic keybindings:
;; M-x org-info (opens the manual)
;; C-c C-t      -- cycle TODO state
;; C-c C-c      -- refresh current element (table, code block, checkbox)
;; C-c '        -- edit source block in a separate buffer
;; S-LEFT/RIGHT -- cycle headline (TODO/DONE) or list item state
;; C-c .        -- insert a date (timestamp)

;; Packages:
;; - `org'          ; organize notes, tasks, and documents
;; - `valign'       ; Pixel-perfect visual alignment for Org and Markdown tables
;; - `org-download' ; drag-and-drop images into Org
;; - `org-appear'   ; reveal Org elements contextually
;; - `htmlize'      ; convert buffer to HTML

;; TODO: org-roam, ob-mermaid

;;; Code:

(use-package org
  :bind
  (("C-c o i" . (lambda () (interactive) (find-file "~/org/index.org")))
   ("C-c o c" . org-capture))
  :hook
  ((org-mode . variable-pitch-mode)
   (org-mode . visual-line-mode)
   (org-mode . (lambda () (display-line-numbers-mode -1))))
  :custom
  ;; Protect hidden text edits
  (org-fold-catch-invisible-edits 'show-and-error)

  ;; Smart C-a/C-x
  (org-special-ctrl-a/e t)

  ;; Log done time stamps
  (org-log-done 'time)

  ;; Add blank line before header
  (org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))

  ;; Set paths
  (org-directory "~/org/")
  (org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
  (org-default-notes-file (concat org-directory "notes.org"))
  ;; See
  ;; - https://orgmode.org/manual/Capture-templates.html
  ;; - https://howardism.org/Technical/Emacs/capturing-intro.html
  (org-capture-templates
   '(("t" "Tasks" entry
      (file+headline "~/org/tasks.org" "Inbox")
      "* TODO %?\n%U")
     ("e" "Event" entry
      (file+headline  "~/org/calendar.org" "Events")
      "** %?\n%T")
     ("r" "Recurring Event" entry
      (file+headline "~/org/calendar.org" "Recurring")
      "** %?\n%T")))

  ;; Images
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300))

  ;; Source block
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)

  ;;
  ;; ~~~ Styles & UI ~~~
  ;; ~~~~~~~~~~~~~~~~~~~
  :custom
  ;; setup keywords and their colors
  (org-todo-keywords
   '((sequence "IDEA(i)" "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELED(c)")))
  (org-todo-keyword-faces
   '(("IDEA" . (:foreground "#CCCCCC" :weight bold))
     ("TODO" . (:foreground "#65D9EF" :weight bold))
     ("NEXT" . (:foreground "#E2DB74" :weight bold))
     ("DONE" . (:foreground "#A7E22E" :weight bold))
     ("CANCELED" . (:foreground "#F92572" :weight bold))))
  :config
  ;; setup variable-pitch font
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'bold :height (cdr face)))

  ;; Make the document title a bigger
  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight
                      'bold :height 1.2)

  ;; To avoidline spacing issues
  (require 'org-indent)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))

  ;; Set some parts of Org document is always use fixed-pitch
  (set-face-attribute 'org-block nil           :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil            :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil          :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil        :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face
                                                          fixed-pitch))
  (set-face-attribute 'org-meta-line nil       :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil        :inherit 'fixed-pitch)

  (set-face-attribute 'org-table nil           :inherit 'fixed-pitch)
  (set-face-attribute 'org-table-header nil    :inherit 'fixed-pitch :weight 'bold)

  ;; Text Prettification
  (setq org-startup-indented t
        org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-pretty-entities t))

;; Disable `visual-line-mode' in org tables
(use-package org
  :config
  (defun my-org-table-disable-wrap ()
    "Disable `visual-line-mode' in org tables."
    (if (org-at-table-p)
        (when visual-line-mode
          (visual-line-mode -1))
      (unless visual-line-mode
        (visual-line-mode +1))))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'post-command-hook 'my-org-table-disable-wrap nil t))))

(use-package valign
  :hook (org-mode . valign-mode)
  :custom (valign-lighter t))

(use-package org-download
  :after org
  :hook (dired-mode . org-download-enable))

(use-package org-appear
  :after org
  :hook
  (org-mode . org-appear-mode))

(use-package htmlize
  :defer t)

(provide 'init-org)
;;; init-org.el ends here
