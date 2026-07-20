;;; init-org.el --- Org mode enhancements -*- lexical-binding: t; -*-

;;; Commentary:

;; Basic keybindings:
;; M-x org-info (opens the manual)
;; C-c C-t      -- cycle TODO state
;; C-c C-c      -- refresh current element (table, code block, checkbox)
;; C-c '        -- edit source block in a separate buffer
;; S-LEFT/RIGHT -- cycle headline (TODO/DONE) or list item state
;; C-c .        -- insert a date (timestamp)
;; C-c C-w      -- refile
;; C-c C-s      -- org schedule
;; C-c C-,      -- org select

;; Packages:
;; - `org'          ; organize notes, tasks, and documents
;; - `valign'       ; Pixel-perfect visual alignment for Org and Markdown tables
;; - `org-download' ; drag-and-drop images into Org
;; - `org-appear'   ; reveal Org elements contextually
;; - `htmlize'      ; convert buffer to HTML

;; TODO: org-journal, org-roam, ob-mermaid

;;; Code:

(use-package org
  :bind
  (("C-c o i" . (lambda () (interactive) (find-file org-directory)))
   ("C-c o a" . org-agenda)
   ("C-c o c" . org-capture))
  :hook
  ((org-mode . variable-pitch-mode))
  :custom
  (org-fold-catch-invisible-edits 'show-and-error) ; Protect hidden text edits
  (org-special-ctrl-a/e t) ; Smart C-a/C-x
  (org-log-done 'time) ; Log done time stamps
  ;; Add blank line before header
  (org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))

  (org-use-speed-commands t) ; Fast commands when cursor in *
  (org-return-follows-link t) ; Open links by RET

  ;; Set paths
  (org-directory (expand-file-name "~/org/"))
  (org-agenda-files '("~/org/inbox.org"
                      "~/org/agenda.org"
                      "~/org/projects.org"))

  ;; refile
  (org-refile-targets
   '((org-agenda-files :maxlevel . 2)
     ("~/org/someday.org" :maxlevel . 1)))
  (org-refile-use-outline-path 'file)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes 'confirm)

  ;; See
  ;; - https://orgmode.org/manual/Capture-templates.html
  ;; - https://howardism.org/Technical/Emacs/capturing-intro.html
  (org-capture-templates
   '(("t" "Task" entry (file "~/org/inbox.org")
      "* TODO %?\n%U" :empty-lines 1)
     ("n" "Note" entry (file "~/org/inbox.org")
      "* %?\n%U" :empty-lines 1)))

  ;; Images & source block
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300))
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-edit-src-content-indentation 0)

  ;;
  ;; ~~~ Styles & UI ~~~
  ;; ~~~~~~~~~~~~~~~~~~~
  :custom
  ;; setup keywords and their colors
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELED(c)")))
  (org-todo-keyword-faces
   '(("TODO" . (:foreground "#65D9EF" :weight bold))
     ("NEXT" . (:foreground "#E2DB74" :weight bold))
     ("DONE" . (:foreground "#A7E22E" :weight bold))
     ("CANCELLED" . (:foreground "#F92572" :weight bold))))

  ;; Use headings as TOC (table of content)
  (org-startup-folded 'content)

  ;; Text Prettification
  (org-startup-indented t)
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  :config
  ;; Setup variable-pitch font
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
  (set-face-attribute 'org-table-header nil    :inherit 'fixed-pitch :weight 'bold))

(use-package valign
  :hook (org-mode . valign-mode)
  :custom (valign-lighter t))

(use-package org-download
  :after org
  :hook (dired-mode . org-download-enable))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autosubmarkers t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t))

(use-package htmlize
  :defer t)

(provide 'init-org)
;;; init-org.el ends here
