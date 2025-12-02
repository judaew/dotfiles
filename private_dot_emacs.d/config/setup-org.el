;;; setup-org.el --- Org mode enhancements -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `org'          ~ organize notes, tasks, and documents
;; - `org-download' ~ drag-and-drop images into Org
;; - `org-modern'   ~ modern visual style for Org
;; - `org-appear'   ~ reveal Org elements contextually

;;; Code:

(defun open-index-org-file ()
  "Open the ~/org/index.org file."
  (interactive)

  (unless (file-exists-p "~/org/index.org")
    (make-directory "~/org" t)
    (write-region "" nil "~/org/index.org"))

  (find-file "~/org/index.org"))

(defun org-visual-mode ()
  "Enable `visual-line-mode' only for `org-mode'."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (visual-line-mode t)))

(add-hook 'org-mode-hook #'org-visual-mode)

(use-package org
  :bind
  (("C-c o i" . open-index-org-file)
   ("C-c o c" . org-capture))
  :custom
  (org-log-done 'time)
  (org-startup-indented t)
  ;; Add blank line before header
  (org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))

  (org-hide-emphasis-markers t)

  (org-todo-keywords
   '((sequence "IDEA(i)" "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELED(c)")))
  (org-todo-keyword-faces
   '(("IDEA" . (:foreground "#CCCCCC" :weight bold))
     ("TODO" . (:foreground "#65D9EF" :weight bold))
     ("NEXT" . (:foreground "#E2DB74" :weight bold))
     ("DONE" . (:foreground "#A7E22E" :weight bold))
     ("CANCELED" . (:foreground "#F92572" :weight bold))))

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

  ;; TODO: 2025 август
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300))
  (org-agenda-start-with-log-mode t)
  (org-log-into-drawer t)
  )

(use-package org-download
  :after org
  :hook (dired-mode . org-download-enable))

(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config (set-face-attribute 'org-modern-symbol nil :family "Iosevka"))

(use-package org-appear
  :after org
  :hook
  ((org-mode . org-appear-mode)
   (org-agenda-finalize . org-modern-agenda)))

;; TODO:
;; (use-package org-crypt
;;   :autoload org-crypt-use-before-save-magic)

(use-package org
  :config
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.15)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Noto Sans" :weight 'bold :height (cdr face)))

  ;; Make the document title a bigger
  (set-face-attribute 'org-document-title nil :font "Noto Sans" :weight
                      'bold :height 1.2)

  ;; To avoidline spacing issues
  (require 'org-indent)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))

  ;; Set some parts of Org document is always use fixed-pitch
  (set-face-attribute 'org-block nil            :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil             :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil           :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil         :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil  :inherit '(font-lock-comment-face
                                                           fixed-pitch))
  (set-face-attribute 'org-meta-line nil        :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil         :inherit 'fixed-pitch)

  ;; Enable `variable-pitch-mode'
  (add-hook 'org-mode-hook 'variable-pitch-mode)

  ;; Decluttering & Text Prettification
  (setq org-adapt-indentation t
        org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-pretty-entities t)

  (setq org-src-fontify-natively t
	org-src-tab-acts-natively t
        org-edit-src-content-indentation 0)

  (add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
  )

(provide 'setup-org)
;;; setup-org.el ends here
