;;; Package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

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
   ("C-c c" . org-capture))
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
      "** %?\n%T"))))

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

;;; setup-org.el ends here
