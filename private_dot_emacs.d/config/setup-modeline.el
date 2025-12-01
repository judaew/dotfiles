;;; setup-modeline.el --- modeline enhancements -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; `mood-line' ~ fancy modeline

;;; Code:

;;
;; Window number
;;
(defun my/aw-get-window-number ()
  "Return the `ace-window' number (label) for the selected window."
  (when (featurep 'ace-window)
    (require 'ace-window)
    (let ((ignore-window-parameters t))
      (avy-traverse
       (avy-tree (aw-window-list) aw-keys)
       (lambda (path leaf)
         (set-window-parameter
          leaf 'ace-window-path
          (apply #'string (reverse path))))))
    (window-parameter (selected-window) 'ace-window-path)))

(defun my/mood-line-segment-ace-window-number ()
  "Mood-line segment: `ace-window' number."
  (when-let* ((num (my/aw-get-window-number)))
    (when (> (length num) 0)
      (propertize (format "%s" num)
                  'face '(:weight bold :foreground "orange")))))

;;
;; File or mode icon
;;
(defun my/mood-line-file-or-mode-icon ()
  "Return a nerd-icons icon for the current file or major mode."
  (when (featurep 'nerd-icons)
    (let* ((file (buffer-file-name))
           (icon (if file
                     (nerd-icons-icon-for-file file)
                   (nerd-icons-icon-for-mode major-mode))))

      (when (and icon (not (string-empty-p icon)))
        (propertize icon
                    'face (list (get-text-property 0 'face icon)
                                '(:family "Symbols Nerd Font Mono" :height 1.0))
                    'help-echo (format "Major mode: %s" mode-name))))))

;;
;; iedit & macro
;;
(defun my/mood-line-segment-macro ()
  "Return indicator when a keyboard macro is being recorded."
  (when (or defining-kbd-macro executing-kbd-macro)
    (propertize "● REC"
                'face '(:weight bold :foreground "orange"))))

(defun my/mood-line-segment-iedit ()
  "Return iedit match index/total."
  (when (and (bound-and-true-p iedit-mode)
             (bound-and-true-p iedit-occurrences-overlays))
    (let* ((ovs (sort (copy-sequence iedit-occurrences-overlays)
                      (lambda (a b)
                        (< (overlay-start a) (overlay-start b)))))
           (cur (iedit-find-current-occurrence-overlay))
           (idx (if cur (1+ (cl-position cur ovs)) "-"))
           (total (length ovs)))
      (propertize (format "[%s/%d]" idx total)
                  'face '(:weight bold :foreground "orange")))))

(defun my/mood-line-segment-matches ()
  "Show only macro recording and iedit match info."
  (let ((out (concat
              (or (my/mood-line-segment-macro) "")
              (or (my/mood-line-segment-iedit)  ""))))
    (unless (string-empty-p out)
      out)))

;;
;; Project
;;
(defvar-local my/project--cached-name nil
  "Cached project name.")

(defun my/project-name ()
  "Return project name using project.el."
  (or my/project--cached-name
      (setq my/project--cached-name
            (let ((proj (project-current)))
              (if proj (format "[%s]"
                          (file-name-nondirectory
                           (directory-file-name
                            (car (project-roots proj))))) "")))))

(defun my/mood-line-segment-project ()
  "Mood-line segment: project name."
  (let ((name (my/project-name)))
    (when (and name (not (string-empty-p name)))
      (propertize name
                  'face '(:slant italic :foreground "gray40")
                  'local-map project-prefix-map))))

;;
;; Input method
;;
(defun my/get-current-input-method ()
  "Return current input method title or nil."
  (cond
   ;; Normal input-method
   (current-input-method
    current-input-method-title)
   (t nil)))

(defun my/mood-line-segment-input-method ()
  "Show current input method in mood-line."
  (when-let* ((im (my/get-current-input-method)))
    (propertize (format "%s" im)
                'face '(:foreground "orange")
                'help-echo (format "Current input method: %s\nmouse-2 disable\nmouse-3 describe" current-input-method)
                'mouse-face 'mode-line-highlight
                'local-map mode-line-input-method-map)))

;;
;; Info
;;
(defun my/mood-line-segment-info ()
  "Show Info topic and node in the mode-line when in Info buffer."
  (when (derived-mode-p 'Info-mode)
    (concat
     "("
     ;; Topic (file name)
     (propertize (if (stringp Info-current-file)
                     (file-name-sans-extension
                      (file-name-nondirectory Info-current-file))
                   (format "*%S*" Info-current-file))
                 'face '(:weight bold :foreground "DeepSkyBlue"))
     ") "
     ;; Node
     (when Info-current-node
       (propertize Info-current-node
                   'face '(:foreground "LightSteelBlue")
                   'help-echo "mouse-1: scroll forward, mouse-3: scroll back"
                   'mouse-face 'mode-line-highlight
                   'local-map Info-mode-line-node-keymap)))))

;;
;; Eglot
;;
(defun my/mood-line-segment-eglot ()
  "Show plain \"LSP:on\" string when Eglot is managing this buffer.
Return nil otherwise so mood-line hides the segment cleanly."
  (when (and (featurep 'eglot)
             (bound-and-true-p eglot--managed-mode)
             (ignore-errors (eglot-current-server)))
    (propertize "LSP:on" 'face '(:weight bold :foreground "orange"))))

;;
;; Mood line
;;

(use-package mood-line
  :hook (after-init . mood-line-mode)
  :config
  (setopt mood-line-glyph-alist mood-line-glyphs-fira-code)
  (setq mood-line-format
        (mood-line-defformat
         :left
         (((my/mood-line-segment-ace-window-number) . "  ")
          ((my/mood-line-segment-matches) . " ")
          ((my/mood-line-file-or-mode-icon) . " ")
          ((mood-line-segment-buffer-status) . " ")
          ((mood-line-segment-buffer-name)   . " ")
          ((my/mood-line-segment-info) . " ")
          ((mood-line-segment-cursor-position) . " ")
          (mood-line-segment-scroll))
         :right
         (((my/mood-line-segment-project) . "  ")
          ((my/mood-line-segment-input-method) . " ")
          ((my/mood-line-segment-eglot) . " ")
          ((mood-line-segment-major-mode) . " " )
          ((mood-line-segment-vc) . " ")
          (mood-line-segment-checker)))))

(provide 'setup-modeline)
;;; setup-modeline.el ends here
