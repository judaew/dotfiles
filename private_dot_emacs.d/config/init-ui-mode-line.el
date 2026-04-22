;;; init-ui-mode-line.el --- Custom mode-line format and configuration  -*- lexical-binding: t; -*-

;;; Commentary:

;; Customizes the mode-line format with a compact layout including
;; shortened VC branch names, project info, hidden minor modes and etc.

;;; Code:

(require 'cl-lib)

;;
;; File or mode icon
;; -----------------
;;
(defun modeline/file-or-mode-icon ()
  "Return a nerd-icons icon for the current file or major mode."
  (when (featurep 'nerd-icons)
    (let* ((file (buffer-file-name))
           (icon (if file (nerd-icons-icon-for-file file)
                   (nerd-icons-icon-for-mode major-mode))))
      (when (and (stringp icon) (> (length icon) 0))
        (propertize icon
                    'help-echo (format "Major mode: %s" mode-name))))))

;;
;; iedit & macro
;; -------------
;;
(defun modeline/macro-indicator ()
  "Return indicator when a keyboard macro is being recorded."
  (when (or defining-kbd-macro executing-kbd-macro)
    (propertize "● REC" 'face `(:weight bold :foreground "orange"))))

(defun modeline/iedit-indicator ()
  "Return iedit match index/total."
  (when (and (bound-and-true-p iedit-mode)
             iedit-occurrences-overlays)
    (let* ((overlays iedit-occurrences-overlays)
           (total (length overlays))
           (cur-ov (cl-find-if (lambda (ov)
                                 (and (>= (point) (overlay-start ov))
                                      (< (point) (overlay-end ov))))
                               overlays))
           (normal-index (if cur-ov (cl-position cur-ov overlays) -1))
           ;; reverse order for normalize (first is first, last is last)
           (reverse-index (if (>= normal-index 0)
                              (- total normal-index)
                            "-")))
      (propertize (format "[%s/%d]" reverse-index total)
                  'face '(:weight bold :foreground "orange")))))

(defun modeline/matches-info ()
  "Show only macro recording and iedit match info."
  (let ((out (concat
              (or (modeline/macro-indicator) "")
              (or (modeline/iedit-indicator)  ""))))
    (unless (string-empty-p out)
      out)))

;;
;; Shorten buffer name string (alter. for `mode-line-buffer-identification')
;; -------------------------------------------------------------------------
;;
(defface modeline/file-extension-face
  '((t :inherit font-lock-comment-face :weight normal))
  "Face for file extensions in the mode line."
  :group 'modeline)

(defun modeline/buffer-name-smart-truncate ()
  "Return buffer name with smart truncation and colored extension.
Replaces <...> with |..., truncates middle if > 25 chars,
and colors the file extension."
  (let* ((raw-name (buffer-name))
         ;; Change <...> to |..., e.g.: main.py<2> -> main.py|2
         (clean-name
          (let ((start-bracket (string-search "<" raw-name))
                (end-bracket (string-search ">" raw-name)))
            (if (and start-bracket end-bracket (> end-bracket start-bracket))
                (concat (substring raw-name 0 start-bracket)
                        "|"
                        (substring raw-name (1+ start-bracket) end-bracket)
                        (substring raw-name (1+ end-bracket)))
              raw-name)))
         (max-len 25)
         (len (length clean-name))
         (truncated-name
          (if (> len max-len)
              (concat (substring clean-name 0 10)
                      "…"
                      (substring clean-name (- len 13)))
            clean-name))
         (ext-pos (string-match "\\.[^.]+\\'" truncated-name))
         (tooltip (or (buffer-file-name) raw-name)))
    (if ext-pos
        (let* ((base-str (substring truncated-name 0 ext-pos))
               (ext-str (substring truncated-name ext-pos))
               (base-prop (propertize base-str
                                      'face '(:weight bold)
                                      'help-echo tooltip))
               (ext-prop (propertize ext-str
                                     'face 'modeline/file-extension-face
                                     'help-echo tooltip)))
          (concat " " base-prop ext-prop))
      (propertize (concat " " truncated-name)
                  'face '(:weight bold)
                  'help-echo tooltip))))

;;
;; Shorten VC string (from emacs-solo)
;; -----------------------------------
;;
(defun modeline/shorten-vc-mode (vc)
  "Shorten VC string to at most 20 characters.
Replacing `Git-' with a branch symbol."
  (let* ((vc (replace-regexp-in-string "^ Git[:-]"
                                       (if (char-displayable-p ?) "  " "Git: ")
                                       vc))) ;; Options:   ᚠ ⎇
    (if (> (length vc) 20)
        (concat (substring vc 0 20)
                (if (char-displayable-p ?…) "…" "..."))
      vc)))

;;
;; Shorten Flymake and Jinx string
;; -------------------------------
;;
;; e.g.: Flymake[0 4 1] -> f[0 4 1]
(with-eval-after-load 'flymake
  (setq flymake-mode-line-lighter " f"))

;; e.g.: Jinx[en uk] -> s[en ua]
(with-eval-after-load 'jinx
  (let ((entry (assoc 'jinx-mode minor-mode-alist)))
    (when entry
      (setcdr entry '((:eval (when jinx-mode
                               (format " s[%s]" jinx-languages))))))))

;; Formats mode-line
(setq-default mode-line-format
              '("%e" "  "
                ;; (:propertize " " display (raise +0.1)) ;; Top padding
                ;; (:propertize " " display (raise -0.1)) ;; Bottom padding

                ;; ### LEFT ###
                ;; ############
                (:eval (modeline/file-or-mode-icon)) " "
                (:eval (modeline/matches-info)) " "

                ;; e.g.: U:@---
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote

                ;; mode-line-frame-identification
                (:eval (modeline/buffer-name-smart-truncate)) " "
                mode-line-position

                ;; ### RIGHT ###
                ;; #############
                mode-line-format-right-align
                (project-mode-line project-mode-line-format) " "
                (vc-mode (:eval (modeline/shorten-vc-mode vc-mode))) " "
                mode-line-modes
                mode-line-misc-info
                "  ")
              project-mode-line t
              mode-line-buffer-identification '(" %b")
              mode-line-position-column-line-format '("%l:%c")
              mode-line-percent-position '(""))

(setq mode-line-modes-delimiters '("" . ""))  ;; EMACS-31

;; EMACS-31
(setq mode-line-collapse-minor-modes
      '(abbrev-mode
        auto-revert-mode
        apheleia-mode
        buffer-face-mode
        completion-preview-mode
        eldoc-mode
        flyspell-mode
        goggles-mode
        gptel-mode
        org-indent-mode
        outline-minor-mode
        smooth-scroll-mode
        visual-line-mode
        which-key-mode
        ws-butler-mode

        ;; replaced by modeline/matches-info
        defining-kbd-macro
        iedit-mode
        isearch-mode
        ))

(provide 'init-ui-mode-line)
;;; init-ui-mode-line.el ends here
