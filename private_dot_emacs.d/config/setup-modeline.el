;;; setup-modeline.el --- modeline enhancements -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; `mood-line' ~ fancy modeline

;;; Code:

(defun my/mood-line-segment-window-number ()
  "Show the winum window number, or nil if winum is inactive."
  (when (and (featurep 'winum)
             (fboundp 'winum-get-number-string))
    (winum-get-number-string)))

(use-package mood-line
  :hook (after-init . mood-line-mode)
  :config
  (setopt mood-line-glyph-alist mood-line-glyphs-fira-code)
  (setq mood-line-format
        ;; Value:
        ;; ((#1=" " (mood-line-segment-modal) " "
        ;;      (or (mood-line-segment-buffer-status) " ") " "
        ;;      (mood-line-segment-buffer-name) "  " (mood-line-segment-anzu)
        ;;      "  " (mood-line-segment-multiple-cursors) "  "
        ;;      (mood-line-segment-cursor-position) " "
        ;;      (mood-line-segment-scroll) "")
        ;;  ((mood-line-segment-vc) "  " (mood-line-segment-major-mode) "  "
        ;;   (mood-line-segment-misc-info) "  " (mood-line-segment-checker) "  "
        ;;   (mood-line-segment-process) "  " #1#))

        (mood-line-defformat
         :left
         (((my/mood-line-segment-window-number) . " ")
          ((mood-line-segment-buffer-status) . " ")
          ((mood-line-segment-buffer-name)   . "  ")
          ((mood-line-segment-cursor-position) . "  ")
          (mood-line-segment-scroll))
         :right
         (((mood-line-segment-major-mode) . " " )
          ;;((when (mood-line-segment-checker) "|") . "  ")
          (mood-line-segment-checker))))
  )

(provide 'setup-modeline)
;;; setup-modeline.el ends here
