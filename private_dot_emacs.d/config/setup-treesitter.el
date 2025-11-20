;;; setup-treesitter.el --- Tree-Sitter in Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:

;;; Code:

(setq major-mode-remap-alist
      '((sh-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (cmake-mode . cmake-ts-mode)
        (csharp-mode . csharp-ts-mode)
        (css-mode . css-ts-mode)
        (html-mode . html-ts-mode)
        (java-mode . java-ts-mode)
        (js-mode . js-ts-mode)
        (json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (lua-mode . lua-ts-mode)
        (markdown-mode . markdown-ts-mode)
        (ruby-mode . ruby-ts-mode)
        (conf-toml-mode . toml-ts-mode)
        (yaml-mode . yaml-ts-mode)))

(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-ts-mode))
;; See `setup-docker.el'
;; (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-ts-mode))
;; (add-to-list 'auto-mode-alist '("Containerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixit-ts-mode))
;; See `setup-lang.el' -> go-mode
;; (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
;; (add-to-list 'auto-mode-alist '("go.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("\\.heex\\'" . heex-ts-mode))
;; `php-ts-mode' don't work correct
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(provide 'setup-treesitter)
;;; setup-treesitter.el ends here
