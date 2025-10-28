;;; setup-treesitter.el --- Tree-Sitter in Emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; When updating, the correct parser hint can be found in the `treesit-auto' package

;;; Code:

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(c "https://github.com/tree-sitter/tree-sitter-c")
	(cpp "https://github.com/tree-sitter/tree-sitter-cpp")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
	(elixir "https://github.com/elixir-lang/tree-sitter-elixir")
	(gomod "https://github.com/camdencheek/tree-sitter-go-mod")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(heex "https://github.com/phoenixframework/tree-sitter-heex")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(java "https://github.com/tree-sitter/tree-sitter-java")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
	(php "https://github.com/tree-sitter/tree-sitter-php" "master" "php/src")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(ruby "https://github.com/tree-sitter/tree-sitter-ruby")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")))

;; Exec in C-:
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

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
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
;; `php-ts-mode' don't work correct
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(provide 'setup-treesitter)
;;; setup-treesitter.el ends here
