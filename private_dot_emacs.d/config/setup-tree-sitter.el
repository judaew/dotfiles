;;; Package --- Summary -*- lexical-binding: t; -*-

;;; Commentary:

;; See more
;;
;; - https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
;; - https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; - https://www.masteringemacs.org/article/whats-new-in-emacs-301
;; - https://github.com/renzmann/treesit-auto/issues/128
;;
;; Golang: https://github.com/dominikh/go-mode.el/issues/440

;;; Code:

;;; Tree-sitter
(setq treesit-language-source-alist
      '(;; Languages
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (c++ "https://github.com/tree-sitter/tree-sitter-cpp")
        (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
        (java "https://github.com/tree-sitter/tree-sitter-java")
        (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")

        ;; Data serialization languages
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")

        ;; Web languages
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (js "https://github.com/tree-sitter/tree-sitter-javascript")
        (php "https://github.com/tree-sitter/tree-sitter-php")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript")

        ;; Tools
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
      '(;; Languages
        (bash-mode . bash-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (csharp-mode . csharp-ts-mode)
        (go-mode . go-ts-mode)
        (go-mod-mode . go-mod-ts-mode)
        (java-mode . java-ts-mode)
        (lua-mode . lua-ts-mode)
        (python-mode . python-ts-mode)
        (ruby-mode . ruby-ts-mode)
        (rust-mode . rust-ts-mode)

        ;; Data serialization languages
        (json-mode . json-ts-mode)
        (toml-mode . toml-ts-mode)
        (yaml-mode . yaml-ts-mode)

        ;; Web languages
        (html-mode . html-ts-mode)
        (css-mode . css-ts-mode)
        (js-mode . js-ts-mode)
        (php-mode . php-ts-mode)
        (typescript-mode . typescript-ts-mode)

        ;; Tools
        (cmake-mode . cmake-ts-mode)
        (dockerfile-mode . dockerfile-ts-mode)))

;;; setup-tree-sitter.el ends here
