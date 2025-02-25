;;; Package --- Summary

;;; Commentary:

;; See
;; - https://git.savannah.gnu.org/cgit/emacs.git/tree/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter
;; - https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;; - https://robbmann.io/posts/emacs-treesit-auto/
;; - https://github.com/renzmann/treesit-auto/issues/128
;; Golang: https://github.com/dominikh/go-mode.el/issues/440

;;; Code:

;;; Tree-sitter
(setq treesit-language-source-alist
      '((c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (c-sharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
        (rust "https://github.com/tree-sitter/tree-sitter-rust")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (lua "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")

        (sql "https://github.com/derekstride/tree-sitter-sql")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (proto "https://github.com/treywood/tree-sitter-proto")
        (toml "https://github.com/tree-sitter-grammars/tree-sitter-toml")
        (yaml "https://github.com/tree-sitter-grammars/tree-sitter-yaml")

        ;; (markdown "")

        (html "https://github.com/tree-sitter/tree-sitter-html")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript")

        (make "https://github.com/alemuller/tree-sitter-make")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

;; (setq major-mode-remap-alist
;;       '((c-mode . c-ts-mode)
;;        (go-mode . go-ts-mode)))

;;(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
;;(add-to-list 'interpreter-mode-alist '("lua" . lua-ts-mode))

;;; setup-tree-sitter.el ends here
