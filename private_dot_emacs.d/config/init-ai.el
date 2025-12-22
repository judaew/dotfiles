;;; init-ai.el --- Configuration for LLM interaction -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `gptel'
;; - `gptel-quick'

;; TODO:
;; - see evedel https://github.com/daedsidog/evedel or
;;       elysium https://github.com/lanceberge/elysium
;; - create an alternative to gptel-quick that will be more universal
;; - just kidding, but maybe for fun to test gptel-autocomplete, BUT with only
;;   local models (ollama)
;;   https://github.com/JDNdeveloper/gptel-autocomplete
;; - ob-gptel https://github.com/jwiegley/ob-gptel

;;; Code:

(use-package gptel
  :bind
  (("C-c q" . gptel-send)
   ("C-c g c" . gptel)
   ("C-c g m" . gptel-menu)
   ("C-c g t" . gptel-tools)
   ("C-c g a" . gptel-add)
   ("C-c g f" . gptel-add-file)
   ("C-c g r" . gptel-context-remove-all))
  :config
  (setopt gptel-default-mode 'org-mode)
  (setopt gptel-track-media t)

  (gptel-make-deepseek "DeepSeek"
    :stream t
    :key gptel-api-key)

  ;; Set default model
  ;; See https://github.com/karthink/gptel/issues/704#issuecomment-2759390992
  (setopt gptel-backend (gptel-get-backend "DeepSeek"))
  (setopt gptel-model 'deepseek-chat)

  (gptel-make-openai "Alibaba"
    :stream t
    :key gptel-api-key
    :protocol "https"
    :host "dashscope-intl.aliyuncs.com"
    :endpoint "/compatible-mode/v1/chat/completions"
    :models '(;; coding
              "qwen3-coder-plus" "qwen3-coder-flash"
              ;; general
              "qwen3-max" "qwen3-plus" "qwen-flash"
              ;; translate
              "qwen-mt-plus"
              "qwen-mt-turbo"))

  (gptel-make-ollama "Local"
    :host "localhost:11434"
    :stream t
    :models '("qwen3:30b" "qwen3-coder:30b" "gpt-oss:20b"))

  ;; My gptel tools
  (load "~/.emacs.d/config/setup-llm-tools.el")
  )

(use-package gptel-quick
  :straight (gptel-quick :type git :host github :repo "karthink/gptel-quick")
  :bind ("C-c Q" . gptel-quick))

(provide 'init-ai)
;;; init-ai.el ends here
