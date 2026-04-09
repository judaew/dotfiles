;;; init-ai.el --- Configuration for LLM interaction -*- lexical-binding: t; -*-

;;; Commentary:

;; Packages:
;; - `gptel'       ~ A simple, extensible LLM client
;; - `gptel-agent' ~ Agent mode for gptel
;; - `eca'         ~ Editor Code Assistant

;; TODO:
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
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "* 👤 user: ")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "* 🤖 assistant: ")
  (setopt gptel-track-media t)

  (gptel-make-deepseek "DeepSeek"
    :stream t
    :key gptel-api-key
    :models '((deepseek-chat
               :capabilities (tool)
               :context-window 128
               :input-cost 0.28
               :output-cost 0.42)
              (deepseek-reasoner
               :capabilities (tool reasoning)
               :context-window 128
               :input-cost 0.28
               :output-cost 0.42)))

  ;; Set default model
  ;; See https://github.com/karthink/gptel/issues/704#issuecomment-2759390992
  (setopt gptel-backend (gptel-get-backend "DeepSeek"))
  (setopt gptel-model 'deepseek-chat) ;; or `deepseek-reasoner'

  (gptel-make-openai "Alibaba"
    :stream t
    :key gptel-api-key
    :protocol "https"
    :host "dashscope-intl.aliyuncs.com"
    :endpoint "/compatible-mode/v1/chat/completions"
    :models '(;; coding
              ;; "qwen3-coder-plus" "qwen3-coder-flash"
              ;; general
              "qwen3.6-plus" "qwen3.6-flash"
              ;; translate
              ;; "qwen-mt-plus" "qwen-mt-turbo"
              ))

  (gptel-make-openai "Alibaba NoThink"
    :stream t
    :key gptel-api-key
    :protocol "https"
    :host "dashscope-intl.aliyuncs.com"
    :endpoint "/compatible-mode/v1/chat/completions"
    :models '("qwen3.6-plus" "qwen3.6-flash")
    :request-params '(:enable_thinking "False")
    )

  (add-to-list 'gptel-directives
               `(english-editor . ,(with-temp-buffer
                                     (insert-file-contents "~/wrk/llm/prompts/english-editor.md")
                                     (buffer-string))))

  ;; (gptel-make-preset 'test
  ;;   :backend "Alibaba"
  ;;   :model 'qwen3.6-plus
  ;;   :request-params '(:enable_thinking "False")
  ;;   :system "")
  )

;; TODO: custom gptel-agent-dir path
;; gptel-agent-dirs is a variable defined in ‘gptel-agent.el’.
;;
;; Its value is ("/home/judaew/.emacs.d/straight/build/gptel-agent/agents/")
;;
;; Agent definition directories for ‘gptel-agent’.
;;
;; Markdown (.md) and Org (.org) files in these directories will be scanned
;; for gptel sub-agent definitions by ‘gptel-agent’.
(use-package gptel-agent
  :config (gptel-agent-update))

(use-package eca
  :straight (eca :type git :host github :repo "editor-code-assistant/eca-emacs" :branch "master"))

(provide 'init-ai)
;;; init-ai.el ends here
