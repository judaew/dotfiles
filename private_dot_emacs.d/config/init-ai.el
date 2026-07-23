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
  )

(use-package gptel-agent
  :bind
  ("C-c g a" . gptel-agent)
  :config
  (setq gptel-agent-dirs '("/home/judaew/wrk/llm/agents/"))
  (gptel-agent-update))

(provide 'init-ai)
;;; init-ai.el ends here
