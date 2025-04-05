;;; init-ellama.el --- Configuration for Ellama with Google Gemini -*- lexical-binding: t -*-

(provide 'init-ellama)

(use-package ellama
  :ensure t
  :init
  (require 'llm-gemini)
  (setopt llm-warn-on-nonfree nil)
  (setopt ellama-provider
          (make-llm-gemini
           :key (getenv "GEMINI_API_KEY")
           :chat-model "gemini-2.5-pro-preview-03-25"))
  (setopt ellama-coding-provider
          (make-llm-gemini
           :key (getenv "GEMINI_API_KEY")
           :chat-model "gemini-2.5-pro-preview-03-25"))
  :config
  ;; 确保函数加载后再绑定快捷键
  (global-set-key (kbd "C-c e c") 'ellama-code-complete)
  (global-set-key (kbd "C-c e r") 'ellama-code-review))

;;; init-ellama.el ends here
