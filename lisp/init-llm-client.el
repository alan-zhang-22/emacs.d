;;; init-llm-client.el --- Configure gptel for OpenAI -*- lexical-binding: t; -*-

;;; Commentary:

;; Configures the gptel package to use the OpenAI API, taking the API
;; key from the `OpenAIAPIKey` environment variable.

;;; Code:

(when (maybe-require-package 'gptel)
  (setq gptel-api-key (getenv "OpenAIAPIKey"))
  (setq gptel-model "gpt-4o")
  (setq gptel-response-format 'markdown))

(provide 'init-llm-client)

;;; init-llm-client.el ends here
