;;; init-llm-client.el --- Configure gptel for OpenAI -*- lexical-binding: t; -*-

;;; Commentary:

;; Configures the gptel package to use the OpenAI API, taking the API
;; key from the `OpenAIAPIKey` environment variable or from the file
;; `~/.emacs.d/gptel-api-key.txt`.

;;; Code:

(when (maybe-require-package 'gptel)
  (if-let ((api-key-file (expand-file-name "~/.emacs.d/gptel-api-key.txt")))
      (when (file-exists-p api-key-file)
        (setq gptel-api-key (with-temp-buffer
                              (insert-file-contents api-key-file)
                              (string-trim (buffer-string)))))
    (setq gptel-api-key (getenv "OpenAIAPIKey")))
  (setq gptel-model "gpt-4o")
  (setq gptel-response-format 'markdown))

(provide 'init-llm-client)

;;; init-llm-client.el ends here