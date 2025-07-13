;;; init-aidermacs.el --- Configuration for aidermacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package aidermacs
  :ensure t
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setq aidermacs-default-model "gemini/gemini-2.5-pro")
  (when (getenv "GEMINI_API_KEY")
    (setenv "GEMINI_API_KEY" (getenv "GEMINI_API_KEY"))))

(provide 'init-aidermacs)
;;; init-aidermacs.el ends here
