;;; init-review-content.el --- A gptel-based content review workflow -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides an interactive workflow for reviewing a file's content using gptel.
;; To use, run `M-x content-review`.

;;; Code:

(require 'cl-lib)

(use-package ediff
  :ensure nil)

(defvar review-content-models '("gpt-4o" "gpt-4-turbo" "gpt-3.5-turbo")
  "A list of recommended LLM models for the review workflow.")

(defvar review-content-methods '("Accept all at once" "Accept one by one")
  "A list of available review methods.")

(defvar review-content-output-formats '("ediff" "side-by-side diff")
  "A list of available output formats for the review.")

(defvar review-content-scopes '("Full document" "Active region")
  "A list of available review scopes.")

(defvar review-content-tones '("formal" "informal" "neutral" "technical" "creative")
  "A list of available review tones.")

;; Forward declaration for the function from gptel.
(declare-function gptel-request "gptel")
(defvar gptel-model)


;; --- Helper Functions for the Workflow ---

(defun content-review--apply-changes (new-content state)
  "Applies the new content to the buffer and saves it."
  (let ((scope (plist-get state :scope))
        (file-path (plist-get state :file-path)))
    (with-current-buffer (find-buffer-visiting file-path)
      (if (string= scope "Active region")
          (progn
            (goto-char (region-beginning))
            (delete-region (region-beginning) (region-end))
            (insert new-content))
        (erase-buffer)
        (insert new-content))
      (save-buffer)
      (message "Content updated and saved."))))

(defun content-review--rewrite-handler (new-content state)
  "Handles the rewritten content from the LLM, shows the diff, and continues the loop."
  (if (not (stringp new-content))
      (error "Gptel request failed. No rewrite received from the LLM.")
    (let* ((original-content (plist-get state :original-content))
           (original-buffer-name "*Review Original*")
           (rewrite-buffer-name "*Review Rewrite*")
           (original-buffer (get-buffer-create original-buffer-name))
           (rewrite-buffer (get-buffer-create rewrite-buffer-name))
           (window-config (current-window-configuration)))

      (with-current-buffer original-buffer
        (erase-buffer)
        (insert original-content))
      (with-current-buffer rewrite-buffer
        (erase-buffer)
        (insert new-content))

      (let ((ediff-split-window-function #'split-window-vertically))
        (ediff-buffers original-buffer rewrite-buffer))

      (if (y-or-n-p "Apply these changes? ")
          (progn
            (content-review--apply-changes new-content state)
            ;; Update the in-memory content for the next potential rewrite in this session.
            (setf (plist-get state :original-content) new-content)
            (message "Changes applied. Continuing review session..."))
        (message "Changes discarded. Continuing review session..."))

      ;; Cleanup and continue the loop.
      (kill-buffer original-buffer)
      (kill-buffer rewrite-buffer)
      (set-window-configuration window-config)
      (content-review--selection-loop state))))

(defun content-review--selection-loop (state)
  "The main selection loop. Displays remaining findings and acts on user choice."
  (let* ((done-str "[Done] Finish review and exit")
         (findings (plist-get state :findings))
         (selection (completing-read "Select a finding to address: " (append findings (list done-str)) nil t)))

    (when (and selection (not (string= selection done-str)))
      ;; Remove the selected finding from the list for the next iteration.
      (setf (plist-get state :findings) (remove selection findings))

      (let* ((model (plist-get state :model))
             (original-content (plist-get state :original-content))
             (tone (plist-get state :tone))
             (prompt (format (concat
                              "You are an expert technical writer. Your task is to rewrite the following document to address a specific issue, adopting a '%s' tone.\n\n"
                              "The issue to fix is: '%s'\n\n"
                              "Please provide the FULL rewritten text of the document. Do not omit any part of it. "
                              "Do not add any commentary, preamble, or explanation outside of the document's content itself.\n\n"
                              "--- ORIGINAL DOCUMENT ---\n\n%s\n\n--- END DOCUMENT ---")
                             tone selection original-content)))
        (message "Requesting rewrite for: %s" selection)
        (let ((gptel-model model))
          (gptel-request prompt :callback (lambda (new-content _info)
                                            (content-review--rewrite-handler new-content state))))))))

(defun content-review--initial-findings-handler (findings-text state)
  "Handles the initial list of findings and starts the main selection loop."
  (if (not (stringp findings-text))
      (error "Gptel request failed. No findings received from the LLM.")
    (let ((findings (seq-remove #'string-empty-p (split-string findings-text "\n"))))
      (setf (plist-get state :findings) findings)
      (content-review--selection-loop state))))


;; --- Main Entry Point ---

(defun content-review ()
  "Start the interactive content review workflow."
  (interactive)
  (unless (require 'gptel nil 'noerror)
    (error "The 'gptel' package is not available. Please install and configure it first."))

  (let* ((scope (completing-read "Select a review scope: " review-content-scopes nil t nil nil "Full document"))
         (file-path (if (string= scope "Full document")
                        (read-file-name "Select file to review: " nil nil t)
                      (buffer-file-name)))
         (domain-knowledge (read-string "Domain knowledge for review (optional): "))
         (model (completing-read "Select a model for the review: " review-content-models nil t nil nil "gpt-4o"))
         (tone (completing-read "Select a review tone: " review-content-tones nil t nil nil "neutral"))
         (original-content (if (and (string= scope "Active region") (use-region-p))
                               (buffer-substring-no-properties (region-beginning) (region-end))
                             (with-current-buffer (find-file-noselect file-path)
                               (buffer-substring-no-properties (point-min) (point-max))))))
    (let ((state (list :file-path file-path
                       :scope scope
                       :domain-knowledge domain-knowledge
                       :model model
                       :tone tone
                       :original-content original-content
                       :findings '())))
      (let ((initial-prompt
             (format (concat
                      "You are an expert content reviewer. Your task is to analyze the following document and identify areas for improvement, adopting a '%s' tone. "
                      "Consider the following domain knowledge: '%s'.\n\n"
                      "Please provide a numbered list of specific, actionable findings. Each finding should be a single, concise line. Do not include a preamble.\n"
                      "For example:\n"
                      "1. The introduction is unclear and should be rewritten for clarity.\n"
                      "2. There is a factual error in the section about quantum computing.\n"
                      "3. The code block in the 'Usage' section has a syntax error.\n\n"
                      "--- DOCUMENT CONTENT ---\n\n%s\n\n--- END DOCUMENT ---")
                     (plist-get state :tone)
                     (plist-get state :domain-knowledge)
                     (plist-get state :original-content))))
        (message "Sending content for initial review...")
        (let ((gptel-model (plist-get state :model)))
          (gptel-request initial-prompt :callback (lambda (findings-text _info)
                                                    (content-review--initial-findings-handler findings-text state))))))))

(let ((map (make-sparse-keymap)))
  (define-key map (kbd "r") 'content-review)
  (global-set-key (kbd "C-c g") map))

(message "Content Review Workflow loaded. Use 'C-c g r' to start.")

(provide 'init-content-review)

;;; init-review-content.el ends here