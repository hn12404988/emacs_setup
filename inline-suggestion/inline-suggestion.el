;;; inline-suggestion.el --- Cursor-style inline ghost text completions via Qwen prefix completion -*- lexical-binding: t -*-

;; Author: Willy
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: completion, convenience
;; URL: https://github.com/willy/inline-suggestion

;;; Commentary:

;; Provides inline ghost text code suggestions using the Qwen API's
;; prefix completion (partial mode) capability.  Suggestions appear as
;; translucent overlay text at the cursor after a short idle delay.
;; Press TAB to accept, or just keep typing to dismiss.
;;
;; No external dependencies — uses built-in `url.el' and `json.el'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

;; ============================================================================
;; Customization
;; ============================================================================

(defgroup inline-suggestion nil
  "Inline ghost text completions via Qwen FIM."
  :group 'completion
  :prefix "inline-suggestion-")

(defcustom inline-suggestion-api-key nil
  "API key for Qwen/DashScope.
Falls back to the environment variable DASHSCOPE_API_KEY if nil."
  :type '(choice (const :tag "Use $DASHSCOPE_API_KEY" nil)
                 (string :tag "API key"))
  :group 'inline-suggestion)

(defcustom inline-suggestion-api-url
  "https://dashscope-intl.aliyuncs.com/compatible-mode/v1"
  "Base URL for the OpenAI-compatible API endpoint."
  :type 'string
  :group 'inline-suggestion)

(defcustom inline-suggestion-model "qwen3.5-flash"
  "Model name to use for completions."
  :type 'string
  :group 'inline-suggestion)

(defcustom inline-suggestion-idle-delay 0.1
  "Seconds of idle time before requesting a suggestion."
  :type 'number
  :group 'inline-suggestion)

(defcustom inline-suggestion-max-tokens 100
  "Maximum number of tokens to generate."
  :type 'integer
  :group 'inline-suggestion)

(defcustom inline-suggestion-max-prefix-lines 50
  "Maximum number of lines before cursor to include as context."
  :type 'integer
  :group 'inline-suggestion)

(defcustom inline-suggestion-max-suffix-lines 20
  "Maximum number of lines after cursor to include as context."
  :type 'integer
  :group 'inline-suggestion)

;; ============================================================================
;; Internal state
;; ============================================================================

(defvar-local inline-suggestion--overlay nil
  "Overlay displaying the current ghost text suggestion.")

(defvar-local inline-suggestion--timer nil
  "Idle timer for scheduling suggestion requests.")

(defvar-local inline-suggestion--http-buffer nil
  "Buffer for the in-flight HTTP request.")

(defvar-local inline-suggestion--active-keymap nil
  "Non-nil when the suggestion keymap is active.")

(defvar-local inline-suggestion--suggestion-text nil
  "The current suggestion text.")

(defvar-local inline-suggestion--real-posn nil
  "Saved posn-at-point before overlay display.
Works around Emacs reporting wrong cursor position with after-string overlays.")

(defvar-local inline-suggestion--request-in-flight nil
  "Non-nil while an HTTP request is pending.
Prevents sending another request until the current one completes.")

(defvar inline-suggestion--api-key-warned nil
  "Non-nil if we already warned about missing API key.")

;; ============================================================================
;; Keymap (active only while suggestion is visible)
;; ============================================================================

(defvar inline-suggestion--keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'inline-suggestion-accept)
    (define-key map (kbd "<tab>") #'inline-suggestion-accept)
    (define-key map (kbd "M-D") #'inline-suggestion-accept-next-word)
    (define-key map (kbd "C-g") #'inline-suggestion-dismiss)
    (define-key map [escape] #'inline-suggestion-dismiss)
    (define-key map [t] #'inline-suggestion-dismiss-and-replay)
    map)
  "Keymap active while a suggestion overlay is visible.")

;; ============================================================================
;; API key resolution
;; ============================================================================

(defun inline-suggestion--api-key ()
  "Return the API key, from `inline-suggestion-api-key' or $DASHSCOPE_API_KEY.
Returns nil and logs a warning if neither is set."
  (or inline-suggestion-api-key
      (getenv "DASHSCOPE_API_KEY")
      (unless inline-suggestion--api-key-warned
        (setq inline-suggestion--api-key-warned t)
        (message "inline-suggestion: No API key. Set `inline-suggestion-api-key' or $DASHSCOPE_API_KEY.")
        nil)))

;; ============================================================================
;; Prefix construction
;; ============================================================================

(defun inline-suggestion--build-prefix ()
  "Build the prefix string from buffer context before point."
  (let ((prefix-start (save-excursion
                         (forward-line (- inline-suggestion-max-prefix-lines))
                         (line-beginning-position))))
    (buffer-substring-no-properties prefix-start (point))))

;; ============================================================================
;; HTTP request
;; ============================================================================

(defun inline-suggestion--escape-non-ascii (str)
  "Replace non-ASCII characters in STR with \\\\uXXXX JSON escapes.
This ensures the HTTP request body is ASCII-only, avoiding
Emacs Bug#23750 where `url-http-create-request' errors on
multibyte text when concatenating headers with the body."
  (replace-regexp-in-string
   "[^[:ascii:]]"
   (lambda (ch) (format "\\u%04x" (string-to-char ch)))
   str nil t))

(defun inline-suggestion--request-body (prefix)
  "Build the JSON request body for PREFIX continuation.
Uses the /v1/chat/completions endpoint with partial mode."
  (let ((body `(("model" . ,inline-suggestion-model)
                ("messages" . ,(vector
                                `(("role" . "user")
                                  ("content" . "Continue the code. Only output code, no explanations or markdown."))
                                `(("role" . "assistant")
                                  ("content" . ,prefix)
                                  ("partial" . t))))
                ("max_tokens" . ,inline-suggestion-max-tokens)
                ("temperature" . 0)
                ("enable_thinking" . :json-false))))
    (encode-coding-string
     (inline-suggestion--escape-non-ascii (json-encode body))
     'utf-8)))

(defun inline-suggestion--fetch (callback)
  "Send async FIM request.  Call CALLBACK with suggestion text on success.
Snapshots buffer state to detect staleness."
  (let ((api-key (inline-suggestion--api-key)))
    (when api-key
      (let* ((snap-point (point))
             (snap-tick (buffer-chars-modified-tick))
             (snap-buffer (current-buffer))
             (prefix (inline-suggestion--build-prefix))
             (url-request-method "POST")
             (url-request-extra-headers
              `(("Content-Type" . "application/json")
                ("Authorization" . ,(concat "Bearer " api-key))))
             (url-request-data (inline-suggestion--request-body prefix))
             (url-show-status nil)
             (api-url (concat inline-suggestion-api-url "/chat/completions")))
        (setq inline-suggestion--request-in-flight t)
        (setq inline-suggestion--http-buffer
              (url-retrieve
               api-url
               (lambda (_status)
                 (unwind-protect
                     (condition-case err
                         (progn
                           ;; Check for HTTP errors
                           (goto-char (point-min))
                           (unless (re-search-forward "^HTTP/[0-9.]+ 200" nil t)
                             (goto-char (point-min))
                             (let ((status-line (buffer-substring-no-properties
                                                 (point) (line-end-position)))
                                   (body ""))
                               (when (re-search-forward "\r?\n\r?\n" nil t)
                                 (setq body (buffer-substring-no-properties
                                             (point) (min (+ (point) 500) (point-max)))))
                               (error "Non-200: %s | %s" status-line body)))
                           ;; Jump to response body.
                           ;; url-http-end-of-headers is set by url-http.el
                           ;; and points right past the header/body blank line.
                           (if (and (boundp 'url-http-end-of-headers)
                                    url-http-end-of-headers)
                               (goto-char url-http-end-of-headers)
                             ;; Fallback: search for blank line (handles \r\n and \n)
                             (goto-char (point-min))
                             (unless (re-search-forward "\r?\n\r?\n" nil t)
                               (error "Could not find HTTP response body")))
                           (let* ((json-object-type 'alist)
                                  (json-array-type 'vector)
                                  (resp (json-read))
                                  (choices (alist-get 'choices resp))
                                  (msg (and (> (length choices) 0)
                                            (alist-get 'message (aref choices 0))))
                                  (text (and msg (alist-get 'content msg))))
                             (when (and text (not (string-empty-p (string-trim text))))
                               (when (buffer-live-p snap-buffer)
                                 (with-current-buffer snap-buffer
                                   (when (and (= snap-point (point))
                                              (= snap-tick (buffer-chars-modified-tick)))
                                     (funcall callback (string-trim-right text))))))))
                       (error
                        (message "inline-suggestion: request error: %s"
                                 (error-message-string err))))
                   ;; Always clear in-flight flag when done
                   (when (buffer-live-p snap-buffer)
                     (with-current-buffer snap-buffer
                       (setq inline-suggestion--request-in-flight nil)))
                   (let ((buf (current-buffer)))
                     (when (buffer-live-p buf)
                       (let ((proc (get-buffer-process buf)))
                         (when (and proc (process-live-p proc))
                           (set-process-query-on-exit-flag proc nil)
                           (delete-process proc)))
                       (kill-buffer buf)))))
               nil t t))
        ;; Prevent "active connection" prompt on Emacs exit
        (when-let ((proc (get-buffer-process inline-suggestion--http-buffer)))
          (set-process-query-on-exit-flag proc nil))))))

;; ============================================================================
;; Overlay display  (technique adapted from copilot.el)
;; ============================================================================

(defun inline-suggestion--posn-advice (&rest args)
  "Return saved cursor position when inline-suggestion overlay is active.
Works around Emacs reporting incorrect pixel coordinates
when an `after-string' overlay property is present."
  (when inline-suggestion-mode
    (let ((pos (or (car-safe args) (point))))
      (when (and inline-suggestion--real-posn
                 (eq pos (car inline-suggestion--real-posn)))
        (cdr inline-suggestion--real-posn)))))

(defun inline-suggestion--show (text)
  "Display TEXT as ghost text overlay at point.
The overlay spans from point to end-of-line.  At EOL the overlay
is hidden via `display' and the ghost text is rendered as
`after-string' with a `cursor' text property that keeps the real
cursor anchored.  Mid-line, `display' replaces the first covered
character and `after-string' holds the rest plus the original
trailing text so existing content remains visible after the ghost."
  (inline-suggestion--clear)
  (when (and text (not (string-empty-p text)))
    (setq inline-suggestion--suggestion-text text)
    (let* ((ov (make-overlay (point) (line-end-position) nil nil t))
           (tail (buffer-substring-no-properties (point) (line-end-position)))
           ;; Ghost text (shadow face) followed by the existing tail (default face)
           (p-completion (concat (propertize text 'face 'shadow) tail)))
      (if (eolp)
          ;; ---- cursor at end of line ----
          (progn
            ;; Temporarily set empty after-string so posn-at-point sees no
            ;; ghost text yet and returns the true pixel position.
            (overlay-put ov 'after-string "")
            (setq inline-suggestion--real-posn
                  (cons (point) (posn-at-point)))
            ;; Anchor cursor at position 0 of the after-string
            (put-text-property 0 1 'cursor t p-completion)
            (overlay-put ov 'display "")
            (overlay-put ov 'after-string p-completion))
        ;; ---- cursor mid-line ----
        ;; The overlay covers existing text from point to EOL.
        ;; `display' replaces the covered region's first char visually;
        ;; `after-string' appends the remainder of the completion + tail.
        (overlay-put ov 'display (substring p-completion 0 1))
        (overlay-put ov 'after-string (substring p-completion 1)))
      (overlay-put ov 'inline-suggestion t)
      (overlay-put ov 'priority 100)
      (setq inline-suggestion--overlay ov))
    ;; Push the active keymap
    (unless inline-suggestion--active-keymap
      (setq inline-suggestion--active-keymap t)
      (set-transient-map inline-suggestion--keymap
                         (lambda () inline-suggestion--active-keymap)
                         #'inline-suggestion-dismiss-and-replay))))

(defun inline-suggestion--clear ()
  "Remove the suggestion overlay and deactivate the keymap."
  (when inline-suggestion--overlay
    (delete-overlay inline-suggestion--overlay)
    (setq inline-suggestion--overlay nil))
  (setq inline-suggestion--suggestion-text nil)
  (setq inline-suggestion--active-keymap nil)
  (setq inline-suggestion--real-posn nil))

;; ============================================================================
;; Request management
;; ============================================================================

(defun inline-suggestion--cancel-request ()
  "Kill any in-flight HTTP request."
  (when (and inline-suggestion--http-buffer
             (buffer-live-p inline-suggestion--http-buffer))
    (let ((proc (get-buffer-process inline-suggestion--http-buffer)))
      (when (and proc (process-live-p proc))
        (set-process-query-on-exit-flag proc nil)
        (delete-process proc)))
    (kill-buffer inline-suggestion--http-buffer))
  (setq inline-suggestion--http-buffer nil)
  (setq inline-suggestion--request-in-flight nil))

(defun inline-suggestion--cancel-timer ()
  "Cancel the pending idle timer."
  (when inline-suggestion--timer
    (cancel-timer inline-suggestion--timer)
    (setq inline-suggestion--timer nil)))

;; ============================================================================
;; Scheduling and triggering
;; ============================================================================

(defun inline-suggestion--trigger ()
  "Snapshot buffer state and fire async FIM request.
Skips if a request is already in flight to avoid piling up
requests on slow connections."
  (setq inline-suggestion--timer nil)
  (if inline-suggestion--request-in-flight
      ;; Request pending — reschedule so we try again after it finishes
      (setq inline-suggestion--timer
            (run-with-idle-timer 0.5 nil
                                 (let ((buf (current-buffer)))
                                   (lambda ()
                                     (when (buffer-live-p buf)
                                       (with-current-buffer buf
                                         (inline-suggestion--trigger)))))))
    (inline-suggestion--cancel-request)
    (inline-suggestion--fetch #'inline-suggestion--show)))

(defun inline-suggestion--schedule ()
  "Cancel previous timer/request and schedule a new idle timer."
  (inline-suggestion--cancel-timer)
  (inline-suggestion--cancel-request)
  (inline-suggestion--clear)
  (setq inline-suggestion--timer
        (run-with-idle-timer inline-suggestion-idle-delay nil
                             (let ((buf (current-buffer)))
                               (lambda ()
                                 (when (buffer-live-p buf)
                                   (with-current-buffer buf
                                     (inline-suggestion--trigger))))))))

;; ============================================================================
;; Post-command hook
;; ============================================================================

(defun inline-suggestion--post-command ()
  "Hook run after each command.  Schedule suggestion on edits."
  (cond
   ;; If an overlay is visible and the command was our accept/dismiss, do nothing
   ((memq this-command '(inline-suggestion-accept
                         inline-suggestion-accept-next-word
                         inline-suggestion-dismiss
                         inline-suggestion-dismiss-and-replay))
    nil)
   ;; On self-insert (typing), schedule new suggestion
   ((eq this-command 'self-insert-command)
    (inline-suggestion--schedule))
   ;; On any editing command (delete, kill, yank, etc.), schedule
   ((memq this-command '(delete-backward-char delete-forward-char
                         backward-delete-char-untabify
                         backward-kill-word kill-word
                         kill-line kill-whole-line
                         yank newline newline-and-indent
                         indent-for-tab-command))
    (inline-suggestion--schedule))
   ;; On movement or anything else, just clear the suggestion
   (t
    (when inline-suggestion--overlay
      (inline-suggestion--clear)))))

;; ============================================================================
;; Interactive commands
;; ============================================================================

(defun inline-suggestion-accept ()
  "Accept the current suggestion and insert it."
  (interactive)
  (when inline-suggestion--suggestion-text
    (let ((text inline-suggestion--suggestion-text))
      (inline-suggestion--clear)
      (insert text))))

(defun inline-suggestion-accept-next-word ()
  "Accept the next word from the current suggestion."
  (interactive)
  (when inline-suggestion--suggestion-text
    (let* ((text inline-suggestion--suggestion-text)
           (syn-table (syntax-table))
           ;; Use buffer's syntax table to find next word boundary
           (word-end (with-temp-buffer
                       (insert text)
                       (set-syntax-table syn-table)
                       (goto-char (point-min))
                       (skip-syntax-forward " >")  ;; skip whitespace/newlines
                       (if (eobp)
                           (length text)
                         (forward-word 1)
                         (1- (point)))))
           (word (substring text 0 word-end))
           (remaining (substring text word-end)))
      (inline-suggestion--clear)
      (insert word)
      ;; If there's remaining text, re-show it as a new overlay
      (when (not (string-empty-p (string-trim remaining)))
        (inline-suggestion--show remaining)))))

(defun inline-suggestion-dismiss ()
  "Dismiss the current suggestion."
  (interactive)
  (inline-suggestion--clear))

(defun inline-suggestion-dismiss-and-replay ()
  "Dismiss the current suggestion and replay the last key."
  (interactive)
  (inline-suggestion--clear)
  (let ((keys (this-command-keys-vector)))
    (when (and keys (> (length keys) 0))
      (setq unread-command-events
            (append (listify-key-sequence keys)
                    unread-command-events)))))

;; ============================================================================
;; Global cleanup
;; ============================================================================

(defun inline-suggestion--kill-all-connections ()
  "Kill all lingering inline-suggestion HTTP buffers and connections."
  (dolist (buf (buffer-list))
    (when (and (buffer-live-p buf)
               (string-match-p "\\` \\*http.*dashscope" (buffer-name buf)))
      (let ((proc (get-buffer-process buf)))
        (when (and proc (process-live-p proc))
          (set-process-query-on-exit-flag proc nil)
          (delete-process proc)))
      (kill-buffer buf))))

;; ============================================================================
;; Minor mode
;; ============================================================================

;;;###autoload
(define-minor-mode inline-suggestion-mode
  "Minor mode for inline ghost text suggestions via Qwen FIM."
  :lighter " InlSug"
  :group 'inline-suggestion
  (if inline-suggestion-mode
      (progn
        (add-hook 'post-command-hook #'inline-suggestion--post-command nil t)
        (add-hook 'kill-emacs-hook #'inline-suggestion--kill-all-connections)
        (advice-add 'posn-at-point :before-until #'inline-suggestion--posn-advice))
    ;; Teardown
    (remove-hook 'post-command-hook #'inline-suggestion--post-command t)
    (inline-suggestion--cancel-timer)
    (inline-suggestion--cancel-request)
    (inline-suggestion--clear)
    (advice-remove 'posn-at-point #'inline-suggestion--posn-advice)))

(provide 'inline-suggestion)

;;; inline-suggestion.el ends here
