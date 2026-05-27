;;; inline-suggestion.el --- Cursor-style inline ghost text completions via a local /infill server -*- lexical-binding: t -*-

;; Author: Willy
;; Version: 0.3.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: completion, convenience
;; URL: https://github.com/willy/inline-suggestion

;;; Commentary:

;; Provides inline ghost text code suggestions by talking to a local
;; HTTP server that speaks the llama.cpp `/infill' FIM API.  This
;; package is a pure client: it does not start, stop, or otherwise
;; manage the server process.
;;
;; On this machine the server is the rkllm shim at
;;   /home/m6/willy/local-llm/server.py
;; and is managed by the systemd user unit `local-llm.service'.  See
;; that directory's Makefile and the project notes for setup.
;;
;; Suggestions appear as translucent overlay text at the cursor.
;; Press TAB to accept, or just keep typing to dismiss.
;;
;; No external Emacs dependencies — uses built-in `url.el' and `json.el'.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

;; ============================================================================
;; Customization
;; ============================================================================

(defgroup inline-suggestion nil
  "Inline ghost text completions via a local /infill server."
  :group 'completion
  :prefix "inline-suggestion-")

(defcustom inline-suggestion-server-url "http://localhost:8080"
  "Base URL for the /infill server."
  :type 'string
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
;; Context construction
;; ============================================================================

(defun inline-suggestion--build-prefix ()
  "Build the prefix string from buffer context before point."
  (let ((prefix-start (save-excursion
                         (forward-line (- inline-suggestion-max-prefix-lines))
                         (line-beginning-position))))
    (buffer-substring-no-properties prefix-start (point))))

(defun inline-suggestion--build-suffix ()
  "Build the suffix string from buffer context after point."
  (let ((suffix-end (save-excursion
                      (forward-line inline-suggestion-max-suffix-lines)
                      (line-end-position))))
    (buffer-substring-no-properties (point) suffix-end)))

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

(defun inline-suggestion--request-body (prefix suffix)
  "Build the JSON request body for /infill with PREFIX and SUFFIX."
  (let ((body `(("input_prefix" . ,prefix)
                ("input_suffix" . ,suffix)
                ("n_predict" . ,inline-suggestion-max-tokens)
                ("temperature" . 0)
                ("stream" . :json-false))))
    (encode-coding-string
     (inline-suggestion--escape-non-ascii (json-encode body))
     'utf-8)))

(defun inline-suggestion--fetch ()
  "Send async FIM request and process the response.
Snapshots `(point)' and `(buffer-chars-modified-tick)' at send time.
When the response arrives:
- If the buffer is unchanged, show the suggestion as ghost text.
- If the user typed during the request and those chars are a prefix
  of the suggestion, show the remaining suggestion.
- Otherwise drop the response and fire a new request."
  (let* ((snap-point (point))
         (snap-tick (buffer-chars-modified-tick))
         (snap-buffer (current-buffer))
         (prefix (inline-suggestion--build-prefix))
         (suffix (inline-suggestion--build-suffix))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data (inline-suggestion--request-body prefix suffix))
         (url-show-status nil)
         (api-url (concat inline-suggestion-server-url "/infill")))
    (setq inline-suggestion--request-in-flight t)
    (setq inline-suggestion--http-buffer
          (url-retrieve
           api-url
           (lambda (_status)
             (let (text)
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
                         (if (and (boundp 'url-http-end-of-headers)
                                  url-http-end-of-headers)
                             (goto-char url-http-end-of-headers)
                           (goto-char (point-min))
                           (unless (re-search-forward "\r?\n\r?\n" nil t)
                             (error "Could not find HTTP response body")))
                         (let* ((json-object-type 'alist)
                                (json-array-type 'vector)
                                (resp (json-read))
                                (resp-text (alist-get 'content resp)))
                           (when (and resp-text
                                      (not (string-empty-p (string-trim resp-text))))
                             (setq text (string-trim-right resp-text)))))
                     (error
                      (message "inline-suggestion: request error: %s"
                               (error-message-string err))))
               ;; Kill the HTTP response buffer
               (let ((buf (current-buffer)))
                 (when (buffer-live-p buf)
                   (let ((proc (get-buffer-process buf)))
                     (when (and proc (process-live-p proc))
                       (set-process-query-on-exit-flag proc nil)
                       (delete-process proc)))
                   (kill-buffer buf)))
               ;; Clear in-flight flag and decide: show, salvage, or re-fire
               (when (buffer-live-p snap-buffer)
                 (with-current-buffer snap-buffer
                   (setq inline-suggestion--request-in-flight nil)
                   (let* ((cur-point (point))
                          (cur-tick (buffer-chars-modified-tick))
                          (state-changed (or (/= snap-point cur-point)
                                             (/= snap-tick cur-tick)))
                          (showed nil))
                     (when text
                       (cond
                        ;; No edits during request — show as-is
                        ((not state-changed)
                         (inline-suggestion--show text)
                         (setq showed t))
                        ;; Typed forward — salvage if typed is a prefix
                        ((and (> cur-point snap-point)
                              (let ((typed (buffer-substring-no-properties
                                            snap-point cur-point)))
                                (string-prefix-p typed text)))
                         (let ((typed (buffer-substring-no-properties
                                       snap-point cur-point)))
                           (inline-suggestion--show
                            (substring text (length typed)))
                           (setq showed t)))))
                     ;; State changed and nothing shown — fire a new request
                     (when (and state-changed (not showed))
                       (inline-suggestion--fetch))))))))
           nil t t))
    ;; Prevent "active connection" prompt on Emacs exit
    (when-let ((proc (get-buffer-process inline-suggestion--http-buffer)))
      (set-process-query-on-exit-flag proc nil))))

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
                         (lambda (&rest _) (inline-suggestion--clear))))))

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

;; ============================================================================
;; Scheduling
;; ============================================================================

(defun inline-suggestion--schedule ()
  "Fire a request now unless one is already in flight.
When a request is in flight, the response handler will fire the
next request itself if the buffer state changed during the wait."
  (unless inline-suggestion--request-in-flight
    (inline-suggestion--fetch)))

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
   ;; On self-insert (typing), clear stale overlay and fire request.
   ;; NOTE: while a suggestion overlay is visible, the transient keymap's
   ;; default ([t]) binding intercepts the key first and runs
   ;; `inline-suggestion-dismiss-and-replay'; the type-along logic lives
   ;; there.  This branch only runs when no overlay is active.
   ((eq this-command 'self-insert-command)
    (inline-suggestion--clear)
    (inline-suggestion--schedule))
   ;; On any editing command (delete, kill, yank, etc.), same as above
   ((memq this-command '(delete-backward-char delete-forward-char
                         backward-delete-char-untabify
                         backward-kill-word kill-word
                         kill-line kill-whole-line
                         yank newline newline-and-indent
                         indent-for-tab-command))
    (inline-suggestion--clear)
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
  "Dismiss the current suggestion and replay the last key.
Type-along: when the key just pressed is a single character that
matches the next character of the visible ghost text, consume that
one character and keep the remaining ghost text in place instead of
dismissing.  This lets you type along the suggestion and accept the
rest with TAB whenever you want.  When the ghost text runs out, fire
a fresh request.  Any non-matching key dismisses and is replayed."
  (interactive)
  (let* ((text inline-suggestion--suggestion-text)
         (keys (this-command-keys-vector))
         (ev (and (> (length keys) 0) (aref keys (1- (length keys))))))
    (if (and text
             (> (length text) 0)
             (characterp ev)
             (eq ev (aref text 0)))
        ;; Typed char matches next ghost char — type-along
        (let ((remaining (substring text 1)))
          (inline-suggestion--clear)
          (insert (char-to-string ev))
          (if (string-empty-p remaining)
              ;; Ghost text fully consumed — ask for more
              (inline-suggestion--schedule)
            ;; Keep showing the remaining ghost text at the new point
            (inline-suggestion--show remaining)))
      ;; No match — dismiss and replay the key normally
      (inline-suggestion--clear)
      (when (and keys (> (length keys) 0))
        (setq unread-command-events
              (append (listify-key-sequence keys)
                      unread-command-events))))))

;; ============================================================================
;; Global cleanup
;; ============================================================================

(defun inline-suggestion--kill-all-connections ()
  "Kill all lingering inline-suggestion HTTP buffers and connections."
  (let ((host (url-host (url-generic-parse-url inline-suggestion-server-url))))
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (string-match-p (concat "\\` \\*http.*" (regexp-quote host))
                                 (buffer-name buf)))
        (let ((proc (get-buffer-process buf)))
          (when (and proc (process-live-p proc))
            (set-process-query-on-exit-flag proc nil)
            (delete-process proc)))
        (kill-buffer buf)))))

;; ============================================================================
;; Health probe
;; ============================================================================

(defun inline-suggestion--health-probe ()
  "Probe the server with an async TCP connect.
On failure, print a clear hint with the exact systemctl command
so silent server-down failure becomes a loud, actionable message."
  (let* ((parsed (url-generic-parse-url inline-suggestion-server-url))
         (host (url-host parsed))
         (port (url-port parsed))
         (hint (format "inline-suggestion: server unreachable at %s. Try: systemctl --user start local-llm.service"
                       inline-suggestion-server-url)))
    (condition-case _err
        (let ((proc (make-network-process
                     :name "inline-suggestion-probe"
                     :host host :service port
                     :nowait t)))
          (set-process-query-on-exit-flag proc nil)
          (set-process-sentinel
           proc
           (lambda (p event)
             (cond
              ((string-prefix-p "open" event)
               (delete-process p))
              ((or (string-prefix-p "failed" event)
                   (string-prefix-p "connection broken" event))
               (message "%s" hint)
               (delete-process p))))))
      (error
       (message "%s" hint)))))

;; ============================================================================
;; Minor mode
;; ============================================================================

;;;###autoload
(define-minor-mode inline-suggestion-mode
  "Minor mode for inline ghost text suggestions via a local /infill server.
The server lifecycle is managed externally (e.g. by the
`local-llm.service' systemd user unit on this machine).  This
mode is a pure client."
  :lighter " InlSug"
  :group 'inline-suggestion
  (if inline-suggestion-mode
      (progn
        (add-hook 'post-command-hook #'inline-suggestion--post-command nil t)
        (add-hook 'kill-emacs-hook #'inline-suggestion--kill-all-connections)
        (advice-add 'posn-at-point :before-until #'inline-suggestion--posn-advice)
        (inline-suggestion--health-probe))
    ;; Teardown
    (remove-hook 'post-command-hook #'inline-suggestion--post-command t)
    (inline-suggestion--cancel-request)
    (inline-suggestion--clear)
    (advice-remove 'posn-at-point #'inline-suggestion--posn-advice)))

;;;###autoload
(defun inline-suggestion-toggle ()
  "Toggle `inline-suggestion-mode' in the current buffer."
  (interactive)
  (if inline-suggestion-mode
      (progn
        (inline-suggestion-mode -1)
        (message "inline-suggestion OFF"))
    (inline-suggestion-mode 1)
    (message "inline-suggestion ON")))

(provide 'inline-suggestion)

;;; inline-suggestion.el ends here
