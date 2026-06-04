;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; ============================================================================
;; NEW LAPTOP SETUP NOTES
;; ============================================================================
;; This config is fully portable. All Emacs packages auto-install on first run.
;;
;; External dependencies (optional - config degrades gracefully without them):
;;   brew install pandoc      # markdown-mode HTML preview (C-c C-c p)
;;   brew install rich        # Dired markdown preview Shift+M (macOS)
;;   pipx install rich-cli && pipx inject rich-cli 'rich>=13.7'   # Linux
;;                            # ^ inject is required: rich-cli pins rich==12.6
;;                            #   which has no markdown table support.
;;   brew install aspell      # Spell checking
;;
;; Fonts:
;;   Install JetBrains Mono from https://www.jetbrains.com/lp/mono/
;;   Or: brew install --cask font-jetbrains-mono
;;
;; For Rust development:
;;   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
;; ============================================================================

;; Suppress cl deprecation warning
(setq byte-compile-warnings '(not obsolete cl-functions))
(setq warning-suppress-log-types '((comp) (bytecomp) (tramp)))
(setq warning-suppress-types '((comp) (bytecomp) (tramp)))

;; --- Performance tuning (TTY + LSP on slower SoCs like rk3588) ---
;; Default gc-cons-threshold (800KB) triggers GC every few keystrokes once LSP
;; is running. Use a high value during startup, then settle at runtime.
(setq gc-cons-threshold (* 256 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 100 1024 1024))
            (setq gc-cons-percentage 0.1)))

;; LSP servers (rust-analyzer, ts-ls) send large JSON-RPC chunks.
;; Default 4KB read buffer causes throughput stalls. Bump to 1MB.
(setq read-process-output-max (* 1024 1024))

;; Disable backup files (the ones ending with ~)
(setq make-backup-files nil)

;; Disable auto-save files (the ones with #...#)
(setq auto-save-default nil)

;; Auto-revert buffers when files change on disk
(global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)

;; PATH setup handled by exec-path-from-shell package below

;; Bootstrap straight.el package manager
;; Hide the *straight-process* buffer (space prefix = hidden buffer in Emacs)
(setq straight-process-buffer " *straight-process*")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el with use-package (no need for package.el)
(require 'cl-lib)  ;; For cl-remove-if and other cl functions
(straight-use-package 'use-package)
(require 'use-package)
(setq straight-use-package-by-default t)  ;; Auto-install packages via straight

;; Suppress compilation warnings from packages
(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level :error)

;; Diminish - hide noisy minor mode lighters from mode line
;; Keeps: LSP, FlyC (useful info). Hides the rest.
(use-package diminish
  :config
  (with-eval-after-load 'eldoc (diminish 'eldoc-mode))
  (with-eval-after-load 'flyspell (diminish 'flyspell-mode))
  (with-eval-after-load 'lsp-lens (diminish 'lsp-lens-mode)))

;; Highlight matching bracket
(show-paren-mode 1)
(defun my/show-matching-paren-offscreen (&rest _)
  "Show matching line in echo area if matching paren is offscreen."
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                            (char-equal (char-syntax cb) ?\))
                            (blink-matching-open))))
    (when (stringp matching-text) (message "%s" matching-text))))
(advice-add 'show-paren-function :after #'my/show-matching-paren-offscreen)

;;clock
(display-time-mode 1)
(setq display-time-day-and-date 1)
(setq display-time-24hr-format 1)

;;ido
(ido-mode 1)
(setq ido-default-buffer-method 'selected-window)
;; Flex matching: "ct" matches "chat.py", "inistl" matches "init.el".
;; Chars must appear in order; does NOT handle typos (e.g. "chad" won't match "chat.py").
(setq ido-enable-flex-matching t)
;; Hide *special* buffers from C-x b (anything starting with " " or "*")
(setq ido-ignore-buffers '("\\` " "\\`\\*"))

;; Mac-specific settings - use Option as Meta in terminal
(when (eq system-type 'darwin)
  (unless (display-graphic-p)
    (setq mac-option-key-is-meta t
          mac-option-modifier 'meta)))

;; Terminal: decode Shift+Tab (ESC [ Z) as <backtab>
(add-hook 'tty-setup-hook
          (lambda ()
            (define-key input-decode-map "\e[Z" [backtab])))

;; Enable clipboard integration
(setq select-enable-clipboard t)
(setq select-enable-primary t)

;; --- Cross-platform clipboard helpers ---
;; macOS: pbcopy/pbpaste
;; Linux in tmux: tmux load-buffer/save-buffer (works even without TMUX env var)
;; Linux with X11: xclip fallback

(defun my/tmux-reachable-p ()
  "Return non-nil if a tmux server is running and reachable.
Works regardless of whether the TMUX env var is set (e.g. Emacs
started as a daemon outside tmux)."
  (and (executable-find "tmux")
       (= 0 (call-process "tmux" nil nil nil "has-session"))))

(defun my/clipboard-copy-text (text)
  "Copy TEXT to system clipboard (cross-platform).
On non-macOS, also sends OSC 52 so the content reaches the local
terminal clipboard (e.g. Mac mini when SSH'd into a Linux box)."
  (cond
   ((eq system-type 'darwin)
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "pbcopy")))
   ((my/tmux-reachable-p)
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "tmux" nil nil nil "load-buffer" "-"))
    ;; Also push to the SSH client's clipboard via OSC 52
    (my/osc52-copy text))
   ((executable-find "xclip")
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "xclip" nil nil nil "-selection" "clipboard")))))

(defun my/clipboard-paste-text ()
  "Return current system clipboard content as string (cross-platform)."
  (string-trim-right
   (cond
    ((eq system-type 'darwin)
     (shell-command-to-string "pbpaste"))
    ((my/tmux-reachable-p)
     (shell-command-to-string "tmux save-buffer -"))
    ((executable-find "xclip")
     (shell-command-to-string "xclip -selection clipboard -o"))
    (t ""))
   "[\n]+"))

;; OSC 52 clipboard support (works over SSH through tmux)
(defun my/osc52-copy (text)
  "Copy TEXT to system clipboard via OSC 52 escape sequence.
Works over SSH through tmux (requires `set -s set-clipboard on`)."
  (send-string-to-terminal
   (format "\e]52;c;%s\a"
           (base64-encode-string (encode-coding-string text 'utf-8) t))))

(defun my-smart-paste ()
  "Sync system clipboard to kill-ring, then paste."
  (interactive)
  (let ((clipboard-text (my/clipboard-paste-text)))
    ;; If clipboard has content and it's different from the last kill
    (when (and (not (string-empty-p clipboard-text))
               (or (null kill-ring)
                   (not (string= clipboard-text (car kill-ring)))))
      ;; Add clipboard content to kill-ring
      (kill-new clipboard-text)))
  ;; Now yank (which will use the most recent kill-ring entry)
  (yank))

(defun my-kill-ring-save ()
  "Copy to both kill ring and system clipboard."
  (interactive)
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (kill-new text)
      (my/clipboard-copy-text text)
      (deactivate-mark)
      (message "Copied to clipboard"))))

;; Disable startup messages (keep *Messages* for debugging)
(setq inhibit-startup-message t)

;; Rename *Shell Command Output* to a hidden buffer (space prefix hides from C-x b)
(defun my/hide-shell-command-output-buffer ()
  (when-let ((buf (get-buffer "*Shell Command Output*")))
    (with-current-buffer buf
      (rename-buffer " *Shell Command Output*" t))))

(add-hook 'window-configuration-change-hook #'my/hide-shell-command-output-buffer)

;; Hide noisy LSP/rust-analyzer buffers (rename with space prefix)
(defun my/hide-noisy-buffers ()
  "Hide buffers that clutter the buffer list by adding space prefix."
  (dolist (buf-name '("*rust-analyzer*" "*rust-analyzer::stderr*" "*lsp-log*" "*Messages*"))
    (when-let ((buf (get-buffer buf-name)))
      (with-current-buffer buf
        (rename-buffer (concat " " buf-name) t)))))

(add-hook 'window-configuration-change-hook #'my/hide-noisy-buffers)

;; exec-path-from-shell - ensure Emacs has the same PATH as shell
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :config
  ;; Use non-interactive shell to speed up initialization
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-mode))

;; TypeScript support
(use-package typescript-mode
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . typescript-mode)))

;; YAML support
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

;; Flycheck for syntax checking
(use-package flycheck
  :config
  (global-flycheck-mode))

;; Projectile for project management
(use-package projectile
  :diminish
  :config
  (projectile-mode +1)
  ;; Use ido for projectile-find-file so ido-enable-flex-matching applies
  (setq projectile-completion-system 'ido)
  ;; Include git-ignored files (like .env) in projectile-find-file
  (setq projectile-git-command "git ls-files -zco --exclude-standard && git ls-files -zcoi --exclude-standard -- ':!target' ':!node_modules'")
  (setq projectile-globally-ignored-directories
        (append '("target" "node_modules") projectile-globally-ignored-directories))
  (setq projectile-use-git-grep t)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(defun my/find-file-smart ()
  "Use projectile-find-file inside a project, else fall back to find-file."
  (interactive)
  (require 'projectile)
  (if (projectile-project-p)
      (projectile-find-file)
    (call-interactively #'find-file)))

;; Rust support
(use-package rust-mode
  :mode "\\.rs\\'"
  :config
  (setq rust-format-on-save t)
  (setq rust-indent-offset 4)
  (add-hook 'rust-mode-hook #'flycheck-mode))

(use-package cargo
  :diminish cargo-minor-mode
  :hook (rust-mode . cargo-minor-mode))

(defun my/cargo-process-build-release ()
  "Run `cargo build --release' through cargo-process."
  (interactive)
  (require 'cargo-process)
  (cargo-process--start "Build Release" "build --release"))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

;; --- Auto-detect Deno vs Node.js ---
;; Defined before use-package lsp-mode so the setup hook is available
;; for the very first TypeScript/JS file opened in the session.
;; Uses locate-dominating-file (built-in) to avoid dependency on
;; projectile or lsp-workspace-root which may not be loaded yet.
(defun my/project-has-file-p (&rest files)
  "Return non-nil if any of FILES exist in a parent directory."
  (let ((dir (or buffer-file-name default-directory)))
    (cl-some (lambda (f) (locate-dominating-file dir f)) files)))

(defun my/setup-ts-js-lsp ()
  "Configure LSP client per-project: Deno for Deno projects, ts-ls for Node."
  (condition-case err
      (if (my/project-has-file-p "deno.json" "deno.jsonc")
          ;; Deno project
          (progn
            (setq-local lsp-disabled-clients '(ts-ls))
            (setq-local lsp-deno-enable t))
        (if (my/project-has-file-p "package.json" "tsconfig.json")
            ;; Node.js project
            (progn
              (setq-local lsp-disabled-clients '(deno-ls))
              (setq-local lsp-deno-enable nil))
          ;; No marker found — default to Deno
          (setq-local lsp-disabled-clients '(ts-ls))
          (setq-local lsp-deno-enable t)))
    (error (message "my/setup-ts-js-lsp: %s" err))))

;; LSP mode for IDE features (completion, go-to-definition, etc.)
(use-package lsp-mode
  :hook ((rust-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred) ;; For Emacs 29+ tree-sitter users
         (python-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :init
  ;; Default Deno off; my/setup-ts-js-lsp enables it per-project
  (setq lsp-deno-enable nil)
  :config
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-idle-delay 0.5)
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io-mode " *lsp-log*")

  ;; --- Deno Specific Tweaks ---
  (setq lsp-deno-suggest-imports t)
  (setq lsp-deno-suggest-auto-imports t)
  (setq lsp-deno-suggest-complete-function-calls t)
  )

;; Register AFTER use-package :hook so LIFO order ensures setup runs BEFORE lsp-deferred
(add-hook 'typescript-mode-hook #'my/setup-ts-js-lsp)
(add-hook 'js-mode-hook #'my/setup-ts-js-lsp)
(add-hook 'typescript-ts-mode-hook #'my/setup-ts-js-lsp)

;; LSP UI enhancements
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  ;; Auto-popup on cursor stop is expensive in TTY; invoke on demand instead
  ;; via M-x lsp-ui-doc-show / lsp-ui-doc-glance.
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable nil))

(use-package lsp-treemacs
  :after lsp
  :config
  (lsp-treemacs-sync-mode 1))

;; Display flycheck errors as a narrow sidebar on the right
(add-to-list 'display-buffer-alist
             '("\\*Flycheck errors\\*"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.3)
               (slot . 0)))

;; Display grep results (projectile-grep / C-M-f) on the right side
(add-to-list 'display-buffer-alist
             '("\\*grep\\*"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.4)
               (slot . 1)))

;; Display workspace-wide LSP diagnostics panels on the right side
(add-to-list 'display-buffer-alist
             '("\\*lsp-\\(errors\\|warnings\\|hints\\)\\*"
               (display-buffer-in-side-window)
               (side . right)
               (window-width . 0.4)
               (slot . 1)))

;; When jumping from a grep result (RET / mouse-2), reuse the existing
;; main window instead of splitting it. The default compilation path
;; binds `pop-up-windows' to t when the *grep* window is selected, which
;; causes a split — we override the display action to prefer reuse.
(defun my/compilation-goto-locus-reuse-window (orig-fn &rest args)
  (let ((display-buffer-overriding-action
         '((display-buffer-reuse-window
            display-buffer-use-some-window)
           (inhibit-same-window . t))))
    (apply orig-fn args)))
(advice-add 'compilation-goto-locus :around
            #'my/compilation-goto-locus-reuse-window)

(defconst my/lsp-diagnostic-panels
  '((errors   . ("*lsp-errors*"   "errors"   (1)))
    (warnings . ("*lsp-warnings*" "warnings" (2)))
    (hints    . ("*lsp-hints*"    "hints"    (3 4))))
  "Map panel key to (buffer-name label severities).
LSP severities: 1=Error, 2=Warning, 3=Information, 4=Hint.
The `hints' panel covers Info+Hint to match lsp-modeline's third counter.")

(defun my/list-workspace-diagnostics (panel)
  "Collect LSP diagnostics for PANEL into a side-window buffer.
The current file's problems are listed first, then an obvious
divider, then problems for every other file in the workspace."
  (require 'lsp-mode)
  (require 'grep)
  (let* ((spec (cdr (assq panel my/lsp-diagnostic-panels)))
         (buf-name (nth 0 spec))
         (label (nth 1 spec))
         (severities (nth 2 spec))
         ;; Capture the file we are viewing BEFORE switching into `buf'.
         (this-file (buffer-file-name))
         (diags (lsp-diagnostics t))
         (buf (get-buffer-create buf-name))
         (current-lines nil)
         (other-lines nil)
         (current-count 0)
         (other-count 0))
    ;; Partition matching diagnostics into current-file vs. the rest.
    (maphash
     (lambda (file diagnostics)
       (dolist (d diagnostics)
         (when (memq (lsp-get d :severity) severities)
           (let* ((range (lsp-get d :range))
                  (start (lsp-get range :start))
                  (line (1+ (lsp-get start :line)))
                  (col (1+ (lsp-get start :character)))
                  (msg (lsp-get d :message))
                  (entry (format "%s:%d:%d: %s\n" file line col msg)))
             (if (and this-file (file-equal-p file this-file))
                 (progn (push entry current-lines) (cl-incf current-count))
               (push entry other-lines)
               (cl-incf other-count))))))
     diags)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Current file on top.
        (if current-lines
            (dolist (l (nreverse current-lines)) (insert l))
          (insert (format "(no %s in current file)\n" label)))
        ;; Obvious divider, then everything else. No colons here so
        ;; `grep-mode' does not treat the divider as a clickable hit.
        (insert "──────────── other files ────────────\n")
        (if other-lines
            (dolist (l (nreverse other-lines)) (insert l))
          (insert (format "(no %s in other files)\n" label)))
        (goto-char (point-min))
        (grep-mode))
      (setq header-line-format
            (format "LSP %s — %d here / %d total"
                    label current-count (+ current-count other-count))))
    (display-buffer buf)
    (when-let ((win (get-buffer-window buf)))
      (select-window win))))

(defun my/toggle-lsp-diagnostics (panel)
  "Toggle workspace-wide LSP diagnostics panel for PANEL."
  (let* ((buf-name (car (cdr (assq panel my/lsp-diagnostic-panels))))
         (win (get-buffer-window buf-name)))
    (if win
        (delete-window win)
      (my/list-workspace-diagnostics panel))))

(defun my/toggle-lsp-errors ()   (interactive) (my/toggle-lsp-diagnostics 'errors))
(defun my/toggle-lsp-warnings () (interactive) (my/toggle-lsp-diagnostics 'warnings))
(defun my/toggle-lsp-hints ()    (interactive) (my/toggle-lsp-diagnostics 'hints))

;; Python LSP via Pyright (find references, go-to-definition, etc.)
(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

;; TOML support for Cargo.toml files
(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)
         ("Cargo\\.lock\\'" . toml-mode)))

;; Markdown support with live preview
(use-package xterm-color)

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)
         ("\\.mkd\\'" . markdown-mode)
         ("\\.mdown\\'" . markdown-mode)
         ("\\.mkdn\\'" . markdown-mode)
         ("\\.mdwn\\'" . markdown-mode))
  :config
  (setq markdown-command "pandoc")
  :hook (markdown-mode . visual-line-mode))

(defun my/rich-render ()
  "Render current markdown file with rich-cli in a buffer with colors."
  (interactive)
  (let* ((file (buffer-file-name))
         (buf-name "*rich*")
         (width (max 60 (- (window-width) 2))))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (let ((buf (get-buffer-create buf-name)))
      (with-current-buffer buf
        ;; render-md.py wraps Rich and forces tables to expand to full width.
        ;; Two env vars are required:
        ;;   COLUMNS - tells Rich how wide to render.
        ;;   TERM    - Emacs sets TERM=dumb for child processes by default,
        ;;             which makes Rich's `is_dumb_terminal' fire and force
        ;;             an 80x25 fallback, bypassing COLUMNS entirely. Override
        ;;             with a real terminfo entry so COLUMNS is honored.
        (with-environment-variables (("COLUMNS" (number-to-string width))
                                     ("TERM" "xterm-256color"))
          (call-process "python3" nil buf nil
                        (expand-file-name "render-md.py" "~/willy/emacs_setup/")
                        file))
        (insert (xterm-color-filter (delete-and-extract-region (point-min) (point-max))))
        (view-mode 1)
        (local-set-key (kbd "q") 'kill-buffer-and-window))
      (pop-to-buffer buf))))

;; Make "q" kill buffers instead of burying them
(advice-add 'quit-window :around
            (lambda (orig-fun &optional kill window)
              (funcall orig-fun t window)))

(defun my/dired-preview-markdown-rich ()
  "Preview markdown file at point in dired using rich-cli."
  (interactive)
  (let ((file (dired-get-file-for-visit))
        (buf  (get-buffer-create "*rich-preview*")))
    ;; Display the buffer FIRST so window-body-width reflects the real
    ;; text area (frame minus fringes/line-numbers). Render at that width.
    (switch-to-buffer buf)
    (delete-other-windows)
    (display-line-numbers-mode -1)
    (setq-local truncate-lines t)
    (let ((width (max 60 (- (window-body-width) 10)))
          (inhibit-read-only t))
      (erase-buffer)
      ;; See my/rich-render for why both COLUMNS and TERM must be set.
      (with-environment-variables (("COLUMNS" (number-to-string width))
                                   ("TERM" "xterm-256color"))
        (call-process "python3" nil t nil
                      (expand-file-name "render-md.py" "~/willy/emacs_setup/")
                      file))
      (insert (xterm-color-filter (delete-and-extract-region (point-min) (point-max))))
      (goto-char (point-min))
      (special-mode))))

(with-eval-after-load 'dired
  (require 'dired-x)  ;; enables C-x C-j (dired-jump) to open Dired on current file's dir
  (setq dired-kill-when-opening-new-dired-buffer t)
  (define-key dired-mode-map (kbd "M") #'my/dired-preview-markdown-rich)
  ;; "q" goes up to the parent directory (no need to move to ".." and hit RET).
  (define-key dired-mode-map (kbd "q") #'dired-up-directory))

;; Hide file details (permissions, owner, group, size, date) in dired by default.
;; Press "(" inside a dired buffer to toggle them back on when needed.
(add-hook 'dired-mode-hook #'dired-hide-details-mode)

;; Load dired-x at startup so C-x C-j works before any dired buffer is opened
(require 'dired-x)

(setq-default tab-width 4 indent-tabs-mode t)

;; JSON indent level
(setq js-indent-level 2)
(add-hook 'json-ts-mode-hook (lambda () (setq-local tab-width 2)))

;; Toggle indentation style globally (M-i).
;; Flips between "2 spaces" and "1 tab displayed as 4 spaces" for every buffer
;; (existing and future). Only affects future indent commands and how TAB chars
;; display; use M-x tabify / untabify to rewrite what's already in a buffer.
(defvar my/indent-step-vars
  '(c-basic-offset
    js-indent-level js-jsx-indent-level
    typescript-indent-level
    typescript-ts-mode-indent-offset tsx-ts-mode-indent-offset
    js-ts-mode-indent-offset
    python-indent-offset py-indent-offset
    css-indent-offset
    web-mode-code-indent-offset web-mode-markup-indent-offset
    web-mode-css-indent-offset web-mode-attr-indent-offset
    sh-basic-offset sh-indentation
    lua-indent-level
    rust-indent-offset rust-ts-mode-indent-offset
    go-ts-mode-indent-offset
    ruby-indent-level
    sgml-basic-offset)
  "Mode-specific indent-step variables set by `my/toggle-indent-style'.")

(defun my/apply-indent-style-globally (use-tabs width)
  "Set indent style as the new global default and update existing buffers."
  (setq-default indent-tabs-mode use-tabs)
  (setq-default tab-width width)
  (dolist (v my/indent-step-vars)
    (when (boundp v) (set-default v width)))
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (local-variable-p 'indent-tabs-mode)
        (setq indent-tabs-mode use-tabs))
      (when (local-variable-p 'tab-width)
        (setq tab-width width))
      (dolist (v my/indent-step-vars)
        (when (and (boundp v) (local-variable-p v))
          (set v width))))))

(defun my/toggle-indent-style ()
  "Toggle global indent style: 2 spaces <-> 1 tab (4 wide). Affects all buffers."
  (interactive)
  (if (default-value 'indent-tabs-mode)
      (progn (my/apply-indent-style-globally nil 2)
             (message "Indent: 2 spaces (global)"))
    (my/apply-indent-style-globally t 4)
    (message "Indent: tabs (width 4) (global)")))

;; Autopair (not available in melpa, skip)
;; Using electric-pair-mode as alternative
(electric-pair-mode 1)

;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (setq yas-verbosity 0)  ;; Suppress "no snippets found" warning
  (yas-global-mode 1))

;; Optional: install common snippets collection
(use-package yasnippet-snippets
  :after yasnippet)

;; Code folding (hideshow)
(use-package hideshow
  :straight nil  ;; built-in package
  :config
  (defun my/hs-fold-on-load ()
    "Automatically fold the buffer on load, keeping only the first layer expanded."
    (interactive)
    (when (and (boundp 'hs-minor-mode)
               (not (minibufferp)))
      (hs-minor-mode 1)
      (save-excursion
        (goto-char (point-min))
        ;; Use level 2 to keep the first layer (e.g., classes, impl blocks, or
        ;; top-level functions) open, while folding everything inside them.
        ;; Change the argument to 1 if you want to fold the top-level blocks too.
        (hs-hide-level 2))))
  :hook ((rust-mode . my/hs-fold-on-load)
         (typescript-mode . my/hs-fold-on-load)
         (js-mode . my/hs-fold-on-load)
         (python-mode . my/hs-fold-on-load)
         (c-mode . my/hs-fold-on-load)
         (c++-mode . my/hs-fold-on-load)))

;; Company mode disabled — inline ghost text (inline-suggestion-mode) replaces it.
;; LSP completions are still reachable on demand via C-M-i (completion-at-point).
;; (use-package company
;;   :diminish
;;   :hook (after-init . global-company-mode)
;;   :bind (:map company-active-map
;;          ("C-w" . company-select-previous)
;;          ("C-s" . company-select-next)))

;; ==========================================================================
;; Font settings
(condition-case nil
    (set-face-attribute 'default nil
                        :family "JetBrains Mono" :height 170 :weight 'normal)
  (error (message "JetBrains Mono font not available, using default")))


;; Smart scroll function for M-w
(defun smart-scroll-up ()
  "Scroll up one page. If already at top, move cursor to beginning of buffer."
  (interactive)
  (condition-case nil
      (scroll-down-command)
    (beginning-of-buffer
     ;; We're already at the top, move cursor to beginning
     (goto-char (point-min)))))

(defun my/backward-kill-word-or-indent ()
  "Kill the previous word, but never cross the line's beginning.
If `backward-kill-word' would cross the current line's beginning,
only kill back to column 0. If point is already at beginning of
line, just delete the newline (joining with previous line)."
  (interactive)
  (cond
   ((bobp) nil)
   ((bolp) (delete-char -1))
   (t
    (let ((target (save-excursion (backward-word 1) (point)))
          (bol (line-beginning-position)))
      (kill-region (max target bol) (point))))))

;; Kill current buffer without prompting
(defun kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

;; Kill shell buffer and its processes
(defun kill-shell-buffer-and-process ()
  "Kill the shell buffer and all its running processes."
  (interactive)
  (let ((kill-buffer-query-functions nil)
        (buffer (current-buffer)))
    ;; First, handle any running process
    (when (eq major-mode 'shell-mode)
      (let ((proc (get-buffer-process buffer)))
        (when (and proc (process-live-p proc))
          ;; Don't query about the process
          (set-process-query-on-exit-flag proc nil)
          ;; Kill the process hierarchy
          (condition-case nil
              (progn
                (interrupt-process proc t)  ;; Kill process group
                (sit-for 0.1)
                (delete-process proc))
            (error nil)))))
    ;; Now kill the buffer without any prompts
    (set-buffer-modified-p nil)  ;; Mark as unmodified to avoid save prompt
    (kill-buffer buffer)))

(defun my/kill-current-buffer-smart ()
  "Kill the current buffer, including its process when in `shell-mode'."
  (interactive)
  (if (eq major-mode 'shell-mode)
      (kill-shell-buffer-and-process)
    (kill-current-buffer)))

;; hide-comnt (not available in melpa, using comment-dwim instead)
(global-set-key (kbd "C-x m") 'comment-dwim)

;; Spell check - only enable if spell checker is available

;; Check if ispell/aspell/hunspell is available before enabling flyspell
(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "en_US"))
 ((executable-find "ispell")
  (setq ispell-program-name "ispell"))
 (t
  (setq ispell-program-name nil)))

;; Only enable flyspell if a spell checker is found
(when (and (boundp 'ispell-program-name) ispell-program-name (executable-find ispell-program-name))
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (eval-after-load "flyspell"
    '(define-key flyspell-mode-map (kbd "C-c") nil)))

;; Use ibuffer for C-x C-b, hiding *special* buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-saved-filter-groups
      '(("default"
         ("User" (not name . "\\`\\*")))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")
            ;; Collapse the [Default] group (the *special* buffers)
            (setq ibuffer-hidden-filter-groups '("Default"))))

;; Skip *special* and " hidden" buffers when cycling with next-buffer /
;; previous-buffer (bound to C-M-s / C-M-w below).
(setq switch-to-prev-buffer-skip
      (lambda (_window buffer _bury-or-kill)
        (string-match-p "\\`[* ]" (buffer-name buffer))))

;; Line numbers
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode t)
  (when (fboundp 'global-linum-mode)
    (global-linum-mode t)))

;; Custom keybindings
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; Movement keys
(define-key my-keys-minor-mode-map (kbd "C-d") 'forward-char)
(define-key my-keys-minor-mode-map (kbd "C-a") 'backward-char)
(define-key my-keys-minor-mode-map (kbd "C-w") 'previous-line)
(define-key my-keys-minor-mode-map (kbd "C-s") 'next-line)
(define-key my-keys-minor-mode-map (kbd "C-f") 'isearch-forward)
;; Make C-g exit isearch and stay at current match (instead of going back)
(define-key isearch-mode-map (kbd "C-g") 'isearch-exit)
;; Make RET go to next match (wraps around at end)
(define-key isearch-mode-map (kbd "RET") 'isearch-repeat-forward)
(setq isearch-wrap-pause 'no)  ;; Wrap immediately without pausing
(define-key my-keys-minor-mode-map (kbd "C-q") 'back-to-indentation)
(define-key my-keys-minor-mode-map (kbd "C-e") 'move-end-of-line)
(define-key my-keys-minor-mode-map (kbd "M-d") 'forward-word)
(define-key my-keys-minor-mode-map (kbd "M-a") 'backward-word)
(define-key my-keys-minor-mode-map (kbd "M-q") 'delete-backward-char)
(define-key my-keys-minor-mode-map (kbd "M-e") 'delete-forward-char)
;; M-DEL (Meta/Alt + Backspace) kills the previous word, but never
;; crosses the newline before point: if only whitespace precedes
;; point on the current line, just kill the indentation.
(define-key my-keys-minor-mode-map (kbd "M-DEL") 'my/backward-kill-word-or-indent)
(define-key my-keys-minor-mode-map (kbd "M-9") 'beginning-of-buffer)
(define-key my-keys-minor-mode-map (kbd "M-0") 'end-of-buffer)
(define-key my-keys-minor-mode-map (kbd "C-M-s") 'next-buffer)
(define-key my-keys-minor-mode-map (kbd "C-M-w") 'previous-buffer)
(define-key my-keys-minor-mode-map (kbd "C-M-d") 'other-window)
(define-key my-keys-minor-mode-map (kbd "C-x SPC") 'set-mark-command)
(define-key my-keys-minor-mode-map (kbd "M-s") 'scroll-up-command)
(define-key my-keys-minor-mode-map (kbd "M-w") 'smart-scroll-up)
(define-key my-keys-minor-mode-map (kbd "C-j") 'newline-and-indent)
;; C-c semantics: copy when a region is active, otherwise let the
;; major/section keymap handle C-c (so forge's C-c C-c list menu,
;; C-c RET item menu, magit's C-c bindings, comint's C-c C-c, etc.
;; all still work when no region is selected).
;;
;; We use `key-translation-map' because the decision needs to happen
;; BEFORE normal keymap lookup. Forge/magit attach section keymaps via
;; text properties (highest lookup priority), and those section maps
;; inherit `C-c' as a prefix from `forge-common-map' / magit maps.
;; Trying a `menu-item :filter' that returns nil for the no-region
;; case does NOT reliably walk into the parent keymap on every Emacs
;; build (verified empirically here — `C-c C-c' landed unbound), so we
;; sidestep the whole keymap-priority puzzle. When a region is active
;; we rewrite C-c to a private event bound to copy; when no region we
;; return nil and the key sequence is looked up exactly as forge/magit
;; expect.
(defun my/c-c-translation (_prompt)
  "Rewrite C-c to a private copy event when a region is active.
Returns nil otherwise, leaving the original C-c untouched so
forge/magit's `C-c …' prefix keys keep working."
  (and (use-region-p) [my-copy-event]))

(define-key key-translation-map (kbd "C-c") #'my/c-c-translation)
(global-set-key [my-copy-event] #'my-kill-ring-save)
(define-key my-keys-minor-mode-map (kbd "C-v") 'my-smart-paste)
(define-key my-keys-minor-mode-map (kbd "C-x k") 'my/kill-current-buffer-smart)
(define-key my-keys-minor-mode-map (kbd "C-x C-f") 'my/find-file-smart)
(define-key my-keys-minor-mode-map (kbd "C-M-f") 'projectile-grep)
(define-key my-keys-minor-mode-map (kbd "C-M-l") 'hs-toggle-hiding)
(define-key my-keys-minor-mode-map (kbd "C-k") 'kill-whole-line)
;; Toggle workspace-wide LSP diagnostics panels by severity.
;; Each panel lists the current file's problems first, then a
;; divider, then the rest of the workspace.
(define-key my-keys-minor-mode-map (kbd "M-1") 'my/toggle-lsp-errors)    ;; severity 1 (error)
(define-key my-keys-minor-mode-map (kbd "M-2") 'my/toggle-lsp-warnings)  ;; severity 2 (warning)
(define-key my-keys-minor-mode-map (kbd "M-3") 'my/toggle-lsp-hints)     ;; severity 3+4 (info+hint)
;; Code folding keys (hideshow)
(define-key my-keys-minor-mode-map (kbd "M-4") 'hs-hide-all)
(define-key my-keys-minor-mode-map (kbd "M-5") 'hs-show-all)
;; Restore the "only first layer expanded" default view
(define-key my-keys-minor-mode-map (kbd "M-l") 'my/hs-fold-on-load)
;; Toggle indent style (2 spaces <-> 1 tab/4 wide) in current buffer.
;; M-i is used by inline-suggestion below.
(define-key my-keys-minor-mode-map (kbd "M-I") 'my/toggle-indent-style)
;; Window resize - smart split line movement
(defun my/shrink-window-smart ()
  "Move split line left (side-by-side) or up (stacked)."
  (interactive)
  (if (window-full-width-p)
      (shrink-window 1)
    (shrink-window-horizontally 1)))

(defun my/enlarge-window-smart ()
  "Move split line right (side-by-side) or down (stacked)."
  (interactive)
  (if (window-full-width-p)
      (enlarge-window 1)
    (enlarge-window-horizontally 1)))

;; M-[ conflicts with terminal CSI prefix (ESC [), breaking Shift+Tab etc.
(define-key my-keys-minor-mode-map (kbd "M-_") 'my/enlarge-window-smart)
(define-key my-keys-minor-mode-map (kbd "M-+") 'my/shrink-window-smart)

(defun my/save-all-buffers ()
  "Save all modified file-visiting buffers without prompting."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'save-some-buffers)))

;; Hotkey reminder panel, using the same Transient UI as Magit/Forge.
(use-package transient
  :config
  (defun my/projectile-project-available-p ()
    "Return non-nil when Projectile sees a project here."
    (and (fboundp 'projectile-project-p)
         (ignore-errors (projectile-project-p))))

  (defun my/lsp-mode-active-p ()
    "Return non-nil when the current buffer has LSP active."
    (bound-and-true-p lsp-mode))

  (defun my/hs-minor-mode-active-p ()
    "Return non-nil when hideshow is active in this buffer."
    (bound-and-true-p hs-minor-mode))

  (defun my/rust-buffer-p ()
    "Return non-nil when the current buffer is a Rust buffer."
    (derived-mode-p 'rust-mode))

  (defun my/dired-buffer-p ()
    "Return non-nil when the current buffer is a Dired buffer."
    (derived-mode-p 'dired-mode))

  (defun my/git-commit-buffer-p ()
    "Return non-nil when the current buffer is a Git commit buffer."
    (derived-mode-p 'git-commit-mode))

  (transient-define-prefix my/custom-hotkeys ()
    "Show my custom keybindings."
    :transient-non-suffix #'transient--do-call
    :refresh-suffixes t
    [["Edit"
      ("C-f" "search" isearch-forward)
      ("C-v" "paste" my-smart-paste)
      ("C-j" "newline" newline-and-indent)
      ("C-k" "kill line" kill-whole-line)
      ("M-q" "del left" delete-backward-char)
      ("M-e" "del right" delete-forward-char)
      ("M-DEL" "kill word" my/backward-kill-word-or-indent)
      ("C-x m" "comment" comment-dwim)
      ("M-I" "indent style" my/toggle-indent-style)
      (:info "isearch C-g exit")
      (:info "isearch RET next")]
     ["Win / Buf"
      ("C-M-s" "next buf" next-buffer)
      ("C-M-w" "prev buf" previous-buffer)
      ("C-M-d" "other win" other-window)
      ("M-s" "page down" scroll-up-command)
      ("M-w" "page up" smart-scroll-up)
      ("M-_" "enlarge" my/enlarge-window-smart)
      ("M-+" "shrink" my/shrink-window-smart)
      (:info "vterm C-g ESC")
      (:info "vterm S-TAB")]
     ["C-x"
      ("C-x ?" "this panel" my/custom-hotkeys)
      ("C-x SPC" "set mark" set-mark-command)
      ("C-x k" "kill buf/proc" my/kill-current-buffer-smart)
      ("C-x C-f" "find file" my/find-file-smart)
      ("C-x C-b" "ibuffer" ibuffer)
      ("C-x C-s" "save all" my/save-all-buffers)
      ("C-x g" "magit" magit-status)
      ("C-x c" "commit msg" my/llm-commit-message
       :inapt-if-not my/git-commit-buffer-p)
      (:info "Forge row M preview")]
     ["Code"
      (:info "C-c p  Projectile")
      ("C-M-f" "grep" projectile-grep
       :inapt-if-not my/projectile-project-available-p)
      ("C-M-l" "fold" hs-toggle-hiding
       :inapt-if-not my/hs-minor-mode-active-p)
      ("M-4" "hide all folds" hs-hide-all
       :inapt-if-not my/hs-minor-mode-active-p)
      ("M-5" "show all folds" hs-show-all
       :inapt-if-not my/hs-minor-mode-active-p)
      ("M-l" "fold level 1" my/hs-fold-on-load)
      ("M-i" "suggestion" inline-suggestion-toggle)
      ("M-1" "errors" my/toggle-lsp-errors
       :inapt-if-not my/lsp-mode-active-p)
      ("M-2" "warnings" my/toggle-lsp-warnings
       :inapt-if-not my/lsp-mode-active-p)
      ("M-3" "hints" my/toggle-lsp-hints
       :inapt-if-not my/lsp-mode-active-p)]
     ["Rust"
      (:info "C-c copies region")
      ("C-c C-c" "run" cargo-process-run
       :inapt-if-not my/rust-buffer-p)
      ("C-c C-t" "test" cargo-process-test
       :inapt-if-not my/rust-buffer-p)
      ("C-c C-b" "build" cargo-process-build
       :inapt-if-not my/rust-buffer-p)
      ("C-c C-r" "release" my/cargo-process-build-release
       :inapt-if-not my/rust-buffer-p)
      ("C-c C-l" "clippy" cargo-process-clippy
       :inapt-if-not my/rust-buffer-p)
      ("C-c C-d" "doc" cargo-process-doc
       :inapt-if-not my/rust-buffer-p)
      ("C-c C-f" "format" rust-format-buffer
       :inapt-if-not my/rust-buffer-p)]
     ["Dired"
      ("M" "preview md" my/dired-preview-markdown-rich
       :inapt-if-not my/dired-buffer-p)
      ("(" "details" dired-hide-details-mode
       :inapt-if-not my/dired-buffer-p)
      (:info "q     up dir")]]))

(define-key my-keys-minor-mode-map (kbd "C-x ?") #'my/custom-hotkeys)

;; Magit
(use-package magit
  :bind (:map my-keys-minor-mode-map
              ("C-x g" . magit-status))
  :hook ((magit-mode . visual-line-mode))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  ;; Expand fully-untracked directories into individual files so TAB on
  ;; each file shows its content. Default `t' collapses them to a single
  ;; folder line with no body, making TAB appear broken.
  (magit-status-show-untracked-files 'all))

;; Forge - GitHub/GitLab issues & PRs inside Magit.
;; Press `@' in a Magit status buffer to open the Forge popup.
;; Auth: add a line to ~/.authinfo (chmod 600):
;;   machine api.github.com login <user>^forge password <token>
;; For two accounts, add one line per account and set, in each repo:
;;   git config github.user <user>
;; After editing ~/.authinfo run M-x auth-source-forget-all-cached.
;;
;; Multiple GitHub accounts via SSH host aliases: if a repo's remote uses
;; an alias host (e.g. git@pg.github.com:... defined in ~/.ssh/config),
;; forge does not recognize it as GitHub unless we map it here. Each entry
;; is (GITHOST APIHOST WEBHOST CLASS). The token is still looked up under
;; APIHOST (api.github.com) using the repo's `git config github.user'.
(defun my/forge--topic-to-markdown (topic)
  "Build a markdown string for TOPIC: title, original post, then every comment."
  (let ((parts (list (format "# #%s — %s\n"
                             (or (oref topic number) "?")
                             (or (oref topic title) "")))))
    ;; Original post (the topic itself).
    (push (format "**@%s** — %s\n\n%s"
                  (or (oref topic author) "(ghost)")
                  (or (oref topic created) "")
                  (or (oref topic body) "*(no description)*"))
          parts)
    ;; Comments.
    (dolist (post (and (slot-boundp topic 'posts) (oref topic posts)))
      (push "\n\n---\n" parts)
      (push (format "**@%s** — %s\n\n%s"
                    (or (oref post author) "(ghost)")
                    (or (oref post created) "")
                    (or (oref post body) ""))
            parts))
    (mapconcat #'identity (nreverse parts) "\n")))

(defun my/forge-preview-rich ()
  "Render the issue/PR at point (body + all comments) with rich-cli.
Mirrors `my/dired-preview-markdown-rich': stitches the topic's markdown
into one document, pipes it through render-md.py, and shows the
ANSI-colored result. Bound to `M' on forge topic rows so it works in
the Magit status buffer."
  (interactive)
  (require 'forge)
  (let ((topic (forge-current-topic)))
    (unless topic (user-error "No issue or pull-request at point"))
    (let ((buf (get-buffer-create "*forge-rich-preview*"))
          (md  (my/forge--topic-to-markdown topic)))
      (switch-to-buffer buf)
      (delete-other-windows)
      (display-line-numbers-mode -1)
      (setq-local truncate-lines t)
      (let ((width  (max 60 (- (window-body-width) 10)))
            (inhibit-read-only t)
            (tmpfile (make-temp-file "forge-rich-" nil ".md")))
        (unwind-protect
            (progn
              (with-temp-file tmpfile (insert md))
              (erase-buffer)
              ;; See my/rich-render for why both COLUMNS and TERM must be set.
              (with-environment-variables (("COLUMNS" (number-to-string width))
                                           ("TERM" "xterm-256color"))
                (call-process "python3" nil t nil
                              (expand-file-name "render-md.py" "~/willy/emacs_setup/")
                              tmpfile))
              (insert (xterm-color-filter
                       (delete-and-extract-region (point-min) (point-max))))
              (goto-char (point-min))
              (special-mode))
          (delete-file tmpfile))))))

(use-package forge
  :after magit
  :config
  (add-to-list 'forge-alist
               '("pg.github.com" "api.github.com"
                 "github.com" forge-github-repository))
  ;; Shift-M on an issue/PR row → rich-rendered preview (like dired's M).
  ;; Per-section keymaps only fire when point is on that section type,
  ;; so magit's global M (remote popup) is unaffected elsewhere.
  (define-key forge-issue-section-map   (kbd "M") #'my/forge-preview-rich)
  (define-key forge-pullreq-section-map (kbd "M") #'my/forge-preview-rich))

;; LLM commit-message generation (local-llm /commit endpoint).
;; In a Magit commit buffer, C-x c sends the staged diff (git diff --cached)
;; to the local LLM server and inserts the returned one-line message at the
;; top of the buffer. The server (local-llm/server.py) lazy-loads a larger
;; instruction-tuned model on first use, so the first call after an idle gap
;; pays a cold start (~10s); warm calls are quick. Bound only in the commit
;; buffer (C-x c sits one Ctrl-hold from C-x C-c = quit Emacs).
(defvar my/llm-commit-url "http://localhost:8080/commit"
  "Endpoint that turns a git diff into a commit message.")

(defun my/llm--escape-non-ascii (str)
  "Replace non-ASCII chars in STR with \\uXXXX escapes.
Keeps the HTTP request body ASCII-only (avoids Emacs Bug#23750,
the same workaround used by inline-suggestion.el)."
  (replace-regexp-in-string
   "[^[:ascii:]]"
   (lambda (ch) (format "\\u%04x" (string-to-char ch)))
   str nil t))

(defun my/llm--staged-diff ()
  "Return the staged diff (git diff --cached) as a string, or nil if empty."
  (let ((default-directory (or (and (fboundp 'magit-toplevel) (magit-toplevel))
                               default-directory)))
    (with-temp-buffer
      (if (and (= 0 (call-process "git" nil t nil "diff" "--cached" "--no-color"))
               (> (buffer-size) 0))
          (buffer-string)
        nil))))

(defun my/llm--commit-callback (status target)
  "Handle the /commit HTTP response; insert the message into TARGET buffer."
  (let ((http-buf (current-buffer)) msg err)
    (unwind-protect
        (condition-case e
            (progn
              (when (plist-get status :error)
                (error "request failed: %s" (plist-get status :error)))
              (goto-char (point-min))
              (unless (re-search-forward "^HTTP/[0-9.]+ 200" nil t)
                (error "server returned non-200"))
              (goto-char (if (and (boundp 'url-http-end-of-headers)
                                  url-http-end-of-headers)
                             url-http-end-of-headers
                           (point-min)))
              (let* ((json-object-type 'alist)
                     (resp (json-read))
                     (m (alist-get 'message resp))
                     (server-err (alist-get 'error resp)))
                (cond
                 (server-err (error "server error: %s" server-err))
                 ((and m (not (string-empty-p (string-trim m))))
                  (setq msg (string-trim m)))
                 (t (error "empty message")))))
          (error (setq err (error-message-string e))))
      (when (buffer-live-p http-buf) (kill-buffer http-buf)))
    (if msg
        (when (buffer-live-p target)
          (with-current-buffer target
            (save-excursion
              (goto-char (point-min))
              (insert msg "\n"))
            (message "Commit message inserted")))
      (message "LLM commit message failed: %s" (or err "unknown error")))))

(defun my/llm-commit-message ()
  "Generate a commit message from staged changes and insert it at the top."
  (interactive)
  (let ((diff (my/llm--staged-diff)))
    (unless diff
      (user-error "No staged changes to summarize"))
    (let* ((target (current-buffer))
           (json-body (json-encode `(("diff" . ,diff))))
           (url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/json")))
           (url-request-data
            (encode-coding-string (my/llm--escape-non-ascii json-body) 'utf-8))
           (url-show-status nil))
      (message "Generating commit message… (first call after idle is slower)")
      (url-retrieve my/llm-commit-url
                    (lambda (status) (my/llm--commit-callback status target))
                    nil t t))))

;; Bind only in the commit buffer.
(with-eval-after-load 'git-commit
  (define-key git-commit-mode-map (kbd "C-x c") #'my/llm-commit-message)
  ;; Turn FIM ghost text OFF in commit buffers. The commit buffer is a
  ;; `text-mode' buffer, so `inline-suggestion-mode' would otherwise be on and
  ;; fire /infill as you type — which, with the server's single NPU slot,
  ;; evicts the commit model and makes every C-x c a cold (~10s) load instead
  ;; of a warm (~4s) one. (Code completion in a commit message is useless too.)
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (when (bound-and-true-p inline-suggestion-mode)
                (inline-suggestion-mode -1)))))

;; diff-hl - git diff indicators in the fringe (like VS Code's gutter colors)
(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (diff-hl-flydiff-mode 1)
  ;; Auto-refresh diff-hl when git state changes outside Emacs
  ;; (idle timer catches external git add/commit/push)
  (run-with-idle-timer 3 t
    (lambda ()
      (when (and (bound-and-true-p diff-hl-mode) buffer-file-name)
        (diff-hl-update))))
  ;; Also refresh when switching buffers
  (add-hook 'window-buffer-change-functions
            (lambda (_frame)
              (when (and (bound-and-true-p diff-hl-mode) buffer-file-name)
                (diff-hl-update)))))

;; Rust keybindings
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-c C-c") 'cargo-process-run)
  (define-key rust-mode-map (kbd "C-c C-t") 'cargo-process-test)
  (define-key rust-mode-map (kbd "C-c C-b") 'cargo-process-build)
  (define-key rust-mode-map (kbd "C-c C-r") 'my/cargo-process-build-release)
  (define-key rust-mode-map (kbd "C-c C-l") 'cargo-process-clippy)
  (define-key rust-mode-map (kbd "C-c C-d") 'cargo-process-doc)
  (define-key rust-mode-map (kbd "C-c C-f") 'rust-format-buffer))

;; Global key settings
(global-set-key (kbd "C-x C-s") #'my/save-all-buffers)

;; Free M-{ and M-} so tmux can use them for pane resize at root level.
;; (Emacs defaults: backward-paragraph / forward-paragraph — no longer used.)
(global-unset-key (kbd "M-{"))
(global-unset-key (kbd "M-}"))
;; Scroll page up/down now uses Option+s/w (M-s/M-w) defined above
;; (global-set-key (kbd "s-s") (kbd "C-u 1 M-s"))  ; Command+s removed
;; (global-set-key (kbd "s-w") (kbd "C-u 1 M-w"))  ; Command+w removed

;; Define minor mode
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter ""
  :keymap my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;; Ensure my-keys-minor-mode has highest priority
(defvar my-keys-minor-mode-map-alist `((my-keys-minor-mode . ,my-keys-minor-mode-map)))
(add-to-list 'emulation-mode-map-alists 'my-keys-minor-mode-map-alist)

;; Minibuffer setup
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))

(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)

;; Shell mode setup to restore C-c C-c functionality
(add-hook 'shell-mode-hook
          (lambda ()
            ;; Prevent command echoing in shell
            (setq comint-process-echoes t)
            ;; Create a local keymap that inherits from my-keys-minor-mode-map
            (let ((shell-my-keys-map (make-sparse-keymap)))
              (set-keymap-parent shell-my-keys-map my-keys-minor-mode-map)
              ;; Remove the C-c binding in this local map
              (define-key shell-my-keys-map (kbd "C-c") nil)
              (define-key shell-my-keys-map (kbd "C-x k") #'kill-shell-buffer-and-process)
              ;; Use this modified keymap in shell mode
              (add-to-list 'minor-mode-overriding-map-alist
                           `(my-keys-minor-mode . ,shell-my-keys-map)))
            ;; Ensure C-x k kills shell buffer and its processes
            (local-set-key (kbd "C-x k") 'kill-shell-buffer-and-process)
            ;; Don't ask for confirmation when killing shell
            (let ((proc (get-buffer-process (current-buffer))))
              (when proc
                (set-process-query-on-exit-flag proc nil)))))

;; Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(menu-bar-mode nil)
 '(package-selected-packages nil)
 '(tool-bar-mode nil))

(custom-set-faces
 )


;; Vterm - full terminal emulator
(use-package vterm
  :commands vterm
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 10000)
  ;; Drop the leading "*" so vterm buffers participate in C-M-s / C-M-w rotation
  ;; (switch-to-prev-buffer-skip filters out buffers starting with "*" or space).
  (setq vterm-buffer-name "vterm")
  (setq vterm-keymap-exceptions '("C-x" "C-u" "C-g" "C-h" "M-x" "M-o"))
  ;; ESC bindings for GUI Emacs
  (define-key vterm-mode-map [escape] #'vterm-send-escape)
  ;; C-g sends ESC to vterm processes (e.g. Claude Code)
  ;; In terminal Emacs, ESC is the Meta prefix and can't be rebound,
  ;; so C-g is the way to send ESC
  (define-key vterm-mode-map (kbd "C-g") #'vterm-send-escape)
  ;; Forward Shift+Tab to vterm for Claude Code mode switching
  (define-key vterm-mode-map [backtab]
    (lambda () (interactive) (vterm-send-string "\e[Z")))
  :hook (vterm-mode . (lambda () (display-line-numbers-mode -1))))


;; Inline ghost text suggestions (Cursor-style, FIM via local rkllm shim).
;; Backend: server.py in /home/m6/willy/emacs_setup/local-llm/ — start with ./start.sh.
;; The plugin source lives in this same repo, loaded directly (no straight clone).
(use-package inline-suggestion
  :straight nil
  :diminish
  :load-path "~/willy/emacs_setup/inline-suggestion"
  :init
  ;; We run the rkllm shim manually; do not let the plugin try to start
  ;; llama-server. Point at our /infill endpoint and lower the token cap
  ;; to keep ghost text snappy on the NPU (~15 tok/s decode).
  (setq inline-suggestion-server-autostart nil)
  (setq inline-suggestion-server-url "http://localhost:8080")
  (setq inline-suggestion-max-tokens 10)
  ;; Trim context to keep prefill cheap on the NPU. Defaults are 50/20,
  ;; but for short FIM completions 15/5 is usually enough signal.
  (setq inline-suggestion-max-prefix-lines 15)
  (setq inline-suggestion-max-suffix-lines 5)
  :bind (:map my-keys-minor-mode-map
         ("M-i" . inline-suggestion-toggle))
  :hook ((prog-mode . inline-suggestion-mode)
         (text-mode . inline-suggestion-mode)))
