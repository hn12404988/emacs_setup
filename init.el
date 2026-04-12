;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

;; ============================================================================
;; NEW LAPTOP SETUP NOTES
;; ============================================================================
;; This config is fully portable. All Emacs packages auto-install on first run.
;;
;; External dependencies (optional - config degrades gracefully without them):
;;   brew install pandoc      # Markdown preview
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
    (when matching-text (message matching-text))))
(advice-add 'show-paren-function :after #'my/show-matching-paren-offscreen)

;;clock
(display-time-mode 1)
(setq display-time-day-and-date 1)
(setq display-time-24hr-format 1)

;;ido
(ido-mode 1)
(setq ido-default-buffer-method 'selected-window)
;; Hide *special* buffers from C-x b
(setq ido-ignore-buffers '("\\` " "\\`\\*.*\\*\\'"))

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
  ;; Include git-ignored files (like .env) in projectile-find-file
  (setq projectile-git-command "git ls-files -zco --exclude-standard && git ls-files -zcoi --exclude-standard -- ':!target' ':!node_modules'")
  (setq projectile-globally-ignored-directories
        (append '("target" "node_modules") projectile-globally-ignored-directories))
  (setq projectile-use-git-grep t)
  :bind-keymap
  ("C-c p" . projectile-command-map))

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
         (python-mode . lsp-deferred))
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
  (setq lsp-ui-doc-enable t)
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

(defun my/toggle-flycheck-errors ()
  "Toggle the flycheck errors sidebar."
  (interactive)
  (let ((win (get-buffer-window "*Flycheck errors*")))
    (if win
        (delete-window win)
      (flycheck-list-errors)
      (other-window 1))))

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

(defun my/glow-render ()
  "Render current markdown file with glow in a buffer with colors."
  (interactive)
  (let* ((file (buffer-file-name))
         (buf-name "*glow*"))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (let ((buf (get-buffer-create buf-name)))
      (with-current-buffer buf
        (shell-command (if (eq system-type 'darwin)
                           (format "script -q /dev/null sh -c 'TERM=xterm-256color COLORTERM=truecolor glow -w 0 %s'" (shell-quote-argument file))
                         (format "script -q -c 'TERM=xterm-256color COLORTERM=truecolor glow -w 0 %s' /dev/null" (shell-quote-argument file)))
                       buf)
        (insert (xterm-color-filter (delete-and-extract-region (point-min) (point-max))))
        (view-mode 1)
        (local-set-key (kbd "q") 'kill-buffer-and-window))
      (pop-to-buffer buf))))

;; Make "q" kill buffers instead of burying them
(advice-add 'quit-window :around
            (lambda (orig-fun &optional kill window)
              (funcall orig-fun t window)))

(defun my/dired-preview-markdown-glow ()
  "Preview markdown file at point in dired using glow."
  (interactive)
  (let* ((file (dired-get-file-for-visit))
         (buf (get-buffer-create "*glow-preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (eq system-type 'darwin)
            (call-process "script" nil t nil "-q" "/dev/null"
                          "sh" "-c" (format "TERM=dumb COLORTERM=truecolor glow -s dark -w 0 %s"
                                            (shell-quote-argument file)))
          (call-process "script" nil t nil "-q" "-c"
                        (format "TERM=dumb COLORTERM=truecolor glow -s dark -w 0 %s"
                                (shell-quote-argument file))
                        "/dev/null"))
        ;; Strip pseudo-TTY control characters before color processing
        (goto-char (point-min))
        (while (re-search-forward "[\004\010\015]" nil t)
          (replace-match ""))
        (insert (xterm-color-filter (delete-and-extract-region (point-min) (point-max))))
        (goto-char (point-min)))
      (special-mode)
      )
    (switch-to-buffer buf)))

(with-eval-after-load 'dired
  (setq dired-kill-when-opening-new-dired-buffer t)
  (define-key dired-mode-map (kbd "M") #'my/dired-preview-markdown-glow))

(setq-default tab-width 4 indent-tabs-mode t)

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

;; Company mode - navigate suggestions with C-w / C-s
(use-package company
  :diminish
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
         ("C-w" . company-select-previous)
         ("C-s" . company-select-next)))

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

;; Smart backward kill: word -> line start -> backspace
(defun smart-backward-kill ()
  "Smart backward kill with three behaviors:
1. If there's a word to the left, kill the word (backward-kill-word)
2. If at the start of text (only whitespace to left), kill to line start (column 0)
3. If already at column 0, act like backspace (join with previous line)"
  (interactive)
  (cond
   ;; At column 0: act like backspace
   ((zerop (current-column))
    (if (bobp)
        (message "Beginning of buffer")
      (delete-backward-char 1)))
   ;; Only whitespace to the left: kill to line start
   ((save-excursion
      (skip-chars-backward " \t")
      (bolp))
    (kill-line 0))
   ;; Otherwise: kill word
   (t
    (backward-kill-word 1))))

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
         ("User" (not name . "\\`\\*.*\\*\\'")))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")
            ;; Collapse the [Default] group (the *special* buffers)
            (setq ibuffer-hidden-filter-groups '("Default"))))

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
;; Multiple bindings for C-backspace with smart behavior
(define-key my-keys-minor-mode-map (kbd "C-DEL") 'smart-backward-kill)
(define-key my-keys-minor-mode-map (kbd "<C-backspace>") 'smart-backward-kill)
(define-key my-keys-minor-mode-map [C-backspace] 'smart-backward-kill)
(global-set-key (kbd "<C-backspace>") 'smart-backward-kill)
;; In terminals, C-backspace sends C-h (ASCII 8), so bind that too
;; Help is still available via F1
(define-key my-keys-minor-mode-map (kbd "C-h") 'smart-backward-kill)
;; Try M-DEL as an alternative (Meta/Alt + backspace)
(define-key my-keys-minor-mode-map (kbd "M-DEL") 'backward-kill-word)
;; In terminal, Ctrl+Backspace sends C-h (the help prefix), so rebind it.
;; Help remains accessible via F1.
(unless (display-graphic-p)
  (define-key my-keys-minor-mode-map (kbd "C-h") 'backward-kill-word))
(define-key my-keys-minor-mode-map (kbd "M-9") 'beginning-of-buffer)
(define-key my-keys-minor-mode-map (kbd "M-0") 'end-of-buffer)
(define-key my-keys-minor-mode-map (kbd "C-M-s") 'next-buffer)
(define-key my-keys-minor-mode-map (kbd "C-M-w") 'previous-buffer)
(define-key my-keys-minor-mode-map (kbd "C-M-d") 'other-window)
(define-key my-keys-minor-mode-map (kbd "C-x SPC") 'set-mark-command)
(define-key my-keys-minor-mode-map (kbd "M-s") 'scroll-up-command)
(define-key my-keys-minor-mode-map (kbd "M-w") 'smart-scroll-up)
(define-key my-keys-minor-mode-map (kbd "C-j") 'newline-and-indent)
(define-key my-keys-minor-mode-map (kbd "C-c") 'my-kill-ring-save)
(define-key my-keys-minor-mode-map (kbd "C-v") 'my-smart-paste)
(define-key my-keys-minor-mode-map (kbd "C-x k") 'kill-current-buffer)
(define-key my-keys-minor-mode-map (kbd "C-x C-f") 'projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "C-M-f") 'projectile-grep)
(define-key my-keys-minor-mode-map (kbd "C-k") 'kill-whole-line)
;; Toggle flycheck errors sidebar
(define-key my-keys-minor-mode-map (kbd "M-1") 'my/toggle-flycheck-errors)
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

;; Magit
(use-package magit
  :bind (:map my-keys-minor-mode-map
              ("C-x g" . magit-status))
  :hook ((magit-mode . visual-line-mode))
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; diff-hl - git diff indicators in the fringe (like VS Code's gutter colors)
(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
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
  (define-key rust-mode-map (kbd "C-c C-r") 'cargo-process-build-release)
  (define-key rust-mode-map (kbd "C-c C-l") 'cargo-process-clippy)
  (define-key rust-mode-map (kbd "C-c C-d") 'cargo-process-doc)
  (define-key rust-mode-map (kbd "C-c C-f") 'rust-format-buffer))

;; Global key settings
(global-set-key (kbd "C-x C-s") (kbd "C-u C-x s"))
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


;; Inline ghost text suggestions (Cursor-style, via local llama.cpp FIM)
;; Disabled - not needed; also prevents LLM server from starting on Emacs launch
;; (use-package inline-suggestion
;;   :straight nil
;;   :diminish
;;   :load-path "~/.emacs.d/straight/repos/emacs_setup/inline-suggestion"
;;   :init
;;   ;; Clone/update from GitHub via straight (the whole repo)
;;   (straight-use-package
;;    '(emacs_setup :host github :repo "hn12404988/emacs_setup" :no-build t))
;;   :bind (:map my-keys-minor-mode-map
;;          ("M-i" . inline-suggestion-toggle))
;;   :hook ((prog-mode . inline-suggestion-mode)
;;          (text-mode . inline-suggestion-mode)))
