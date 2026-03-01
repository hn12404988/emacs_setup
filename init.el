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

;; Mac-specific settings - use Option as Meta in terminal
(when (eq system-type 'darwin)
  (unless (display-graphic-p)
    (setq mac-option-key-is-meta t
          mac-option-modifier 'meta)))

;; Enable clipboard integration
(setq select-enable-clipboard t)
(setq select-enable-primary t)

;; macOS clipboard support for terminal Emacs
(defun copy-to-macos-clipboard ()
  "Copy region to macOS clipboard."
  (interactive)
  (when (use-region-p)
    (shell-command-on-region (region-beginning) (region-end) "pbcopy")
    (message "Copied to clipboard")
    (deactivate-mark)))

(defun my-smart-paste ()
  "Sync system clipboard to kill-ring, then paste."
  (interactive)
  (let ((clipboard-text (shell-command-to-string "pbpaste")))
    ;; If clipboard has content and it's different from the last kill
    (when (and (not (string-empty-p clipboard-text))
               (not (string= clipboard-text (car kill-ring))))
      ;; Add clipboard content to kill-ring
      (kill-new clipboard-text)))
  ;; Now yank (which will use the most recent kill-ring entry)
  (yank))

;; Override the C-c binding to use macOS clipboard
(defun my-kill-ring-save ()
  "Copy to both kill ring and system clipboard."
  (interactive)
  (when (use-region-p)
    (kill-ring-save (region-beginning) (region-end))
    (copy-to-macos-clipboard)))

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

;; color-identifiers-mode (enable per-mode instead of globally for performance)
(use-package color-identifiers-mode
  :diminish
  :hook ((rust-mode . color-identifiers-mode)
         (js-mode . color-identifiers-mode)
         (typescript-mode . color-identifiers-mode)))

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

;; LSP mode for IDE features (completion, go-to-definition, etc.)
(use-package lsp-mode
  :hook ((rust-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-idle-delay 0.5)
  ;; Auto-import projects without prompting
  (setq lsp-auto-guess-root t)
  ;; Hide LSP buffers (space prefix = hidden from C-x b)
  (setq lsp-log-io-mode " *lsp-log*")
  ;; Disable features you might find noisy (uncomment if needed)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)
  )

;; LSP UI enhancements
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable nil))

;; TOML support for Cargo.toml files
(use-package toml-mode
  :mode (("\\.toml\\'" . toml-mode)
         ("Cargo\\.lock\\'" . toml-mode)))

;; Markdown support with live preview
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
        (shell-command (format "CLICOLOR_FORCE=1 glow -w 0 %s" (shell-quote-argument file)) buf)
        (ansi-color-apply-on-region (point-min) (point-max))
        (view-mode 1)
        (local-set-key (kbd "q") 'kill-buffer-and-window))
      (pop-to-buffer buf))))


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

;; Company mode - disabled in favor of inline-suggestion
;; (use-package company
;;   :hook (prog-mode . company-mode)
;;   :config
;;   (setq company-idle-delay 0.2)
;;   (setq company-minimum-prefix-length 1)
;;   (setq company-selection-wrap-around t)
;;   :bind (:map company-active-map
;;               ("C-w" . company-select-previous)
;;               ("C-s" . company-select-next)))

;; ==========================================================================
;; PIP-BOY THEME - Vault-Tec Approved Terminal Display
;; Green phosphor CRT aesthetic inspired by the RobCo Pip-Boy 3000
;; ==========================================================================

;; Color palette
(defvar pipboy-green       "#20C20E" "Primary phosphor green - main text.")
(defvar pipboy-bright      "#33FF33" "Bright green - keywords, highlights.")
(defvar pipboy-medium      "#18a010" "Medium green - types, functions.")
(defvar pipboy-dim         "#0f6e0a" "Dim green - comments, secondary text.")
(defvar pipboy-dark        "#0a3d08" "Dark green - subtle backgrounds.")
(defvar pipboy-bg          "#0a0a0a" "Near-black background.")
(defvar pipboy-bg-light    "#111611" "Slightly lighter background for contrast.")
(defvar pipboy-region      "#1a3a1a" "Selection/region background.")
(defvar pipboy-amber       "#FF8C00" "Amber - warnings, Pip-Boy alert color.")
(defvar pipboy-red         "#CC3333" "Red - errors only.")
;; Modern syntax colors (colorful but still feels techy on the green CRT)
(defvar pipboy-cyan        "#61DAFB" "Cyan - functions.")
(defvar pipboy-gold        "#E5C07B" "Gold - strings.")
(defvar pipboy-teal        "#4EC9B0" "Teal - types.")
(defvar pipboy-orange      "#D19A66" "Warm orange - constants, numbers.")
(defvar pipboy-blue        "#9CDCFE" "Light blue - variables.")
(defvar pipboy-purple      "#C586C0" "Soft purple - builtins, preprocessor.")
(defvar pipboy-doc-green   "#6A9955" "Muted green - doc strings.")

;; Frame defaults
(add-to-list 'default-frame-alist `(foreground-color . ,pipboy-green))
(add-to-list 'default-frame-alist `(background-color . ,pipboy-bg))

;; Font settings
(condition-case nil
    (set-face-attribute 'default nil
                        :family "JetBrains Mono" :height 170 :weight 'normal)
  (error (message "JetBrains Mono font not available, using default")))

;; Apply Pip-Boy faces after init to ensure they override everything
(defun pipboy-apply-faces ()
  "Apply Pip-Boy green phosphor CRT faces."

  ;; Core faces
  (set-face-attribute 'default nil
                      :foreground pipboy-green :background pipboy-bg)
  (set-face-attribute 'cursor nil
                      :background pipboy-bright)
  (set-face-attribute 'region nil
                      :background pipboy-region :foreground pipboy-bright)
  (set-face-attribute 'highlight nil
                      :background pipboy-region)
  (set-face-attribute 'fringe nil
                      :background pipboy-bg :foreground pipboy-dark)
  (set-face-attribute 'vertical-border nil
                      :foreground pipboy-dark)
  (set-face-attribute 'minibuffer-prompt nil
                      :foreground pipboy-bright :weight 'bold)
  (set-face-attribute 'link nil
                      :foreground pipboy-bright :underline t)
  (set-face-attribute 'link-visited nil
                      :foreground pipboy-medium :underline t)
  (set-face-attribute 'shadow nil
                      :foreground pipboy-dim)

  ;; Line numbers
  (set-face-attribute 'line-number nil
                      :foreground pipboy-dark :background pipboy-bg)
  (set-face-attribute 'line-number-current-line nil
                      :foreground pipboy-bright :background pipboy-bg-light
                      :weight 'bold)

  ;; Mode line - Pip-Boy status bar
  (set-face-attribute 'mode-line nil
                      :background "#0f3d0a"
                      :foreground pipboy-bright)
  (set-face-attribute 'mode-line-inactive nil
                      :background pipboy-bg-light
                      :foreground pipboy-dim)
  (set-face-attribute 'mode-line-buffer-id nil
                      :foreground pipboy-bright :weight 'bold)

  ;; Syntax highlighting - modern colorful on Pip-Boy CRT
  (set-face-attribute 'font-lock-keyword-face nil
                      :foreground pipboy-bright :weight 'bold)
  (set-face-attribute 'font-lock-function-name-face nil
                      :foreground pipboy-cyan)
  (set-face-attribute 'font-lock-variable-name-face nil
                      :foreground pipboy-blue)
  (set-face-attribute 'font-lock-string-face nil
                      :foreground pipboy-gold)
  (set-face-attribute 'font-lock-comment-face nil
                      :foreground pipboy-dim :slant 'italic)
  (set-face-attribute 'font-lock-comment-delimiter-face nil
                      :foreground pipboy-dim :slant 'italic)
  (set-face-attribute 'font-lock-type-face nil
                      :foreground pipboy-teal :weight 'bold)
  (set-face-attribute 'font-lock-constant-face nil
                      :foreground pipboy-orange)
  (set-face-attribute 'font-lock-builtin-face nil
                      :foreground pipboy-purple)
  (set-face-attribute 'font-lock-preprocessor-face nil
                      :foreground pipboy-purple)
  (set-face-attribute 'font-lock-warning-face nil
                      :foreground pipboy-amber :weight 'bold)
  (set-face-attribute 'font-lock-doc-face nil
                      :foreground pipboy-doc-green :slant 'italic)

  ;; Search
  (set-face-attribute 'isearch nil
                      :background pipboy-bright :foreground pipboy-bg
                      :weight 'bold)
  (set-face-attribute 'lazy-highlight nil
                      :background pipboy-dark :foreground pipboy-bright)
  (set-face-attribute 'isearch-fail nil
                      :background pipboy-red :foreground pipboy-bg)

  ;; Paren matching
  (set-face-attribute 'show-paren-match nil
                      :background pipboy-dark :foreground pipboy-bright
                      :weight 'bold)
  (set-face-attribute 'show-paren-mismatch nil
                      :background pipboy-red :foreground pipboy-bg)

  ;; Flycheck / errors & warnings
  (when (facep 'flycheck-error)
    (set-face-attribute 'flycheck-error nil
                        :underline `(:style wave :color ,pipboy-red))
    (set-face-attribute 'flycheck-warning nil
                        :underline `(:style wave :color ,pipboy-amber))
    (set-face-attribute 'flycheck-info nil
                        :underline `(:style wave :color ,pipboy-dim)))

  ;; Completions
  (when (facep 'completions-common-part)
    (set-face-attribute 'completions-common-part nil
                        :foreground pipboy-bright :weight 'bold)
    (set-face-attribute 'completions-first-difference nil
                        :foreground pipboy-amber))

  ;; Markdown
  (when (facep 'markdown-header-face-1)
    (set-face-attribute 'markdown-header-face-1 nil
                        :foreground pipboy-bright :weight 'bold :height 1.3)
    (set-face-attribute 'markdown-header-face-2 nil
                        :foreground pipboy-medium :weight 'bold :height 1.2)
    (set-face-attribute 'markdown-header-face-3 nil
                        :foreground pipboy-dim :weight 'bold :height 1.1))

  ;; Magit
  (when (facep 'magit-section-heading)
    (set-face-attribute 'magit-section-heading nil
                        :foreground pipboy-bright :weight 'bold)
    (set-face-attribute 'magit-diff-added nil
                        :foreground pipboy-bright :background "#0a1f0a")
    (set-face-attribute 'magit-diff-removed nil
                        :foreground pipboy-red :background "#1f0a0a")
    (set-face-attribute 'magit-diff-added-highlight nil
                        :foreground pipboy-bright :background "#0f2f0f")
    (set-face-attribute 'magit-diff-removed-highlight nil
                        :foreground pipboy-red :background "#2f0f0f"))

  ;; LSP UI
  (when (facep 'lsp-ui-doc-background)
    (set-face-attribute 'lsp-ui-doc-background nil
                        :background pipboy-bg-light))
  (when (facep 'lsp-face-highlight-textual)
    (set-face-attribute 'lsp-face-highlight-textual nil
                        :background pipboy-region))
  )

(add-hook 'after-init-hook #'pipboy-apply-faces)
;; Also apply to new frames
(add-hook 'server-after-make-frame-hook #'pipboy-apply-faces)


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

(define-key my-keys-minor-mode-map (kbd "C-,") 'my/shrink-window-smart)
(define-key my-keys-minor-mode-map (kbd "C-.") 'my/enlarge-window-smart)

;; Magit
(use-package magit
  :bind (:map my-keys-minor-mode-map
              ("C-x g" . magit-status)))

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
 ;; Pip-Boy theme handles faces via pipboy-apply-faces
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
  ;; Bind C-g to send ESC (reliable in both terminal and GUI)
  (define-key vterm-mode-map (kbd "C-g") #'vterm-send-escape)
  ;; Also bind C-c C-e as alternative
  (define-key vterm-mode-map (kbd "C-c C-e") #'vterm-send-escape))


;; Inline ghost text suggestions (Cursor-style, via local llama.cpp FIM)
(use-package inline-suggestion
  :straight nil
  :diminish
  :load-path "~/.emacs.d/straight/repos/emacs_setup/inline-suggestion"
  :init
  ;; Clone/update from GitHub via straight (the whole repo)
  (straight-use-package
   '(emacs_setup :host github :repo "hn12404988/emacs_setup" :no-build t))
  :hook ((prog-mode . inline-suggestion-mode)
         (text-mode . inline-suggestion-mode)))
