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
  :config
  (projectile-mode +1)
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
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-diagnostics t))

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


(setq-default tab-width 4 indent-tabs-mode t)

;; Autopair (not available in melpa, skip)
;; Using electric-pair-mode as alternative
(electric-pair-mode 1)

;; yasnippet
(use-package yasnippet
  :config
  (setq yas-verbosity 0)  ;; Suppress "no snippets found" warning
  (yas-global-mode 1))

;; Optional: install common snippets collection
(use-package yasnippet-snippets
  :after yasnippet)

;; Company mode (completion framework, works great with LSP)
(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t)
  :bind (:map company-active-map
              ("C-w" . company-select-previous)
              ("C-s" . company-select-next)))

;; Color theme
(add-to-list 'default-frame-alist '(foreground-color . "#E0DFDB"))
(add-to-list 'default-frame-alist '(background-color . "black"))

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
(define-key my-keys-minor-mode-map (kbd "C-q") 'beginning-of-line)
(define-key my-keys-minor-mode-map (kbd "M-d") 'forward-word)
(define-key my-keys-minor-mode-map (kbd "M-a") 'backward-word)
(define-key my-keys-minor-mode-map (kbd "M-q") 'delete-backward-char)
(define-key my-keys-minor-mode-map (kbd "M-e") 'delete-forward-char)
;; Multiple bindings for C-backspace to ensure it works
(define-key my-keys-minor-mode-map (kbd "C-DEL") 'backward-kill-word)
(define-key my-keys-minor-mode-map (kbd "<C-backspace>") 'backward-kill-word)
(define-key my-keys-minor-mode-map [C-backspace] 'backward-kill-word)
(global-set-key (kbd "<C-backspace>") 'backward-kill-word)
;; Try M-DEL as an alternative (Meta/Alt + backspace)
(define-key my-keys-minor-mode-map (kbd "M-DEL") 'backward-kill-word)
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
  :lighter " my-keys"
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
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-header-face-1 ((t (:inherit font-lock-function-name-face :weight bold :height 1.3))))
 '(markdown-header-face-2 ((t (:inherit font-lock-variable-name-face :weight bold :height 1.2))))
 '(markdown-header-face-3 ((t (:inherit font-lock-keyword-face :weight bold :height 1.1)))))


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

;; Eat - Emulate A Terminal (alternative to vterm, may have less flicker)
(use-package eat
  :commands eat
  :config
  (setq eat-kill-buffer-on-exit t)
  ;; Enable shell integration for better experience
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  ;; ESC handling
  (define-key eat-semi-char-mode-map (kbd "C-g") #'eat-self-input)
  (define-key eat-semi-char-mode-map (kbd "C-c C-e") #'eat-self-input))

;; inheritenv - required dependency for claude-code.el
(use-package inheritenv
  :straight (:host github :repo "purcell/inheritenv"))

;; Claude Code integration
(use-package claude-code
  :straight (:host github :repo "stevemolitor/claude-code.el")
  :demand t
  :config
  (setq claude-code-terminal-backend 'eat)  ;; Use eat instead of vterm
  (claude-code-mode)

  (defun my/claude-code-start-dev ()
    "Start Claude Code with CLAUDE.dev.md"
    (interactive)
    (let ((claude-code-program-switches '("--system-prompt-file" "./CLAUDE.dev.md")))
      (claude-code)))
  :bind-keymap ("C-c l" . claude-code-command-map))
;; Key bindings (prefix C-c l):
;;   C-c l c - Start Claude
;;   C-c l s - Send command via minibuffer
;;   C-c l r - Send region/buffer
;;   C-c l e - Fix error at point
;;   C-c l t - Toggle Claude window
;;   C-c l b - Switch to Claude buffer
;;   C-c l m - Transient menu
