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
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq warning-suppress-types '((comp) (bytecomp)))

;; Disable backup files (the ones ending with ~)
(setq make-backup-files nil)

;; Disable auto-save files (the ones with #...#)
(setq auto-save-default nil)

;; Fix PATH for macOS GUI Emacs FIRST before anything else (portable version)
(when (eq system-type 'darwin)
  ;; Ensure cargo/rustup binaries are in exec-path using HOME variable
  (let ((cargo-bin (expand-file-name "~/.cargo/bin")))
    (when (file-directory-p cargo-bin)
      (unless (member cargo-bin exec-path)
        (setenv "PATH" (concat cargo-bin ":" (getenv "PATH")))
        (push cargo-bin exec-path))))
  ;; Also add common macOS paths if they exist
  (dolist (path '("/usr/local/bin" "/opt/homebrew/bin"))
    (when (file-directory-p path)
      (unless (member path exec-path)
        (setenv "PATH" (concat path ":" (getenv "PATH")))
        (push path exec-path)))))

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

;; Package management setup
(require 'package)
(require 'cl-lib)  ;; For cl-remove-if and other cl functions
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure use-package
(require 'use-package)
(setq use-package-always-ensure t)  ;; Auto-install packages

;; Integrate straight.el with use-package
(straight-use-package 'use-package)

;; Suppress compilation warnings from packages
(setq native-comp-async-report-warnings-errors nil)
(setq warning-minimum-level :error)


;;highlight bracket
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                            (char-equal (char-syntax cb) ?\))
                            (blink-matching-open))))
    (when matching-text (message matching-text))))
(show-paren-mode 1)

;;clock
(display-time-mode 1)
(setq display-time-day-and-date 1)
(setq display-time-24hr-format 1)

;;ido
(ido-mode 1)
(setq ido-default-buffer-method 'selected-window)

;; Mac-specific settings
(when (eq system-type 'darwin)
  ;; GUI Emacs settings
  (when (display-graphic-p)
    (setq mac-option-key-is-meta nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta
          mac-option-modifier 'none))
  
  ;; Terminal Emacs settings
  (unless (display-graphic-p)
    ;; Use Option as Meta in terminal
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

(defun paste-from-macos-clipboard ()
  "Paste from macOS clipboard."
  (interactive)
  (insert (shell-command-to-string "pbpaste")))

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

;; Disable startup messages
(setq-default message-log-max nil)
(when (get-buffer "*Messages*")
  (kill-buffer "*Messages*"))

;; exec-path-from-shell - ensure Emacs has the same PATH as shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; color-identifiers-mode
(use-package color-identifiers-mode
  :config
  (global-color-identifiers-mode 1))

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
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'eldoc-mode))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :hook (flycheck-mode . flycheck-rust-setup))

;; Rust M-. for finding definitions using grep
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "M-.")
    (lambda ()
      (interactive)
      (let ((symbol (thing-at-point 'symbol)))
        (when symbol
          (if (fboundp 'projectile-project-root)
              (rgrep (concat "\\b" symbol "\\b") "*.rs" (projectile-project-root))
            (rgrep (concat "\\b" symbol "\\b") "*.rs" default-directory)))))))

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


;; jquery-doc - removed, no longer needed

;; C/C++ support removed - cc-mode settings removed
(setq-default tab-width 4 indent-tabs-mode t)

;; Autopair (not available in melpa, skip)
;; Using electric-pair-mode as alternative
(electric-pair-mode 1)

;; yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; auto-complete
(use-package auto-complete
  :config
  (require 'auto-complete-config)
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  (ac-set-trigger-key "<tab>"))

;; C/C++ support removed - no longer needed

;; yasnippet compatibility fixes
(when (fboundp 'yas--get-snippet-tables)
  (defalias 'yas/get-snippet-tables 'yas--get-snippet-tables)
  (defalias 'yas/table-hash 'yas--table-hash))

;; Color theme
(add-to-list 'default-frame-alist '(foreground-color . "#E0DFDB"))
(add-to-list 'default-frame-alist '(background-color . "black"))

;; Font settings
(condition-case nil
    (set-face-attribute 'default nil
                        :family "JetBrains Mono" :height 170 :weight 'normal)
  (error (message "JetBrains Mono font not available, using default")))

;; Comment block function removed (was for C/C++)

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
(when (and ispell-program-name (executable-find ispell-program-name))
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
;; Removed M-j and M-m keybindings (were for C/C++ comments)
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
(global-set-key (kbd "s-s") (kbd "C-u 1 M-s"))
(global-set-key (kbd "s-w") (kbd "C-u 1 M-w"))

;; Define minor mode
(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

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
  :ensure t
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 10000))


(message "Emacs configuration loaded successfully!")
