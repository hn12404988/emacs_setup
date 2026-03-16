;;; init.lite.el --- Lightweight Emacs config (keybindings only) -*- lexical-binding: t -*-

;; Minimal config for resource-constrained devices (e.g. Termux on e-ink).
;; Only custom keybindings and essential settings. No packages required.

;; Basic settings
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-startup-message t)
(setq-default tab-width 4 indent-tabs-mode t)
(global-auto-revert-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(ido-mode 1)
(setq ido-default-buffer-method 'selected-window)

;; Line numbers
(if (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode t)
  (when (fboundp 'global-linum-mode)
    (global-linum-mode t)))

;; Clock
(display-time-mode 1)
(setq display-time-day-and-date 1)
(setq display-time-24hr-format 1)

;; Menu bar off
(menu-bar-mode -1)

;; ============================================================================
;; E-ink monochrome theme - all white on black, max contrast
;; ============================================================================
(defun eink-apply-faces ()
  "Apply high-contrast monochrome faces for e-ink displays."
  ;; Use #xxxxxx hex colors to bypass terminal ANSI color mapping
  (let ((fg "#ffffff") (bg "#000000") (inv-fg "#000000") (inv-bg "#ffffff"))
    (set-face-attribute 'default nil :foreground fg :background bg)
    (set-face-attribute 'cursor nil :background fg)
    (set-face-attribute 'font-lock-keyword-face nil :foreground fg :weight 'normal :slant 'normal)
    (set-face-attribute 'font-lock-function-name-face nil :foreground fg :weight 'normal)
    (set-face-attribute 'font-lock-variable-name-face nil :foreground fg)
    (set-face-attribute 'font-lock-string-face nil :foreground fg)
    (set-face-attribute 'font-lock-comment-face nil :foreground fg :slant 'normal)
    (set-face-attribute 'font-lock-comment-delimiter-face nil :foreground fg)
    (set-face-attribute 'font-lock-type-face nil :foreground fg :weight 'normal)
    (set-face-attribute 'font-lock-constant-face nil :foreground fg)
    (set-face-attribute 'font-lock-builtin-face nil :foreground fg :weight 'normal)
    (set-face-attribute 'font-lock-preprocessor-face nil :foreground fg)
    (set-face-attribute 'font-lock-warning-face nil :foreground fg :weight 'normal)
    (set-face-attribute 'font-lock-doc-face nil :foreground fg :slant 'normal)
    (set-face-attribute 'region nil :background inv-bg :foreground inv-fg)
    (set-face-attribute 'highlight nil :background inv-bg :foreground inv-fg)
    (set-face-attribute 'isearch nil :background inv-bg :foreground inv-fg)
    (set-face-attribute 'lazy-highlight nil :background inv-bg :foreground inv-fg)
    (set-face-attribute 'show-paren-match nil :background inv-bg :foreground inv-fg)
    (set-face-attribute 'minibuffer-prompt nil :foreground fg)
    (set-face-attribute 'mode-line nil :background inv-bg :foreground inv-fg)
    (set-face-attribute 'mode-line-inactive nil :background "#333333" :foreground fg)
    (set-face-attribute 'line-number nil :foreground fg)
    (set-face-attribute 'line-number-current-line nil :foreground fg)
    (set-face-attribute 'shadow nil :foreground fg)
    (set-face-attribute 'link nil :foreground fg :underline t)
    (set-face-attribute 'link-visited nil :foreground fg :underline t)
    (set-face-attribute 'fringe nil :foreground fg :background bg)
    (set-face-attribute 'vertical-border nil :foreground fg)
    ;; Dired directories
    (with-eval-after-load 'dired
      (set-face-attribute 'dired-directory nil :foreground fg))
    ;; Ido
    (when (facep 'ido-subdir)
      (set-face-attribute 'ido-subdir nil :foreground fg))
    (when (facep 'ido-first-match)
      (set-face-attribute 'ido-first-match nil :foreground fg))))

(add-hook 'after-init-hook #'eink-apply-faces)

;; Terminal: decode Shift+Tab (ESC [ Z) as <backtab>
(add-hook 'tty-setup-hook
          (lambda ()
            (define-key input-decode-map "\e[Z" [backtab])))

;; ============================================================================
;; Helper functions
;; ============================================================================

(defun smart-scroll-up ()
  "Scroll up one page. If already at top, move cursor to beginning of buffer."
  (interactive)
  (condition-case nil
      (scroll-down-command)
    (beginning-of-buffer
     (goto-char (point-min)))))

(defun smart-backward-kill ()
  "Smart backward kill: word -> line start -> backspace."
  (interactive)
  (cond
   ((zerop (current-column))
    (if (bobp)
        (message "Beginning of buffer")
      (delete-backward-char 1)))
   ((save-excursion
      (skip-chars-backward " \t")
      (bolp))
    (kill-line 0))
   (t
    (backward-kill-word 1))))

(defun kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

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

;; ============================================================================
;; Custom keybindings (minor mode so they override major modes)
;; ============================================================================

(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; Movement keys
(define-key my-keys-minor-mode-map (kbd "C-d") 'forward-char)
(define-key my-keys-minor-mode-map (kbd "C-a") 'backward-char)
(define-key my-keys-minor-mode-map (kbd "C-w") 'previous-line)
(define-key my-keys-minor-mode-map (kbd "C-s") 'next-line)
(define-key my-keys-minor-mode-map (kbd "C-q") 'back-to-indentation)
(define-key my-keys-minor-mode-map (kbd "C-e") 'move-end-of-line)
(define-key my-keys-minor-mode-map (kbd "M-d") 'forward-word)
(define-key my-keys-minor-mode-map (kbd "M-a") 'backward-word)
(define-key my-keys-minor-mode-map (kbd "M-9") 'beginning-of-buffer)
(define-key my-keys-minor-mode-map (kbd "M-0") 'end-of-buffer)

;; Scroll
(define-key my-keys-minor-mode-map (kbd "M-s") 'scroll-up-command)
(define-key my-keys-minor-mode-map (kbd "M-w") 'smart-scroll-up)

;; Editing
(define-key my-keys-minor-mode-map (kbd "M-q") 'delete-backward-char)
(define-key my-keys-minor-mode-map (kbd "M-e") 'delete-forward-char)
(define-key my-keys-minor-mode-map (kbd "C-DEL") 'smart-backward-kill)
(define-key my-keys-minor-mode-map (kbd "<C-backspace>") 'smart-backward-kill)
(define-key my-keys-minor-mode-map [C-backspace] 'smart-backward-kill)
(global-set-key (kbd "<C-backspace>") 'smart-backward-kill)
(define-key my-keys-minor-mode-map (kbd "C-h") 'smart-backward-kill)
(define-key my-keys-minor-mode-map (kbd "M-DEL") 'backward-kill-word)
(unless (display-graphic-p)
  (define-key my-keys-minor-mode-map (kbd "C-h") 'backward-kill-word))
(define-key my-keys-minor-mode-map (kbd "C-k") 'kill-whole-line)
(define-key my-keys-minor-mode-map (kbd "C-j") 'newline-and-indent)

;; Copy/paste (simple kill-ring, no macOS clipboard)
(define-key my-keys-minor-mode-map (kbd "C-c") 'kill-ring-save)
(define-key my-keys-minor-mode-map (kbd "C-v") 'yank)

;; Search
(define-key my-keys-minor-mode-map (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-g") 'isearch-exit)
(define-key isearch-mode-map (kbd "RET") 'isearch-repeat-forward)
(setq isearch-wrap-pause 'no)

;; Buffers & windows
(define-key my-keys-minor-mode-map (kbd "C-M-s") 'next-buffer)
(define-key my-keys-minor-mode-map (kbd "C-M-w") 'previous-buffer)
(define-key my-keys-minor-mode-map (kbd "C-M-d") 'other-window)
(define-key my-keys-minor-mode-map (kbd "C-x k") 'kill-current-buffer)
(define-key my-keys-minor-mode-map (kbd "C-x SPC") 'set-mark-command)
(define-key my-keys-minor-mode-map (kbd "M-_") 'my/shrink-window-smart)
(define-key my-keys-minor-mode-map (kbd "M-+") 'my/enlarge-window-smart)

;; Comment
(global-set-key (kbd "C-x m") 'comment-dwim)

;; Save all
(global-set-key (kbd "C-x C-s") (kbd "C-u C-x s"))

;; ============================================================================
;; Activate minor mode
;; ============================================================================

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter ""
  :keymap my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;; Ensure highest priority
(defvar my-keys-minor-mode-map-alist `((my-keys-minor-mode . ,my-keys-minor-mode-map)))
(add-to-list 'emulation-mode-map-alists 'my-keys-minor-mode-map-alist)

;; Disable in minibuffer
(defun my-minibuffer-setup-hook ()
  (my-keys-minor-mode 0))
(add-hook 'minibuffer-setup-hook 'my-minibuffer-setup-hook)
