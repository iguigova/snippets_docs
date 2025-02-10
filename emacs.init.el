;; Performance optimization during startup
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; Package initialization
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Ensure we have package.el
(unless package-archive-contents
  (package-refresh-contents))

;; Define and install required packages
(defvar my-packages
  '(projectile
    clojure-mode
    cider
    web-mode
    paredit
    go-mode
    godoctor)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))

;; Basic UI settings
(setq-default w32-use-full-screen-buffer nil)
(setq-default inhibit-startup-message t)
(setq-default frame-title-format "%f (%i) - %b")
(setq-default initial-frame-alist '((top . 280) (left . 880) (width . 140) (height . 50)))
(setq-default split-width-threshold 9999)
(setq-default pop-up-windows nil)
(setq-default visible-bell t)
(setq-default suggest-key-bindings nil)

;; Editor behavior
(setq-default default-major-mode 'text-mode)
(setq-default confirm-kill-emacs 'yes-or-no-p)
(setq-default undo-limit 1000)
(setq-default case-fold-search t)
(setq-default auto-fill-mode 1)
(setq-default compare-ignore-whitespace t)
(setq-default create-lockfiles nil)
(setq-default find-file-reuse-dir-buffer t)

;; Line handling
(setq-default truncate-partial-width-windows nil)
(setq-default indicate-empty-lines t)
(setq-default scroll-step 1)

;; File handling and backup
(setq-default auto-save-default nil)
(setq-default auto-save-timeout 360)
(setq-default make-backup-files t)
(setq-default version-control nil)
(setq-default backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

;; Search and grep
(setq-default grep-command "grep -iIrne \"PATTERN\" .")

;; Indentation
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
(setq-default standard-indent 2)
(setq-default js-indent-level 2)
(setq-default tab-always-indent nil)

;; Mode settings - these use setq because they're global
(menu-bar-mode nil)
(tool-bar-mode 0)
(icomplete-mode t)
(global-display-line-numbers-mode 1)
(line-number-mode 1)
(column-number-mode t)
(delete-selection-mode t)
(recentf-mode 1)
(desktop-save-mode t)
(visual-line-mode 1)

;; IDO mode settings
(ido-mode 1)
(setq-default ido-enable-flex-matching t)
(setq-default ido-everywhere t)

;; Font and color settings
(global-font-lock-mode 1)
(setq-default font-lock-maximum-decoration t)

;; Parenthesis handling
(show-paren-mode 1)
(setq-default blink-matching-paren-distance nil)
(setq-default show-paren-style 'expression)

;; Auto-revert and dired settings
(global-auto-revert-mode 1)
(put 'dired-find-alternate-file 'disabled nil)

;; Language-specific settings
;; Go
(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq standard-indent 4)
            (setq gofmt-tabs t)))
(add-hook 'before-save-hook 'gofmt-before-save)

;; Clojure
(setq-default cider-show-error-buffer nil)
(setq-default cider-auto-select-error-buffer nil)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.[c|m]js[x]?\\'" . js-mode))

;; Custom functions
(defun create-shell ()
  "Creates a shell with a given name"
  (interactive)
  (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

(defun create-shell-with-compilation ()
  "Creates a named shell with compilation mode enabled"
  (interactive)
  (let* ((shell-name (read-string "shell name: " nil))
         (shell-buffer (shell (concat "*" shell-name "*"))))
    (with-current-buffer shell-buffer
      (compilation-shell-minor-mode 1))))

(defun sh-shell ()
  "Run git sh in shell mode. Use bash shell in windows."
  (interactive)
  (let ((explicit-shell-file-name "/bin/bash")
        (explicit-sh.exe-args '("--login" "-i")))
    (call-interactively 'create-shell)))

(defun sh-shell-with-compilation ()
  "Run git bash shell with compilation mode enabled"
  (interactive)
  (let ((explicit-shell-file-name "/bin/bash")
        (explicit-sh.exe-args '("--login" "-i")))
    (let ((shell-buffer (create-shell-with-compilation)))
      (with-current-buffer shell-buffer
        (compilation-shell-minor-mode 1)))))

(defun reshell-current-buffer ()
  "Kills the process in the current buffer and starts a new shell in it"
  (interactive)
  (delete-process (get-buffer-process(current-buffer)))
  (let ((explicit-shell-file-name "/bin/bash")
        (explicit-sh.exe-args '("--login" "-i")))
    (shell (current-buffer))))

(defun insert-time ()
  "Insert string for the current time"
  (interactive)
  (insert (format-time-string "%H:%M:%S %z")))

(defun insert-date ()
  "Insert current date yyyy-mm-dd."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun set-selective-display-dlw (&optional level)
  "Fold text indented same of more than the cursor."
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))

(defun copy-paste-buffer ()
  "Kill the entire buffer content and reinsert it, preserving cursor position."
  (interactive)
  (let ((orig-point (point))
        (content (buffer-string)))
    (kill-region (point-min) (point-max))
    (insert content)
    (goto-char orig-point)))

(defun swap-buffer-windows ()
  "Swap the buffers between the two current windows."
  (interactive)
  (let ((buffer1 (window-buffer (selected-window)))
        (window2 (next-window))
        (buffer2 (window-buffer (next-window))))
    (set-window-buffer (selected-window) buffer2)
    (set-window-buffer window2 buffer1)))

;; Aliases
(defalias 'rof 'recentf-open-files)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'qrr 'query-replace-regexp)

;; Dired mode key binding
(add-hook 'dired-mode-hook
          (lambda ()
            (local-set-key "\C-c\C-f" 'browse-url-of-dired-file)))

;; Key bindings
(global-set-key (kbd "<f3>") 'cider-selector)
(global-set-key (kbd "<f4>") 'kill-this-buffer)
(global-set-key (kbd "<f5>") #'(lambda () (interactive)(enlarge-window 5)))
(global-set-key (kbd "<f6>") 'whitespace-mode)
(global-set-key (kbd "<f7>") 'toggle-truncate-lines)
(global-set-key (kbd "<f8>") 'set-selective-display-dlw)
(global-set-key (kbd "<f12>") 'compare-windows)

(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "<backtab>") 'swap-buffer-windows)
(global-set-key (kbd "C-p") 'previous-buffer)
(global-set-key (kbd "C-n") 'next-buffer)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-z") 'copy-paste-buffer)

(global-set-key "\C-c\C-s" 'create-shell)
(global-set-key (kbd "C-c C-S-s") 'create-shell-with-compilation)
(global-set-key "\C-c\C-h" 'sh-shell)
(global-set-key "\C-c\C-j" 'reshell-current-buffer)

(global-set-key "\C-c\C-d" 'insert-date)
(global-set-key "\C-c\C-t" 'insert-time)

;; Theme
(custom-set-variables
 '(custom-enabled-themes '(misterioso))
 '(package-selected-packages
   '(web-mode godoctor go-mode paredit cider clojure-mode projectile)))
