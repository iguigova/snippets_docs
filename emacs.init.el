;; Performance optimization during startup
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; Package initialization
(require 'package)
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
    godoctor
		dired-subtree
    dired-filter
		lsp-mode
		go-mode
		go-eldoc)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-refresh-contents)  ; Refresh before installing
    (package-install p)))

;; Basic UI settings
(setq-default
 w32-use-full-screen-buffer nil
 inhibit-startup-message t
 frame-title-format "%f (%i) - %b"
 split-width-threshold 9999
 pop-up-windows nil
 visible-bell t
 suggest-key-bindings nil)
(set-face-attribute 'default nil :height 90)  ; 100 = 10pt font

;; Editor behaviorx
(setq-default
 default-major-mode 'text-mode
 confirm-kill-emacs 'yes-or-no-p
 undo-limit 1000000 
 case-fold-search t
 auto-fill-mode 1
 compare-ignore-whitespace t
 create-lockfiles nil
 find-file-reuse-dir-buffer t
 sentence-end-double-space nil
 require-final-newline t)  

;; Line handling
(setq-default
 truncate-partial-width-windows nil
 indicate-empty-lines t
 scroll-step 1
 scroll-conservatively 101   
 scroll-margin 3) 

;; File handling and backup
(setq-default
 auto-save-default nil
 auto-save-timeout 360
 make-backup-files t
 version-control t        
 kept-new-versions 6
 kept-old-versions 2
 delete-old-versions t
 backup-directory-alist '((".*" . "~/.emacs_backups/")))

;; Search and grep
(setq-default grep-command "grep -nHIri -e \"pattern\" .")

;; Indentation
(setq-default
 indent-tabs-mode t
 tab-width 2
 standard-indent 2
 js-indent-level 2
 tab-always-indent nil)

;; Mode settings
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))  ; Grouped UI modes
  (when (fboundp mode) (funcall mode -1)))

(dolist (mode '(icomplete-mode
                global-display-line-numbers-mode
                line-number-mode
                column-number-mode
                delete-selection-mode
                recentf-mode
                desktop-save-mode
                visual-line-mode
                save-place-mode))
  (funcall mode 1))

;; IDO mode settings
(ido-mode 1)
(ido-everywhere 1)
(setq-default
 ido-enable-flex-matching t
 ido-enable-recursive-matches t      ; Enable recursive matching
 ido-use-filename-at-point 'guess    ; Use filename at point if available
 ido-everywhere t
 ido-auto-merge-work-directories-length 2)
;; For even better IDO experience, add ido-vertical-mode
(unless (package-installed-p 'ido-vertical-mode)
  (package-install 'ido-vertical-mode))
(ido-vertical-mode 1)

;; Syntax highlighting
(global-highlight-thing-mode t) ; Automatically highlights the word or symbol under the cursor
(set-face-attribute 'highlight-thing nil :background "#FFFFCC")  
(global-font-lock-mode 1) ;; Highlight whenever you can
(setq-default font-lock-maximum-decoration 2)

;; Parenthesis handling
(show-paren-mode 1)
(setq-default
 blink-matching-paren-distance nil
 show-paren-style 'expression)
(electric-pair-mode 1)  ; Auto-pair parentheses

(global-auto-revert-mode 1) 

;; Dired
(put 'dired-find-alternate-file 'disabled nil) ;; use a in Dired mode to open files or directories while replacing the current buffer

(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map "\C-c\C-f" #'browse-url-of-dired-file)  ; removed ()
     (define-key dired-mode-map (kbd "[") #'dired-subtree-toggle)
     (define-key dired-mode-map (kbd "]") #'dired-subtree-cycle)
     (define-key dired-mode-map (kbd "\\") #'dired-subtree-up)
     (define-key dired-mode-map (kbd "=") #'dired-filter-by-name)))
(add-hook 'dired-mode-hook 'dired-filter-mode)

(add-hook 'dired-mode-hook 'dired-filter-mode)

;; Language-specific settings

;; Go
(require 'lsp-mode)
(setq lsp-log-io t)
(setq lsp-gopls-server-path "/home/ig/go/bin/gopls")
(setq lsp-golangci-lint-server-path "/home/ig/go/bin/golangci-lint-langserver")
(add-hook 'go-mode-hook
          (lambda ()
	    (lsp-deferred)
            (setq tab-width 4
                  standard-indent 4
                  gofmt-tabs t
		  gofmt-command "goimports")))

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)    ; Go to definition
  (define-key lsp-mode-map (kbd "M-,") #'pop-tag-mark)           ; Go back
  (define-key lsp-mode-map (kbd "M-?") #'lsp-find-references))   ; Find references

(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook #'go-eldoc-setup)

;; Clojure
(setq-default
 cider-show-error-buffer nil
 cider-auto-select-error-buffer nil)
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

(global-set-key (kbd "C-c C-S-s") 'create-shell-with-compilation)
(global-set-key "\C-c\C-s" 'create-shell)
(global-set-key "\C-c\C-h" 'sh-shell)
(global-set-key "\C-c\C-j" 'reshell-current-buffer)

(global-set-key "\C-c\C-d" 'insert-date)
(global-set-key "\C-c\C-t" 'insert-time)

;; Theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(misterioso))
 '(package-selected-packages
	 '(lsp-mode dired-filter dired-subtree highlight-thing web-mode godoctor go-mode paredit cider clojure-mode projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
