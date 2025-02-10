;VARIABLES
(setq w32-use-full-screen-buffer nil)           ; http://everything2.com/title/Tips+for+using+GNU+Emacs+in+MS+Windows

(setq inhibit-startup-message t)                ; No message at startup
(setq frame-title-format "%f (%i) - %b")        ; Use buffer name as frame title
(setq initial-frame-alist '((top . 280) (left . 880) (width . 140) (height . 50)))
(setq split-width-threshold 9999)               ; Horizontal splitting https://www.emacswiki.org/emacs/HorizontalSplitting
(setq pop-up-windows nil)                       ; Reuse windows https://emacs.stackexchange.com/questions/20492/how-can-i-get-a-sensible-split-window-policy
(setq default-major-mode 'text-mode)            ; Text-mode is default mode
(setq visible-bell t)                           ; No beep when reporting errors
(setq confirm-kill-emacs 'yes-or-no-p)          ; Confirm quit
(setq undo-limit 1000)                          ; Increase number of undo
(setq auto-save-default nil)                    ; Disable autosave
(setq auto-save-timeout 360)                    ; Autosave every minute
(setq make-backup-files t)                      ; Enable backups
(setq version-control nil)                      ; Enable versioning
(setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))
(setq truncate-partial-width-windows nil)       ; Don't truncate long lines in horizontally split win
(setq indicate-empty-lines t)                   ; Show empty lines
(setq scroll-step 1)                            ; Line-by-line scrolling
(setq grep-command "grep -iIrne \"PATTERN\" .") ;"grep -iIrn --exclude-dir=\"\\.svn\" --include=\"*\\.*\" -e \"PATTERN \" .")
(setq compare-ignore-whitespace t)              ; https://www.gnu.org/software/emacs/manual/html_node/emacs/Comparing-Files.html
(setq create-lockfiles nil)                     ; https://github.com/boot-clj/boot/wiki/Running-on-Windows
(setq find-file-reuse-dir-buffer t)             ; This makes find-file (C-x C-f) reuse the current buffer when opening files in the same directory.
(setq suggest-key-bindings nil)                 ; No hints for M-x
(setq case-fold-search t)                       ; Case-insensitive search (default)
(setq auto-fill-mode 1)                         ; auto-formatting in all major modes

(setq-default indent-tabs-mode t)               ; Use spaces instead of tabs
(setq-default tab-width 2)                      ; Length of tab is 2 spacesx
(setq-default standard-indent 2)                ; Standard indent is 2 spaces
(setq-default js-indent-level 2)                ; JavaScript
(setq tab-always-indent nil)

(add-hook 'go-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq standard-indent 4)
            (setq gofmt-tabs t)))  ; Make sure gofmt uses spaces

(add-hook 'before-save-hook 'gofmt-before-save)

(setq cider-show-error-buffer nil)              ; https://stackoverflow.com/questions/59021212/how-can-i-prevent-an-emacs-error-message-window-from-stealing-focus-and-requirin
(setq cider-auto-select-error-buffer nil)

(add-hook 'clojure-mode-hook #'enable-paredit-mode)

(add-to-list 'auto-mode-alist '("\\.[c|m]js[x]?\\'" . js-mode))

;MODES
 
(menu-bar-mode nil)                             ; No menubar
(tool-bar-mode 0)                               ; No toolbar
(icomplete-mode t)                              ; Completion in mini-buffer
(global-display-line-numbers-mode 1)            ; Show line number on each row
(line-number-mode 1)                            ; Show line number in mode-line
(column-number-mode t)                          ; Show column number in mode-line
(delete-selection-mode t)                       ; Delete selection enabled
(recentf-mode 1)                                ; Recent files menu enabled
(desktop-save-mode t)                           ; Save session before quitting
(visual-line-mode 1)                            ; Edit on visual (not logical) lines + line wrapping

(ido-mode 1)                                    ; Remaps 'find-file' + 'switch-to-buffer' keybindings
(setq ido-enable-flex-matching t)               ; Enable flexible matching
(setq ido-everywhere t)                         ; Use ido everywhere

(global-font-lock-mode 1)                       ; Color enabled
(setq font-lock-maximum-decoration t)

(show-paren-mode 1)                             ; Highlight parenthesis pairs
(setq blink-matching-paren-distance nil)        ; Blinking parenthesis
(setq show-paren-style 'expression)             ; Highlight text between parens

(global-auto-revert-mode 1)                     ; https://emacs.stackexchange.com/questions/169/how-do-i-reload-a-file-in-a-buffer

(put 'dired-find-alternate-file 'disabled nil)  ; https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer

(add-hook 'dired-mode-hook
		  (lambda ()
	      (local-set-key "\C-c\C-f" 'browse-url-of-dired-file))) ;; in Dired mode, opens the file under the cursor in your default web browser.

(defun create-shell ()
    "Creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
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
    ;(shell-file-name explicit-shell-file-name)
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
  ;(shell (current-buffer)))  
  (let ((explicit-shell-file-name "/bin/bash")
    ;(shell-file-name explicit-shell-file-name)
    (explicit-sh.exe-args '("--login" "-i")))
    (shell (current-buffer))))

(defun insert-time ()
  "Insert string for the current time"
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%H:%M:%S %z")))

(defun insert-date ()
  "Insert current date yyyy-mm-dd."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))


; https://stackoverflow.com/questions/1085170/how-to-achieve-code-folding-effects-in-emacs
(defun set-selective-display-dlw (&optional level)
"Fold text indented same of more than the cursor.
If level is set, set the indent level to LEVEL.
If 'selective-display' is already set to LEVEL, clicking
F5 again will unset 'selective-display' by setting it to 0."
  (interactive "P")
  (if (eq selective-display (1+ (current-column)))
      (set-selective-display 0)
    (set-selective-display (or level (1+ (current-column))))))

(defun copy-paste-buffer ()
  "Kill the entire buffer content, keep it in the kill ring, and reinsert it,
preserving the original cursor position."
  (interactive)
  (let ((orig-point (point))         ; Save the current cursor position
        (content (buffer-string)))    ; Save the current content of the buffer
    (kill-region (point-min) (point-max))    ; Kill the entire buffer content
    (insert content)                         ; Reinsert the content
    (goto-char orig-point)))                 ; Restore cursor position

(defun swap-buffer-windows ()
  "Swap the buffers between the two current windows."
  (interactive)
  (let ((buffer1 (window-buffer (selected-window)))
        (window2 (next-window))
        (buffer2 (window-buffer (next-window))))
    (set-window-buffer (selected-window) buffer2)
    (set-window-buffer window2 buffer1)))


(defalias 'rof 'recentf-open-files)
(defalias 'yes-or-no-p 'y-or-n-p)              ; y/n instead of yes/no
(defalias 'qrr 'query-replace-regexp)          ; Define an alias

(global-set-key (kbd "<f3>") 'cider-selector) ; https://docs.cider.mx/cider/usage/misc_features.html
(global-set-key (kbd "<f4>") 'kill-this-buffer) 
(global-set-key (kbd "<f5>") #'(lambda () (interactive)(enlarge-window 5)))
(global-set-key (kbd "<f6>") 'whitespace-mode)
(global-set-key (kbd "<f7>") 'toggle-truncate-lines)
(global-set-key (kbd "<f8>") 'set-selective-display-dlw)
;(global-set-key (kbd "<f8>") 'other-window)
;(global-set-key (kbd "<f10>") 'previous-buffer)
;(global-set-key (kbd "<f11>") 'next-buffer)
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

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(projectile
                      clojure-mode
                      cider
                      web-mode))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))
	
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(misterioso))
 '(package-selected-packages
   '(web-mode godoctor go-mode paredit cider clojure-mode projectile)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;MISCELLANEOUS

;http://xahlee.org/emacs/emacs_installing_packages.html
;http://www.haskell.org/haskellwiki/Haskell_mode_for_Emacs
;http://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00663.html
;(add-to-list 'load-path "c:/Program Files/emacs-23.3/lisp/othermodes/haskell-mode-2.8.0")
;(load "haskell-site-file")
;(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;http://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html
;c:\Program Files (x86)\erl5.9.2\lib\tools-2.6.8\emacs
; (setq load-path (cons "c:/Program Files (x86)/erl5.9.2/lib/tools-2.6.8/emacs" load-path))
; (setq erlang-root-dir "c:/Program Files (x86)/erl5.9.2")
; (setq exec-path (cons "c:/Program Files (x86)/erl5.9.2/bin" exec-path))
; (require 'erlang-start)

;http://xahlee.org/emacs/emacs_html.html : Colored color values in css and html
 ;; (defvar hexcolour-keywords
 ;;   '(("#[abcdef[:digit:]]\\{6\\}"
 ;;      (0 (put-text-property
 ;;          (match-beginning 0)
 ;;          (match-end 0)
 ;;          'face (list :background 
 ;;                      (match-string-no-properties 0)))))))
 ;; (defun hexcolour-add-to-font-lock ()
 ;;   (font-lock-add-keywords nil hexcolour-keywords))
 ;; (add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)
;; (add-hook 'html-mode-hook 'hexcolour-add-to-font-lock)

;http://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs 
;;  (defun bf-pretty-print-xml-region (begin end)
;;   "Pretty format XML markup in region. You need to have nxml-mode
;; http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
;; this.  The function inserts linebreaks to separate tags that have
;; nothing but whitespace between them.  It then indents the markup
;; by using nxml's indentation rules."
;;     (interactive "r")
;;     (save-excursion
;;       (nxml-mode)
;;       (goto-char begin)
;;       (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
;;         (backward-char) (insert "\n"))
;;       (indent-region begin end))
;;     (message "Ah, much better!"))
