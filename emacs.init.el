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
 (setq undo-limit 100000)                        ; Increase number of undo

 (setq auto-save-default nil)                    ; Disable autosave
 (setq auto-save-timeout 360)                    ; Autosave every minute

 (setq make-backup-files t)                      ; Enable backups
 (setq version-control t)                        ; Enable versioning
 (setq backup-directory-alist (quote ((".*" . "~/.emacs_backups/"))))

 (setq-default indent-tabs-mode nil)             ; Use spaces instead of tabs
 (setq tab-width 2)                              ; Length of tab is N SPC
 (setq indent-line-function 'insert-tab)
 (setq standard-indent 4)                        ; Set Indent Size
 (setq tab-stop-list '(2 4 6 8 10 12 14 16 18 20 22 24 26 28 30 32 34 36 38 40 42 44 46 48 50 52 54 56 58 60 62 64 66 68 70 72 74 76 78 80)) ;http://stackoverflow.com/questions/69934/set-4-space-indent-in-emacs-in-text-mode

 (setq truncate-partial-width-windows nil)       ; Don't truncate long lines in horizontally split win
 (setq indicate-empty-lines t)                   ; Show empty lines

 (setq scroll-step 1)                            ; Line-by-line scrolling

 (setq grep-command "grep -iIrne \"PATTERN\" .") ;"grep -iIrn --exclude-dir=\"\\.svn\" --include=\"*\\.*\" -e \"PATTERN \" .")
; (setq find-grep-command "find -name \"*.js\" -not -path \"./directory/*\"")
; (setq find-program "c:/Program Files (x86)/Git/bin/find.exe")

 (setq compare-ignore-whitespace t)              ; https://www.gnu.org/software/emacs/manual/html_node/emacs/Comparing-Files.html
 (setq create-lockfiles nil)                     ; https://github.com/boot-clj/boot/wiki/Running-on-Windows

 (setq cider-show-error-buffer nil)              ;https://stackoverflow.com/questions/59021212/how-can-i-prevent-an-emacs-error-message-window-from-stealing-focus-and-requirin
 (setq cider-auto-select-error-buffer nil)

;(setq sentence-end-double-space nil)            ; Sentences end with one space   
;(setq mouse-yank-at-point t)                    ; Paste at cursor position
;(setq scroll-preserve-screen-position t)        ; Scroll without moving cursor
;(setq enable-recursive-minibuffers t)           ; Stack minibuffers
;(setq suggest-key-bindings nil)                 ; No hints for M-x
;(setq ispell-dictionary "english")              ; Set ispell dictionary
;(setq calendar-week-start-day 1)                ; Week starts monday
;(setq european-calendar-style 't)               ; European style calendar
;(setq ps-paper-type 'a4)                        ; Specify printing format
;(setq case-fold-search t)                       ; Search is case sensitive
;(setq exec-path (append exec-path '("/bin")))   ; Change binary path
;...

;FUNCTIONS
 (set-background-color "#222222")                ; Background color
 (set-foreground-color "white")                  ; Foreground color
 (set-cursor-color "white")                      ; Cursor color 
 (set-face-attribute 'default nil :height 80)    ; increase font size for better readability

;(put 'narrow-to-region  'disabled nil)          ; Allow narrow-to-region command
;...

;MODES
 
 (menu-bar-mode nil)                             ; No menubar
 (tool-bar-mode 0)                               ; No toolbar
 (icomplete-mode t)                              ; Completion in mini-buffer
 (global-display-line-numbers-mode 1)            ; Show line number on each row
 (line-number-mode 1)                            ; Show line number in mode-line
 (column-number-mode t)                          ; Show column number in mode-line
 (set-scroll-bar-mode 'right)                    ; Scrollbar on the right
;(flyspell-mode 1)                               ; Auto spell-check
 (delete-selection-mode t)                       ; Delete selection enabled
 (recentf-mode 1)                                ; Recent files menu enabled
 (ido-mode 1)                                    ; Remaps 'find-file' + 'switch-to-buffer' keybindings
;(desktop-save-mode t)                           ; Save session before quitting
;(global-hl-line-mode t)                         ; Highlight cursor line
;(blink-cursor-mode 0)                           ; No blinking cursor
;(mouse-wheel-mode t)                            ; Mouse-wheel enabled

 (global-font-lock-mode 1)                       ; Color enabled
 (setq font-lock-maximum-decoration t)

 (show-paren-mode 1)                             ; Highlight parenthesis pairs
 (setq blink-matching-paren-distance nil)        ; Blinking parenthesis
 (setq show-paren-style 'expression)             ; Highlight text between parens

 (visual-line-mode 1)                            ; Edit on visual (not logical) lines + line wrapping
;(setq line-move-visual nil)                     ; Move point by logical (not visual) lines
;(setq track-eol nil)                            ; Cursor don't track end-of-line
;(setq next-line-add-newlines t)                 ; Add newline when at buffer end
;(setq require-final-newline t)                  ; Always newline at end of file
;(setq auto-fill-mode 1)                         ; auto-formatting in all major modes
;(add-hook 'text-mode-hook 'turn-on-auto-fill)   ; auto-formatting in text-mode

 (put 'dired-find-alternate-file 'disabled nil)  ; https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer

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
 (defvar hexcolour-keywords
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background 
                      (match-string-no-properties 0)))))))
 (defun hexcolour-add-to-font-lock ()
   (font-lock-add-keywords nil hexcolour-keywords))
 (add-hook 'css-mode-hook 'hexcolour-add-to-font-lock)
 (add-hook 'html-mode-hook 'hexcolour-add-to-font-lock)

;http://stackoverflow.com/questions/6532998/how-to-run-multiple-shells-on-emacs
 (defun create-shell ()
    "Creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))

       (shell (concat "*" shell-name "*"))))

;http://stackoverflow.com/questions/235254/how-can-i-run-cygwin-bash-shell-from-within-emacs
;http://stackoverflow.com/questions/16676750/windows-emacs-git-bash-and-shell-command
 (defun sh-shell ()
  "Run git sh in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "c:/Program Files/Git/bin/sh.exe")
    ;(shell-file-name explicit-shell-file-name)
    (explicit-sh.exe-args '("--login" "-i")))
    (call-interactively 'create-shell)))

;http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
 (defun insert-time ()
  "Insert string for the current time"
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%H:%M:%S %z")))

 (defun insert-date ()
  "Insert current date yyyy-mm-dd."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))

 (defun reshell-current-buffer ()
  "Kills the process in the current buffer and starts a new shell in it"
  (interactive)
  (delete-process (get-buffer-process(current-buffer)))
  ;(shell (current-buffer)))  
  (let ((explicit-shell-file-name "c:/Program Files/Git/bin/sh.exe")
    ;(shell-file-name explicit-shell-file-name)
    (explicit-sh.exe-args '("--login" "-i")))
    (shell (current-buffer))))

;http://stackoverflow.com/questions/10627289/emacs-internal-process-killing-any-command
 (defun joaot/delete-process-at-point ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (cond ((and process
                (processp process))
           (delete-process process)
           (revert-buffer))
          (t
           (error "no process at point!")))))

 ;http://stackoverflow.com/questions/12492/pretty-printing-xml-files-on-emacs 
 (defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
    (interactive "r")
    (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Ah, much better!"))

;http://www.gnu.org/software/emacs/windows/Installing-Emacs.html#Installing-Emacs
; (server-start)

 (defalias 'rof 'recentf-open-files)
 (defalias 'yes-or-no-p 'y-or-n-p)              ; y/n instead of yes/no
 (defalias 'qrr 'query-replace-regexp)          ; Define an alias

 (global-set-key (kbd "<f4>") 'kill-this-buffer) 
 (global-set-key (kbd "<f5>") #'(lambda () (interactive)(enlarge-window 5)))
 (global-set-key (kbd "<f6>") 'whitespace-mode)
 (global-set-key (kbd "<f7>") 'toggle-truncate-lines)
 (global-set-key (kbd "<f8>") 'next-buffer)
 (global-set-key (kbd "<f9>") 'other-window)
; (global-set-key (kbd "<f10>") 'compare-windows)
 (global-set-key (kbd "<f10>") 'cider-selector) ; https://docs.cider.mx/cider/usage/misc_features.html
 (global-set-key (kbd "<f11>") 'ido-switch-buffer)
 
 (global-set-key "\C-c\C-s" 'create-shell)
 (global-set-key "\C-c\C-h" 'sh-shell)
 (global-set-key "\C-c\C-j" 'reshell-current-buffer)
 (global-set-key "\C-c\C-d" 'insert-date)
 (global-set-key "\C-c\C-t" 'insert-time)

 (define-key process-menu-mode-map (kbd "C-k") 'joaot/delete-process-at-point)

;References
; http://homepages.inf.ed.ac.uk/s0243221/emacs/
; http://www.gnu.org/software/emacs/windows/big.html#Display-Settings
; http://www.gnu.org/software/emacs/windows/old/faq4.html
; http://xahlee.org/emacs/emacs_make_modern.html
; http://xahlee.org/emacs/emacs_esoteric.html
; http://xahlee.org/emacs/emacs_adv_tips.html
; http://www.emacswiki.org/emacs/TruncateLines
; http://www.emacswiki.org/emacs/McMahanEmacsConfiguration
; http://www.masteringemacs.org/articles/2011/02/08/mastering-key-bindings-emacs/
(put 'upcase-region 'disabled nil)

; http://clojure-doc.org/articles/tutorials/emacs.html
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(defvar my-packages '(projectile
                      clojure-mode
                      cider))

(dolist (p my-packages)
  (unless (package-installed-p p)
    (package-install p)))
	
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (paredit cider clojure-mode projectile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'clojure-mode-hook #'enable-paredit-mode)
