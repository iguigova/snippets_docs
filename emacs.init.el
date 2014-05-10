;VARIABLES

 (setq inhibit-startup-message t)                ; No message at startup
 (setq frame-title-format "%b - emacs")          ; Use buffer name as frame title
 (setq initial-frame-alist '((top . 20) (left . 100) (width . 80) (height . 40)))
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
 (setq tab-width 4)                              ; Length of tab is 4 SPC
 (setq indent-line-function 'insert-tab)
 (setq standard-indent 4)                        ; Set Indent Size

 (setq truncate-partial-width-windows nil)       ; Don't truncate long lines in horizontally split win
 (setq indicate-empty-lines t)                   ; Show empty lines

 (setq scroll-step 1)                            ; Line-by-line scrolling

 (setq grep-command "grep -iIrne \"PATTERN\" .") ;"grep -iIrn --exclude-dir=\"\\.svn\" --include=\"*\\.*\" -e \"PATTERN \" .")
; (setq find-grep-command "find -name \"*.js\" -not -path \"./directory/*\"")
; (setq find-program "c:/Program Files (x86)/Git/bin/find.exe")

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

;(put 'narrow-to-region  'disabled nil)          ; Allow narrow-to-region command
;...

;MODES
 
 (menu-bar-mode nil)                             ; No menubar
 (tool-bar-mode 0)                               ; No toolbar
 (icomplete-mode t)                              ; Completion in mini-buffer
 (global-linum-mode 1)                           ; Show line number on each row
 (line-number-mode 1)                            ; Show line number in mode-line
 (column-number-mode t)                          ; Show column number in mode-line
 (set-scroll-bar-mode 'right)                    ; Scrollbar on the right
 (flyspell-mode 1)                               ; Auto spell-check
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


;MISCELLANEOUS

;http://stackoverflow.com/questions/69934/set-4-space-indent-in-emacs-in-text-mode
 (setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))

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

;http://stackoverflow.com/questions/251908/how-can-i-insert-current-date-and-time-into-a-file-using-emacs
 (defun insert-time ()
  "Insert string for the current time"
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%H:%M:%S %z")))

 (defun insert-date ()
  "Insert current date yyyy-mm-dd."
  (interactive)                 ; permit invocation in minibuffer
  (insert (format-time-string "%Y-%m-%d")))

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

;http://everything2.com/title/Tips+for+using+GNU+Emacs+in+MS+Windows
 (setq w32-use-full-screen-buffer nil)

;http://www.gnu.org/software/emacs/windows/Installing-Emacs.html#Installing-Emacs
 (server-start)

 (defalias 'rof 'recentf-open-files)
 (defalias 'yes-or-no-p 'y-or-n-p)              ; y/n instead of yes/no
 (defalias 'qrr 'query-replace-regexp)          ; Define an alias

 (global-set-key (kbd "<f5>") '(lambda () (interactive)(enlarge-window 5)))
 (global-set-key (kbd "<f6>") 'whitespace-mode)
 (global-set-key (kbd "<f7>") 'toggle-truncate-lines)
 (global-set-key (kbd "<f8>") 'next-buffer)
 (global-set-key (kbd "<f9>") 'other-window)

 (global-set-key "\C-c\C-s" 'create-shell)
 (global-set-key "\C-c\C-d" 'insert-date)
 (global-set-key "\C-c\C-t" 'insert-time)

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
