;;Jim Wang
;;
;;for Win32 & OSX, gnu emacs, no cygwin (on Win32).
;;
;;some of the settings as recommended by
;;http://opal.cabochon.com/~stevey/blog-rants/effective-emacs.html
;;

;;really sub-par hack to support carbonized emacs.
;;(setq exec-path nil)
(setenv "PATH" (concat "/opt/local/bin:/opt/local/mysql/bin:/opt/local/sbin:" (getenv "PATH")))
(add-to-list 'exec-path (getenv "PATH"))
(add-to-list 'exec-path "/bin")
(add-to-list 'exec-path "/usr/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/opt/local/bin")
(add-to-list 'exec-path "/usr/local/mysql/bin")
(add-to-list 'exec-path "/opt/local/godi/bin")
(add-to-list 'exec-path "/opt/local/godi/sbin")
(add-to-list 'exec-path "/opt/local/sbin")


(autoload 'vm "~/vm" "Start VM on your primary inbox." t)
(autoload 'vm-visit-folder "~/vm" "Start VM on an arbitrary folder." t)
(autoload 'vm-mail "~/vm" "Send a mail message using VM." t)
(autoload 'vm-submit-bug-report "~/vm" "Send a bug report about VM." t)


;;Org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/googletasks.org"))



;; Date
;; Insert date string
(defun insert-date-string ()
"Insert a nicely formated date string."
(interactive)
(insert (format-time-string "%a %b %d %Y")))

(global-set-key (kbd "C-c d") 'insert-date-string)

;;Help is for the weak
(global-set-key "\C-h" 'backward-kill-word)
(global-set-key [(control \.)] 'other-window)


;;Effective Emacs
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(defalias 'qrr 'query-replace-regexp)
(global-set-key [(ctrl tab)] 'bury-buffer)

;;Scroll bars are for the weak
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
  (let* ((my-lisp-dir "~/elisp/")
         (default-directory my-lisp-dir))
    (setq load-path (cons my-lisp-dir load-path))
    (normal-top-level-add-subdirs-to-load-path)))


;;I'm a lazy typist
;;(abbrev-mode 1)


;;LaTeX
(setq tex-dvi-view-command "\"C:\\Program Files (x86)\\MiKTeX 2.7\\miktex\\bin\\yap.exe\"")
;;--
;;misc
;;--
(require 'ido)
(ido-mode t)

(require 'align)

(require 'browse-kill-ring)

(require 'psvn)

;;-----------------------------------------------------------
;;erlang
(require 'erlang)
(autoload 'erlang-mode "erlang-mode" "Erlang editing mode." t)
(setq auto-mode-alist (cons '("\\.erl$" . erlang-mode) auto-mode-alist))
;;-----------------------------------------------------------


;;-----------------------------------------------------------
;;nice fonts for OSX
;;-----------------------------------------------------------
;;(set-face-attribute 'default nil :family "monaco" :height 130)
;;(set-face-attribute 'default nil :family "andale mono" :height 140)
;;(set-face-attribute 'default nil :family "anonymous" :height 130)
;;(set-face-attribute 'default nil :family "consolas" :height 140)

;;-----------------------------------------------------------
;;ruby
;;-----------------------------------------------------------

(require 'ruby-mode)
(require 'rubydb)
(require 'rails)
(require 'yaml-mode)
(require 'rcov)
(require 'autotest)



(setq ri-ruby-script "~/elisp/ri-emacs")
(autoload 'ri "~/elisp/ri-ruby.el" nil t)

(autoload 'ruby-mode "ruby-mode" "Ruby editing mode." t)

(modify-coding-system-alist 'file "\\.rb$" 'utf-8)
(modify-coding-system-alist 'file "\\.rhtml$" 'utf-8)

(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(?:^\\|/\\)Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(add-hook 'ruby-mode-hook

          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                        (save-excursion
                          (untabify (point-min) (point-max))
                          (delete-trailing-whitespace)
                          )))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (imenu-add-to-menubar "IMENU")
            (require 'ruby-electric)
            (ruby-electric-mode t)
            ))

(require 'rcodetools)
(require 'icicles-rcodetools)

(global-set-key [f6] 'xmp)

;; Alignment
(add-to-list 'align-rules-list
             '(ruby-comma-delimiter
               (regexp . ",\\(\\s-*\\)[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))
(add-to-list 'align-rules-list
             '(ruby-hash-literal
               (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
               (repeat . t)
               (modes  . '(ruby-mode))))

;; RI everywhere!
(define-key help-map "r" 'ri)

;;-----------------------------------------------------------

(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))

(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))

;;--- yaml ----
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(define-key yaml-mode-map "\C-m" 'newline-and-indent)

(add-hook 'yaml-mode-hook
 '(lambda ()
    (define-key yaml-mode-map "\C-m" 'newline-and-indent)))



;;-----------------------------------------------------------
;;java script
;;-----------------------------------------------------------

(require 'javascript-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

(if (condition-case nil
        (symbol-function 'c-block-in-arglist-dwim)
      (error nil))
    (defun javascript-fixed-c-lineup-arglist (langelem)
      (save-excursion
        (if (c-block-in-arglist-dwim (c-langelem-2nd-pos c-syntactic-element))
            0
          (c-lineup-arglist langelem))))
  (defun javascript-fixed-c-lineup-arglist (langelem)
    (c-lineup-arglist langelem)))


;; Ugghh....  So much trouble w/ what cc-mode allows to control or not.  Ended
;; up pulling everything in to try to get indentation correct, only to discover
;; that cc-mode does not currently allow the style I want.  Good thing I don't
;; have to do much Javascript programming ATM.

;; Setup a new "C" style to support this Javascript maddness
(c-add-style
 "javascript"
 ;; Expand all tabs with appropriate number of spaces
 '((indent-tabs-mode         . nil)
   (tab-width                . 4)
   (c-basic-offset           . 4)
   (fill-column              . 79)
   (c-ignore-auto-fill       . '(string cpp))
   ;; Most of this isn't necessary, but I don't feel like cleaning it
   (c-hanging-braces-alist   . ((defun-open after)
                                (defun-close  before after)
                                (defun-block-intro after)
                                (brace-list-intro after)
                                (brace-entry-open after)
                                (statement-cont after)
                                (statement-block-open after)
                                (statement-block-intro after)
                                (statement-case-open after)
                                (extern-lang-open after)
                                (namespace-open after)
                                (module-open after)
                                (composition-open after)
                                (inexpr-class-open after)
                                (inexpr-class-close before after)
                                (class-open        after)
                                (class-close       before after)
                                (brace-list-open   after)
                                (brace-list-close  before after)
                                (inline-open       after)
                                (inline-close      before after)
                                (brace-entry-open  after)
                                (substatement-open after)
                                (brace-entry-open  after)
                                (block-open        after)
                                (block-close       . c-snug-do-while)
                                (else-clause       after)
                                (arglist-cont-nonempty after)
                                (arglist-open after)))
   (c-offsets-alist          . ((inexpr-class . 0)
                                (inexpr-statement . 0)
                                (lambda-intro-cont . 0)
                                (inlambda . 0)
                                (template-args-cont c-lineup-template-args 0)
                                (incomposition . 0)
                                (inmodule . 0)
                                (innamespace . 0)
                                (inextern-lang . 0)
                                (composition-close . 0)
                                (module-close . 0)
                                (namespace-close . 0)
                                (extern-lang-close . 0)
                                (composition-open . 0)
                                (module-open . 0)
                                (namespace-open . 0)
                                (extern-lang-open . 0)
                                (objc-method-call-cont
                                 . c-lineup-ObjC-method-call)
                                (objc-method-args-cont
                                 . c-lineup-ObjC-method-args)
                                (objc-method-intro . [0])
                                (friend . 0)
                                (cpp-define-intro c-lineup-cpp-define 0)
                                (cpp-macro-cont . 0)
                                (cpp-macro . [0])
                                (inclass . 0)
                                (stream-op . c-lineup-streamop)
                                (arglist-cont-nonempty
                                 . javascript-fixed-c-lineup-arglist)
                                (arglist-cont . 0)
                                (comment-intro
                                 c-lineup-knr-region-comment
                                 c-lineup-comment)
                                (catch-clause . 0)
                                (else-clause . 0)
                                (do-while-closure . 0)
                                (access-label . -)
                                (case-label . 0)
                                (substatement . 0)
                                (statement-case-intro . 0)
                                (statement . 0)
                                (brace-entry-open . 0)
                                (brace-list-entry . 0)
                                (brace-list-intro . +)
                                (brace-list-close . 0)
                                (block-close . 0)
                                (block-open . 0)
                                (inher-cont . 0)
                                (inher-intro . 0)
                                (member-init-cont
                                 . c-lineup-multi-inher)
                                (member-init-intro . 0)
                                (topmost-intro-cont . 0)
                                (topmost-intro . 0)
                                (knr-argdecl . 0)
                                (func-decl-cont . 0)
                                (inline-close . 0)
                                (class-close . 0)
                                (class-open . 0)
                                (defun-block-intro . +)
                                (defun-close . 0)
                                (defun-open . 0)
                                (c . c-lineup-C-comments)
                                (string . c-lineup-dont-change)
                                (brace-list-open . 0)
                                (inline-open . 0)
                                (arglist-close . 0)
                                (arglist-intro . 0)
                                (statement-cont . 0)
                                (statement-case-open . 0)
                                (label . +)
                                (substatement-label . 2)
                                (substatement-open . 0)
                                (knr-argdecl-intro . 0)
                                (statement-block-intro . +)))))


;;-----------------------------------------------------------
;;Haskell stuff...
;;-----------------------------------------------------------
(load "~/elisp/haskell-site-file")

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
;;-----------------------------------------------------------


;;-----------------------------------------------------------
;;OCaml stuff
;;-----------------------------------------------------------
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
  (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
  (autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;;no more yes-or-no questions please...
(fset 'yes-or-no-p 'y-or-n-p)


(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)


(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(global-set-key [f5] 'call-last-kbd-macro)

;;syntax hilighting is teh roxors
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))


;;remote file editing
(require 'tramp)
;;plink works best for me on win32
(setq tramp-default-method "ssh")

;;goto line would be nice...
(global-set-key "\C-xg" 'goto-line)
(global-set-key "\C-x\C-g" 'goto-line)

;;complete recently used words
(require 'completion)
(initialize-completions)

;;I dnt spel to nice
;;(setq ispell-program-name "aspell")
;;(setq exec-path (cons "c:/tools/aspell/bin" exec-path))
(require 'ispell)

;;colors... pretty colors
;;(color-theme-montz) ;looks like crap in gnuemacs
(require 'color-theme)
;;(color-theme-deep-blue)
;;(color-theme-subtle-hacker)
;;(color-theme-late-night)
;;(color-theme-charcoal-black)
;;(color-theme-midnight)
(color-theme-clarity)


;; Set up mouse wheel
(defun up-slightly () (interactive) (scroll-up 5))
(defun down-slightly () (interactive) (scroll-down 5))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)

(defun up-one () (interactive) (scroll-up 1))
(defun down-one () (interactive) (scroll-down 1))
(global-set-key [S-mouse-4] 'down-one)
(global-set-key [S-mouse-5] 'up-one)

(defun up-a-lot () (interactive) (scroll-up))
(defun down-a-lot () (interactive) (scroll-down))
(global-set-key [C-mouse-4] 'down-a-lot)
(global-set-key [C-mouse-5] 'up-a-lot)


;; disable startup message
(setq inhibit-startup-message t)

;; display the current time
(display-time)

;; Show column number at bottom of screen
(column-number-mode 1)

;; highlight matches from searches
(setq isearch-highlight t)
(setq search-highlight t)
(setq-default transient-mark-mode t)

;; format the title-bar to always include the buffer name
;;(setq frame-title-format "emacs@   -   %b")


;;cedet stuff... playing

;;w3
;;(setq load-path (cons "~/elisp/w3-4.0pre.47/lisp" load-path))
;;(require 'w3-auto)


(require 'htmlize)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.33beta1")
 '(ecb-wget-setup (quote cons))
 '(ido-mode (quote ) nil (ido))
 '(rails-ws:default-server-type "mongrel"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; X copy pasta
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)