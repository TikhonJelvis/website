                                        ; UTILITY 
(defun easy-move ()
  (local-set-key (kbd "n") 'next-line)
  (local-set-key (kbd "p") 'previous-line)
  (local-set-key (kbd "l") 'recenter-top-bottom))

;; I really have to make my bool flipping function prettier, probably
;; by using an assoc list or something.
(defun is-bool (str)
  (member str '("true" "True" "#t" "t" "false" "False" "#f" "nil" "yes" "no" "Yes" "No")))
      

(defun flip-bool-at-point ()
  "Flips the boolean literal at point, changing true to false and
vice-versa."
  (interactive)
  (if (is-bool (current-word))
    (save-excursion
      (forward-word)
      (let ((wrd (current-word)))
        (backward-kill-word 1)
        (insert (cond
                 ((equal wrd "true") "false")
                 ((equal wrd "True") "False")
                 ((equal wrd "#t") "#f")
                 ((equal wrd "yes") "no")
                 ((equal wrd "Yes") "No")
                 ((equal wrd "false") "true")
                 ((equal wrd "False") "True")
                 ((equal wrd "#f") "#t")
                 ((equal wrd "no") "yes")
                 ((equal wrd "No") "Yes")
                 (t wrd)))))))

                                        ; GENERAL STUFF
;; Add my ~/elisp directory and all subdirectories to load-path
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir (expand-file-name "~/elisp"))
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))

;; Make emacs whine:
(load "whine.el")

;; I want an easy command for opening new shells:
(defun new-shell (name)
  "Opens a new shell buffer with the given name in
asterisks (*name*) in the current directory with and changes the
prompt to name>."
  (interactive "sName: ")
  (pop-to-buffer (concat "*" name "*"))
  (unless (eq major-mode 'shell-mode)
    (shell (current-buffer))
    (sleep-for 0 200)
    (delete-region (point-min) (point-max))
    (comint-simple-send (get-buffer-process (current-buffer)) 
                        (concat "export PS1='" name ">'"))))

;; A better mode for proper terminal interaction:
(load "multi-term.el")
(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; Better commands for window management:
(require 'emacs-tile)
(ido-mode t)
(setq ido-default-buffer-method 'selected-window)

;; Get rid of the annoying beeping:
(setq visible-bell t)
;; Reasonable parentheses mode:
(show-paren-mode 1)
;; Show the column number:
(column-number-mode t)
;; Transient mark mode is for the weak!
(transient-mark-mode -1)
;; I want lines truncated by default:
(setq-default truncate-lines t)

;; Auto-indenting on RET:
(add-hook 'c-mode-common-hook '(lambda ()
                                 (local-set-key (kbd "RET") 
                                                'newline-and-indent)))
(add-hook 'python-mode-hook '(lambda ()
			       (local-set-key (kbd "RET") 
					      'newline-and-indent)))

;; I don't like tabs very much:
(setq-default indent-tabs-mode nil)

;;Make the window simpler:
(tool-bar-mode -1)
(scroll-bar-mode -1) 
(menu-bar-mode -1)
(fringe-mode 0)
(menu-bar-mode -1)

;; Get rid of the annoying splash screen:
(setq inhibit-splash-screen t)

;; For enabling color themes:
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-blackboard)))

;; Have a list of recent files:
(recentf-mode 1)
(recentf-open-files nil "*Recent Files*")
(setq recentf-max-saved-items 1200)
(add-hook 'recentf-dialog-mode-hook 'easy-move)

;; make text-mode the default major mode and start auto-fill mode
(setq default-major-mode 'text-mode)

;; Do nothing on C-x C-c:
(global-unset-key (kbd "C-x C-c"))
;; I'm phasing C-x o out:
(global-unset-key (kbd "C-x o"))

;; Make complete tag not be alt-tab!
(global-set-key (kbd "M-<return>") 'complete-tag)
;; Some nice keyboard shortcuts:
(global-set-key (kbd "C-x 5 3") 'make-frame-command)
;; Open recent files easily:
(global-set-key (kbd "C-x C-a") 'recentf-open-files)
;; Hippie-expand!
(global-set-key (kbd "M-/") 'hippie-expand)
;; C-w remap:
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-c C-k") 'kill-region)
;; Easy compiling:
(global-set-key (kbd "C-c C-j") 'compile)
;; Align stuff:
(global-set-key (kbd "C-c a") 'align-regexp)
(global-set-key (kbd "M-#") 'ispell-complete-word)
(global-set-key (kbd "C-c s") 'new-shell)
(global-set-key (kbd "C-c b") 'flip-bool-at-point)
                                        ; JABBER
;; Spellcheck my jabber conversations.
(add-hook 'jabber-chat-mode-hook 'flyspell-mode)

;; Set up jabber.el to interface nicely with Google talk:
(require 'jabber)
(setq jabber-account-list
      '(("tikhonjelvis@gmail.com" 
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))
(add-hook 'jabber-roster-mode-hook 'easy-move)

                                        ; HASKELL
;; Load Haskell mode:
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(setq haskell-font-lock-symbols t)
(setq delete-selection-mode nil)

;; hpaste integration
(load "hpaste/hpaste")
(require 'hpaste)

                                        ; LISP/SCHEME
;; Set the scheme binary correctly:
(setq scheme-program-name "guile")

                                        ; LATEX
;; Better LaTeX support:
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; PDF Mode by default:
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) ;;pdf mode by default

;; Continuous scroll when viewing pdfs:
(setq doc-view-continuous 1)

;; Automatically update pdfs:
(global-auto-revert-mode t)


                                        ; TPL
(require 'tpl-mode)

                                        ; CS 164
(require 'cs164-mode)

                                        ; Prolog
(setq load-path (cons "/usr/lib/xemacs/site-lisp" load-path))                 
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)               
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)  
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi) 
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)                      
                                ("\\.m$" . mercury-mode))                     
                               auto-mode-alist)) 

                                        ; LUA
;; I just want to test lua mode out:
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)    
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))   
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


                                        ; WEB DEVELOPMENT
;; Make the default browser googly chrome:
(setq browse-url-generic-program "google-chrome"
      browse-url-browser-function 'browse-url-generic)

;; Gosu program mode for server stuff:
(require 'gosu-program-mode)

;; Edit .less files with css mode:
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

;; Django template stuff and Yasnippet
(require 'yasnippet)
(yas/initialize)
(setq yas/prompt-functions '(yas/dropdown-prompt yas/completing-prompt))

(add-hook 'django-html-mode-hook
	  '(lambda () (rng-validate-mode -1)))
(require 'django-html-mode)
(require 'django-mode)
;; Consider thinking about django template extensions.

;; If we are in a templates directory, assume that .html files are django templates:
(defun django-mode-for-templates ()
  (if (string-match-p ".*templates.*" default-directory)
      (progn (sleep-for 0 750)
	     (django-html-mode))));; I think we need to sleep because changing modes is weird.
(add-hook 'sgml-mode-hook 'django-mode-for-templates)

;; If I open a shell called *django*, load gsp mode with profile "django":
(add-hook 'shell-mode-hook
	  '(lambda () (if (string-match-p "\\*django\\*" (buffer-name))
			  (progn (gosu-program-mode)
				 (gosu-program-profile-by-name "django")))))

;; If I'm at work, make sure python-indent is set to 4:
(add-hook 'python-mode-hook
	  '(lambda () (if (string-match-p ".*/Documents/work.*" default-directory)
			  (setq python-indent 4))))

;; An even better version of the better JavaScript mode:
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq-default js2-enter-indents-newline t)

;; I don't do much php, so let's edit it with html mode:
(add-to-list 'auto-mode-alist '("\\.php$" . html-mode))

                                        ; CUSTOM-SET STUFF
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cs164-base-directory "~/Documents/cs/164/p/2/cs164sp12/pa2/")
 '(cs164-grammar "cs164b.grm")
 '(mark-even-if-inactive t)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#0C1021" :foreground "#F8F8F8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(sgml-namespace ((t (:inherit font-lock-builtin-face)))))

                                        ; COMMANDS
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
