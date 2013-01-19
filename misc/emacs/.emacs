                                        ; PACKAGE INITIALIZATION
(add-to-list 'load-path "~/.emacs.d/packages")

;; Configure package management:
(require 'package)
(add-to-list 'package-archives 
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

                                        ; UTILITY FUNCTIONS
(defun easy-move ()
  (local-set-key (kbd "n") 'next-line)
  (local-set-key (kbd "p") 'previous-line)
  (local-set-key (kbd "l") 'recenter-top-bottom))

(defun quit ()
  "Prompts to save unsaved buffers and then kills the emacs server."
  (interactive)
  (save-some-buffers)
  (kill-emacs))

;; A list of opposite boolean pairs.
(defvar bools '(("true" . "false") ("True" . "False") ("#t" . "#f")
                ("yes" . "no") ("Yes" . "No")))
      
(defun flip-bool-at-point ()
  "Flips the boolean literal at point, changing true to false and
vice-versa."
  (interactive)
  (let* ((true  (cdr (assoc  (current-word) bools)))
         (false (car (rassoc (current-word) bools)))
         (wrd (cond (true true)
                    (false false)
                    (t (current-word)))))
    (save-excursion
      (forward-word)
      (backward-kill-word 1)
      (insert wrd))))
(global-set-key (kbd "C-c C-b") 'flip-bool-at-point)
(global-set-key (kbd "C-c b") 'flip-bool-at-point)

(defun duplicate-line ()
  "Duplicates the current line."
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)))
(global-set-key (kbd "C-c C-d") 'duplicate-line)

;; Insert TODO comments programmatically:
(defun todo-comment ()
  "Inserts an empty TODO comment or makes an existing comment
into a TODO."
  (interactive)
  (when (not (region-active-p))
    (comment-dwim nil)
    (unless (equal (current-word) "TODO") (insert "TODO: "))))
(global-set-key (kbd "C-c t") 'todo-comment)

(defun file-name-at-point ()
  "Prompts the user for a file path using the standard C-x C-f
interface and inserts it at point."
  (interactive)
  (if ido-mode (insert (ido-read-file-name "file path: "))
    (insert (read-file-name "file path: "))))
(global-set-key (kbd "C-c f") 'file-name-at-point)

                                        ; GENERAL STUFF
;; Auto-complete stuff
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)

;; Have compile scroll to the end by default.
(setq-default compilation-scroll-output 'foo-bar)

;; Flymake stuff
(setq flymake-cursor-error-display-delay 0.1)

;; Flyspell stuff
(add-hook 'flyspell-mode-hook '(lambda ()
				(set-face-attribute 'flyspell-duplicate nil
						    :foreground nil
						    :underline "dark orange"
						    :bold nil)
				(set-face-attribute 'flyspell-incorrect nil
						    :foreground nil
						    :underline "red"
						    :bold nil)))

;; Better commands for window management:
;; swap-with taken from emacsd-tile 0.1 by marius a. eriksen
;; (https://gist.github.com/287633)
(defun swap-with (dir)
  (interactive)
  (let ((other-window (windmove-find-other-window dir)))
    (when other-window
      (let* ((this-window  (selected-window))
             (this-buffer  (window-buffer this-window))
             (other-buffer (window-buffer other-window))
             (this-start   (window-start this-window))
             (other-start  (window-start other-window)))
        (set-window-buffer this-window  other-buffer)
        (set-window-buffer other-window this-buffer)
        (set-window-start  this-window  other-start)
        (set-window-start  other-window this-start)))))

(global-set-key (kbd "C-M-S-N") (lambda () (interactive) (swap-with 'down)))
(global-set-key (kbd "C-M-S-P") (lambda () (interactive) (swap-with 'up)))
(global-set-key (kbd "C-M-S-B") (lambda () (interactive) (swap-with 'left)))
(global-set-key (kbd "C-M-S-F") (lambda () (interactive) (swap-with 'right)))

(global-set-key (kbd "M-N") 'windmove-down)
(global-set-key (kbd "M-P") 'windmove-up)
(global-set-key (kbd "M-B") 'windmove-left)
(global-set-key (kbd "M-F") 'windmove-right)

(ido-mode t)
(setq ido-default-buffer-method 'selected-window)

;; Some minor preferences:
(setq visible-bell t)
(show-paren-mode 1)
(column-number-mode t)
(transient-mark-mode -1)
(setq mark-even-if-inactive t)
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

;; For enabling color themes:
(setq custom-theme-directory "~/.emacs.d/themes/")
(setq custom-safe-themes t)
(load-theme 'blackboard)

;;Make the window simpler:
(tool-bar-mode -1)
(scroll-bar-mode -1) 
(menu-bar-mode -1)
(fringe-mode 0)
(menu-bar-mode -1)

;; Now make it prettier:
(require 'powerline)
(set-face-attribute 'mode-line nil
                    :foreground "Black"
                    :background "DarkOrange"
                    :box nil)
(setq powerline-arrow-shape 'arrow)

;; Rainbow parentheses
(defun rainbow-delimiters-colors ()
  (set-face-foreground 'rainbow-delimiters-depth-1-face "dark red")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "dark green")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "deep pink")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "yellow")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "green")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "light blue")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "orange")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "slate blue")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "light gray")
  (set-face-foreground 'rainbow-delimiters-unmatched-face "white"))
(add-hook 'rainbow-delimiters-mode-hook 'rainbow-delimiters-colors)

;; Get rid of the annoying splash screen:
(setq inhibit-splash-screen t)

;; Have a list of recent files:
(add-hook 'recentf-dialog-mode-hook 'easy-move)
(recentf-mode 1)
(recentf-open-files nil "*Recent Files*")
(setq recentf-max-saved-items 100000)
(global-set-key (kbd "C-x C-a") 'recentf-open-files) ;; Open recent files easily

;; make text-mode the default major mode
(setq default-major-mode 'text-mode)

                                        ; ORG-MODE
;; Spellcheck my org mode files.
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

                                        ; JABBER
;; Spellcheck my jabber conversations.
(add-hook 'jabber-chat-mode-hook 'flyspell-mode)

;; Set up jabber.el to interface nicely with Google talk:
(setq jabber-account-list
      '(("tikhonjelvis@gmail.com/emacs" 
         (:network-server . "talk.google.com")
         (:connection-type . ssl)
         (:password . "qcplmupwavbptlsw"))
        ("tikhon@jelv.is/emacs" 
         (:network-server . "talk.google.com")
         (:connection-type . ssl)
         (:password . "qv785rdf#t"))))
(add-hook 'jabber-roster-mode-hook 'easy-move)
(defvar jabber-blue "#6699FF")
(defvar jabber-red "#FF9966")
(defun jabber-color-hook ()
  (set-face-foreground 'jabber-chat-prompt-local jabber-blue)
  (set-face-foreground 'jabber-roster-user-online jabber-blue)
  (set-face-foreground 'jabber-activity-personal-face jabber-blue)
  (set-face-foreground 'jabber-chat-prompt-other jabber-red))
(add-hook 'jabber-roster-mode-hook 'jabber-color-hook)

;; Do nothing on C-x C-c:
(global-unset-key (kbd "C-x C-c"))
;; I'm phasing C-x o out:
(global-set-key (kbd "C-x o") 'other-frame)

;; Make complete tag not be alt-tab!
(global-set-key (kbd "M-<return>") 'complete-tag)

;; Some nice keyboard shortcuts:
(global-set-key (kbd "C-x 5 3") 'make-frame-command)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-j") 'compile)
(global-set-key (kbd "C-c a") 'align-regexp)
(global-set-key (kbd "M-#") 'ispell-complete-word)

;; C-w remap:
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)

                                        ; SHELL BUFFERS
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
                        (concat "export PS1=\"\033[33m" name "\033[0m:\033[35m\\W\033[0m>\""))))
(global-set-key (kbd "C-c s") 'new-shell)

;; ANSI colors in shell mode would be nice by default:
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setq ansi-color-names-vector ["white" "light red" "green" "yellow" "pale blue" "magenta" "cyan" "tan"])

;; A mode to handle buffers gotten from stdout:
(require 'stdout-mode)

                                        ; ELISP
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

                                        ; COQ
(load-file "/home/tikhon/Documents/cs/263/coq/ProofGeneral/generic/proof-site.el")
(setq coq-prog-name "/usr/bin/coqtop")
(require 'proof-site)

(setq proof-three-window-enable nil)
(setq proof-splash-enable nil)
(setq proof-shrink-windows-tofit t)
(add-hook 'proof-mode-hook (lambda () (set-input-method "TeX") ))
(add-hook 'proof-mode-hook (lambda ()
  (proof-electric-terminator-toggle t)
  (set (make-local-variable 'overlay-arrow-string) nil)
  (setq proof-strict-read-only t)
  (setq PA-one-command-per-line nil)
  (define-key proof-mode-map "\C-c\C-a" 'proof-retract-until-point-interactive)
  (define-key proof-mode-map "\C-c\C-e" 'proof-assert-until-point-interactive)
  (define-key proof-mode-map "\C-\\" 'proof-display-some-buffers)
  ;; hack for pre-release
  (defun proof-script-next-commmand-advance ())
  ))
(defun proof-script-next-commmand-advance ())
(add-hook 'proof-shell-mode-hook
          (lambda ()
            (set-process-query-on-exit-flag
             (get-buffer-process (current-buffer)) nil)))

                                        ; HASKELL
;; Load Haskell mode:
(defun haskell-save-and-format ()
  "Formats the import statements using haskell-stylish and saves
the current file."
  (interactive)
  (save-buffer)
  (haskell-mode-stylish-buffer)
  (save-buffer))

(defun my-haskell-load-and-run ()
  "Loads and runs the current Haskell file."
  (interactive)
  (let ((start-buffer (current-buffer)))
    (inferior-haskell-load-and-run inferior-haskell-run-command)
    (sleep-for 0 100)
    (end-of-buffer)
    (pop-to-buffer start-buffer)))
(setq inferior-haskell-run-command "main")

(defun my-haskell-mode-hook ()
  (local-set-key (kbd "C-x C-s") 'haskell-mode-save-buffer)
  (local-set-key (kbd "C-c C-s") 'haskell-save-and-format)
  (local-set-key (kbd "C-c C-r") 'my-haskell-load-and-run))

(defun my-inferior-haskell-mode-hook ()
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))

(require 'haskell-project-mode)

(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'haskell-project-mode)
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)
(add-hook 'inferior-haskell-mode-hook 'my-inferior-haskell-mode-hook)
(setq haskell-font-lock-symbols t)
(setq delete-selection-mode nil)
(setq inferior-haskell-find-project-root nil)

;; ghc-mod
(defun ghc-mod-init-hook ()
  (ghc-init)
  (local-set-key (kbd "C-c C-j") 'compile)
  (local-unset-key (kbd "M-t")))

(add-to-list 'load-path "~/.cabal/share/ghc-mod-1.11.2/")
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook 'ghc-mod-init-hook)

;; hpaste integration
(load "hpaste/hpaste")
(require 'hpaste)

                                        ; OCAML
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . typerex-mode))
(add-to-list 'interpreter-mode-alist '("ocamlrun" . typerex-mode))
(add-to-list 'interpreter-mode-alist '("ocaml" . typerex-mode))
(autoload 'typerex-mode "typerex" "Major mode for editing Caml code" t)

(setq ocp-server-command "/home/tikhon/local/bin/ocp-wizard")
(setq ocp-auto-complete t)
(setq ocp-theme "tuareg")
(setq ocp-prefix-key (kbd "C-;"))
(setq typerex-font-lock-symbols 't)
(setq typerex-use-abbrev-mode nil)

;; (setq ocp-flymake-available 't)
;; (add-hook 'typerex-mode-hook '(lambda () (flymake-mode) (flymake-cursor-mode)))

                                        ; RACKET
(require 'quack)
(setq quack-remap-find-file-bindings-p nil)
(setq quack-fontify-style nil)
(setq quack-default-program "racket")

(defun scheme-enter-module (module-path)
  "Enter the module of the specified file in the active Racket process."
  (message (format "Entering %s." module-path))
  (comint-send-string (scheme-proc) (format "(enter! \"%s\")\n" module-path)))
(defun scheme-load-file (file-path)
  "Loads the given file into the running Scheme process."
  (message (format "Loading: %s." file-path))
  (comint-send-string (scheme-proc) (format "(load \"%s\" )\n" file-path)))

(defun scheme-enter-current-file ()
  "Enters the module of the current file."
  (interactive)
  (save-buffer)
  (scheme-enter-module (file-name-nondirectory (buffer-file-name)))
  (pop-to-buffer scheme-buffer))
(defun scheme-load-current-file ()
  "Loads the current file into the running Scheme process."
  (interactive)
  (save-buffer)
  (scheme-load-file (buffer-file-name))
  (pop-to-buffer scheme-buffer))

(defun my-scheme-hook ()
  (paredit-mode)
  (local-set-key (kbd "C-c C-l") 'scheme-enter-current-file)
  (local-set-key (kbd "C-c M-l") 'scheme-load-current-file)
  (local-set-key (kbd "M-[") 'paredit-wrap-square))
(add-hook 'scheme-mode-hook 'my-scheme-hook)

;; TODO: Use this as an ac-source for scheme and inferior scheme buffers.
(defvar scheme-symbols-command "
       (set->list (set-subtract
            (list->set (namespace-mapped-symbols))
            (list->set (namespace-mapped-symbols (module->namespace 'racket)))))
")

(defun my-inferior-scheme-hook ()
  (paredit-mode)
  (auto-complete-mode))
(add-hook 'inferior-scheme-mode-hook 'my-inferior-scheme-hook)

                                        ; FORTH
(defun forth-load-current-file ()
  "Loads the currently visited file into a running forth process."
  (interactive)
  (save-buffer)
  (let ((name (buffer-file-name)))
    (unless (and forth-process-buffer
                 (get-buffer forth-process-buffer)
                 (get-buffer-process forth-process-buffer))
      (run-forth forth-program-name))
    (forth-load-file name)
    (pop-to-buffer forth-process-buffer)
    (end-of-buffer)))
(add-hook 'forth-mode-hook
          '(lambda () (local-set-key (kbd "C-c C-l") 'forth-load-current-file)))

                                        ; ARRAYFORTH
(require 'array-forth-mode)
(add-to-list 'auto-mode-alist '("\\.\\(cfs\\|forth\\)" . array-forth-mode))
(setq array-forth-trim-markers t)

                                        ; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(defun my-markdown-hook ()
  (pandoc-mode)
  (flyspell-mode)
  (longlines-mode)
  (flyspell-buffer)
  (local-unset-key (kbd "C-M-b"))
  (local-unset-key (kbd "C-M-f"))  )
(add-hook 'markdown-mode-hook 'my-markdown-hook)
(setq markdown-enable-math t)

                                        ; LATEX
(defun my-LaTeX-hook ()
  "Turn on wrapping and spell-check for LaTeX documents."
  (flyspell-mode)
  (auto-fill-mode))
(add-hook 'LaTeX-mode-hook 'my-LaTeX-hook)

;; PDF Mode by default:
(add-hook 'LaTeX-mode-hook 'TeX-PDF-mode) ;;pdf mode by default

;; Continuous scroll when viewing pdfs:
(setq doc-view-continuous 1)

;; Automatically update pdfs:
(global-auto-revert-mode t)

;; Pandoc stuff:
(setq pandoc-binary "pandoc")

                                        ; CFDG
(require 'cfdg-mode)
(add-to-list 'auto-mode-alist '("\\.cfdg" . cfdg-mode))

                                        ; TPL
(require 'tpl-mode)

                                        ; CS 164
(require 'cs164-mode)
(setq cs164-base-directory "~/Documents/cs/164/p/2/cs164sp12/pa2/")
(setq cs164-grammar "cs164b.grm")

;;                                         ; Prolog
;; (setq load-path (cons "/usr/lib/xemacs/site-lisp" load-path))                 
;; (autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)               
;; (autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)  
;; (autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
;; (setq prolog-system 'swi) 
;; (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)                      
;;                                 ("\\.m$" . mercury-mode))                     
;;                                auto-mode-alist)) 

;;                                         ; LUA
;; ;; I just want to test lua mode out:
;; (autoload 'lua-mode "lua-mode" "Lua editing mode." t)    
;; (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))   
;; (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


                                        ; WEB DEVELOPMENT
;; Make the default browser googly chrome:
(setq browse-url-generic-program "google-chrome"
      browse-url-browser-function 'browse-url-generic)

;; Make JS-2 mode the default:
(add-to-list 'auto-mode-alist '("\\.js" . js2-mode))
(setq js2-basic-offset 2)

;; Edit .less files with css mode:
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

;; ;; ∀ x ∈ gosu-program-profiles: buffer ≡ 〈*,x,*〉 → (gosu-program-mode buffer)
;; (defun my-program-mode-profile-hook ()
;;   (let ((name (buffer-name))
;;         (mode (if (string-match-p "\\*.*\\*" name)
;;                   (substring name 1 -1) name)))
;;     (when ((assoc mode gosu-program-profiles))
;;       (gosu-program-mode)
;;       (gosu-program-profile-by-name mode))))
;; (add-hook 'shell-mode-hook 'my-program-mode-profile-hook)

;; If I'm at work, make sure python-indent is set to 4:
(defun my-python-work-settings-hook ()
  (if (string-match-p ".*/Documents/work.*" default-directory)
      (setq python-indent 4)))
(add-hook 'python-mode-hook 'my-python-work-settings-hook)

;; I don't do much php, so let's edit it with html mode:
(add-to-list 'auto-mode-alist '("\\.php$" . html-mode))

                                        ; CUSTOM-SET STUFF
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(send-mail-function (quote sendmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(flymake-errline ((t (:background "#00000000" :underline "red"))))
 '(flymake-warnline ((t (:background "#00000000" :underline "dark orange"))))
 '(sgml-namespace ((t (:inherit font-lock-builtin-face)))))

                                        ; COMMANDS
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'set-goal-column 'disabled nil)
