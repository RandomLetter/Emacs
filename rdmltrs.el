;;Create new buffers without forced ido completion
(setq ido-create-new-buffer 'always) ;; THIS DOES NOT WORK annoying use C-j as a work around

;;Custome Themes Load Path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


;;Nice Starter Kits
(starter-kit-load "python")
(starter-kit-load "js")
(starter-kit-load "ruby")


;;Turn off global hi-line mode
(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(global-hl-line-mode nil)
(remove-hook 'coding-hook 'turn-on-hl-line-mode)
(hl-line-mode 0)
(global-hl-line-mode -1)

;; Turn off default directory (bug exiting ido-completion to create new file
;;This also does not work!!
 (setq insert-default-directory 'nil)

;;Customizing initial frame size and line width
(add-to-list 'default-frame-alist '(height . 55))
(add-to-list 'default-frame-alist '(width . 82))
(setq-default fill-column 80)
(setq auto-fill-mode 1)

;;Turn off annoying sound!
(setq ring-bell-function 'ignore) ;; Just Turn Off bell function
;; Remove Scratch Buffer Text
(setq initial-scratch-message nil)

;;use things installed by homebrew
(push "/usr/local/bin" exec-path)

;; Set site-lisp location
(let ((default-directory "~/.emacs.d/site-lisp/"))
(normal-top-level-add-to-load-path '("."))
(normal-top-level-add-subdirs-to-load-path))

;; Set cursor type
(setq-default cursor-type 'bar)
(blink-cursor-mode 1) 

;; Use 12-pt Bold Consolas as default font
(set-face-attribute 'default nil
                    :family "Consolas" :height 140 :weight 'bold)

;; ESS - emacs speaks statistics
;;(require 'ess-site)

;; Lose Distracting UI elements
;;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;ORG MODE
;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only


;;Color ZenBURN!!!
;;(push "~/.emacs.d/site-lisp/" custom-theme-load-path)
;;(load-theme 'zenburn)
;; Effective Emacs Customizations

;; Invoke M-x without Alt Key: C-x(c)-C-m = M-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;; Shortcut Backward-Kill-Word
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region) ;; new kill region cmds
(global-set-key "\C-c\C-k" 'kill-region)

;; Org-Mode Remember
(org-remember-insinuate) 
(setq org-directory "~/Folio/") 
(setq org-default-notes-file (concat org-directory "/notes.org")) 
(define-key global-map "\C-cr" 'org-remember)

;;Full Screen
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)


;; PYTHON EMACS
(add-to-list 'load-path "~/.emacs.d/")

;;autocomplete
;;(require 'auto-complete)
;;(global-auto-complete-mode t)

;;YAS SNIPPET
;;(require 'yasnippet-bundle)

;;'Normal YASNIPPET install 
;;(require 'yasnippet)


;;make them behave with each other
;;(require 'auto-complete)
;;(require 'yasnippet)
;;(global-auto-complete-mode t)

