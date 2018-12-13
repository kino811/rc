;;; This is Kino's .emacs

;;; add load-path
(add-to-list 'load-path "~/.emacs.d/lisp")

;; adding a package source
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/")
	       t)
  )

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(buffer-file-coding-system (quote utf-8) t)
 '(current-language-environment "English")
 '(electric-pair-inhibit-predicate (quote electric-pair-conservative-inhibit))
 '(electric-pair-mode t)
 '(helm-gtags-prefix-key "C-c g")
 '(helm-gtags-suggested-key-mapping t)
 '(org-agenda-files (quote ("~/work/kaiser/todo.org")))
 '(package-selected-packages
   (quote
    (w3 company-anaconda company-box evil dotnet omnisharp jedi helm-gtags el-get use-package google-c-style irony-eldoc exec-path-from-shell helm-company flycheck-irony company-irony-c-headers company-irony irony cmake-ide rtags cmake-mode plantuml-mode wsd-mode use-package-chords key-chord use-package python-docstring company-glsl flymake-yaml yaml-mode flycheck-pycheckers flymake-json flymake-lua flymake-shell flycheck company-jedi company-lua company-shell company wgrep-ag wgrep-helm projectile-ripgrep swiper-helm ripgrep rg helm-rg ibuffer-projectile org-projectile helm-projectile yasnippet-snippets yasnippet mark-multiple ace-jump-mode edit-server which-key multi-term wgrep iedit avy swiper prodigy eyebrowse projectile csharp-mode airline-themes powerline magit solarized-theme helm)))
 '(projectile-keymap-prefix "p")
 '(safe-local-variable-values (quote ((cmake-tab-width . 4)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package)
  )

;; hide toolbar and menu
(tool-bar-mode -1)

;; don't show splash-screen
(setq inhibit-splash-screen t)

;; set theme
(add-to-list 'custom-theme-load-path 
	     "~/.emacs.d/elpa/solarized-theme-20170831.1159")
(load-theme 'solarized-dark t)

;; highlight brackets
(show-paren-mode t)

;; save/restore opend files and windows config
(desktop-save-mode t)

(which-key-mode)

;; exec path
(if (eq system-type 'darwin)
    (exec-path-from-shell-initialize)
  )

;; windmove
(use-package windmove
  :ensure t
  :bind
  ("C-c w h" . windmove-left)
  ("C-c w j" . windmove-down) 
  ("C-c w k" . windmove-up)   
  ("C-c w l" . windmove-right)
  :config
  (windmove-default-keybindings 'meta)
  )

;; keep a list of recently opened files
(use-package recentf
  :ensure t
  :bind
  (("C-c f r" . 'recentf-open-files))
  :config
  (recentf-mode 1)
  )

;; 
;; alias
(defalias 'list-buffers 'ibuffer)
(defalias 'yes-or-no-p 'y-or-n-p)
;; 

(use-package powerline
  :ensure t
  :config
  (display-time-mode t)  
  )

;; 
;; bat-mode
(defun kino-call-process-shell-async-current-buffername ()
  "For bat-mode shell-command by current-buffername"
  (interactive)
  (call-process-shell-command 
   (format "start cmd /c %s" (buffer-name)))
  )

(defun kino-set-bat-mode-hook ()
  "Set bat-mode hook"
  (interactive)
  (local-set-key (kbd "<f5>") 'kino-call-process-shell-async-current-buffername))

(require 'bat-mode)
(add-hook 'bat-mode-hook 'kino-set-bat-mode-hook)
;;

(require 'helm-config)
;; helm
(use-package helm
  :ensure t
  :bind-keymap
  ("C-c h" . helm-command-map)
  :bind
  (("C-x C-f" . helm-find-files)
   ("C-x r b" . helm-filtered-bookmarks)
   ("M-y" . helm-show-kill-ring)
   ("M-x" . helm-M-x)
   ("C-x b" . 'helm-mini)
   )
  (:map helm-command-map
	("o" . helm-occur)
	("SPC" . helm-all-mark-rings)
	)
  :config
  (global-unset-key (kbd "C-x c"))
  (setq helm-split-window-inside-p t)
  (setq helm-autoresize-max-height 50)
  (setq helm-autoresize-min-height 30)
  (helm-autoresize-mode 1)
  (helm-mode 1)
  )

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :config
  (setq projectile-keymap-prefix (kbd "C-p p"))
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  )

(use-package edit-server
  :ensure t
  :config
  (edit-server-start)
  )

(use-package org
  :ensure t
  :after ob
  :bind
  (:map org-mode-map
	("C-c C-c" . (lambda ()
			(interactive) 
			(org-ctrl-c-ctrl-c)
			(org-display-inline-images))))
  :config
  ;; make org mode allow eval of some langs
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (C . t)
     (plantuml . t)))

  (setq org-src-fontify-natively t)
  (setq org-todo-keywords
	'((sequence "TODO" "PROGRESS" "WAITING" "DONE")))

  (setq org-plantuml-jar-path
	(expand-file-name "~/.emacs.d/plantuml.jar"))
  (add-hook 'org-babel-after-execute-hook
	    (lambda ()
	      (when org-inline-image-overlays
		(org-redisplay-inline-images))
	      )
	    )
  (add-to-list 'org-structure-template-alist
	       '("u" "#+BEGIN_SRC plantuml :file ?.png\n
skinparam monochrome true\n
#+END_SRC"))

  (setq org-startup-with-inline-images t)
  )

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  )

(use-package swiper
  :ensure t
  :bind
  (:map global-map ("C-c s s" . 'swiper))
  )

(use-package iedit
  :ensure t
  :bind
  (:map global-map ("C-c ;" . 'iedit-mode))
  )

(use-package avy
  :ensure t
  :bind
  (:map global-map ("C-c j c" . 'avy-goto-char))
  (:map global-map ("C-c j l" . 'avy-goto-line))
  :config
  (avy-setup-default)
  )

(use-package wgrep)

(require 'multi-term)
;; todo :: not work now.
;; (if (eq system-type 'windows-nt)
;;     (setq multi-term-program "c:/cygwin64/bin/bash.exe")
;;   )
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; undo-tree-mode
(global-undo-tree-mode)

;; shader-mode
(add-to-list 'auto-mode-alist
	     '("\\.shader\\'" . shader-mode))

(global-linum-mode)

(use-package magit
  :ensure t
  :init
  (setenv "GIT_ASKPASS" "git-gui--askpass")
  :bind
  ("C-x g" . magit-status)
  )

(use-package jedi
  :ensure t
  :after company
  :config
  (defun my/python-mode-hook ()
    (with-eval-after-load 'company (add-to-list 'company-backends 'company-jedi))
    )
  (add-hook 'python-mode-hook 'my/python-mode-hook)
  )

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings (kbd "C-c R"))
  )

;; python simple server
(defun my:open-server-working-dir-http ()
  (interactive)
  (let (shell-cmd)
    (setq shell-cmd "python3 -m http.server")
    (if (eq system-type 'windows-nt)
	(setq shell-cmd (concat "start " shell-cmd))
      (if (eq system-type 'darwin)
	  (setq shell-cmd (concat "open " shell-cmd))
	)
      )
    (async-shell-command shell-cmd)
    )
  )

(defun my:open-server-working-dir-ftp ()
  (interactive)
  (let (shell-cmd)
    (setq shell-cmd "python3 -m pyftpdlib")
    (if (eq system-type 'windows-nt)
	(setq shell-cmd (concat "start " shell-cmd))
      (if (eq system-type 'darwin)
	  (setq shell-cmd (concat "open " shell-cmd))
	)
      )
    (async-shell-command shell-cmd)
    )
  )


;; cursor line always highlighted
(if (display-graphic-p)
    (global-hl-line-mode 1)
  )


;; when a file is updated outside emacse, make it update if it's already opend in emacs
(global-auto-revert-mode 1)

(use-package use-package-chords
  :ensure t
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.1)	;default 0.1
  )

;; insert buffer-name at minibuffer
(defun insert-buffer-name ()
  (interactive)
  (insert (buffer-name (window-buffer (minibuffer-selected-window)))))
(define-key minibuffer-local-map (kbd "C-c i b n")
  'insert-buffer-name)

;; company
(use-package company
  :bind
  (:map global-map ("C-c c c" . 'company-complete))
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package company-anaconda
  :after (anaconda-mode company)
  :config (add-to-list 'company-backend 'company-anaconda))

(use-package company-irony
  :ensure t
  :after (company irony)
  :config
  (add-to-list 'company-backends 'company-irony)
  )

;; flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
  )

;; open gui file manager
(defun open-file-manager-cwd ()
  (interactive)
  (if (eq system-type 'windows-nt)
      (async-shell-command "explorer .")
    (if (eq system-type 'darwin)
	(async-shell-command "open .")
	)
      )
  )

(use-package irony
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  
  (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
						  irony-cdb-clang-complete))

  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0))
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
  
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc)
  )

(use-package irony-eldoc
  :ensure t
  :after (eldoc irony)
  :config
  (add-hook 'irony-mode-hook #'irony-eldoc)
  )

(use-package google-c-style
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'google-set-c-style)
  (add-hook 'c-mode-common-hook 'google-make-newline-indent)
  )

(use-package helm-gtags
  :ensure t
  :after (helm)
  :init
  :config
  (add-hook 'c-mode-common-hook 'helm-gtags-mode)
  )

(defun my:set-c-common-style ()
  (c-set-style "google")
  (setq tab-width 4)
  (setq c-basic-offset 4)
  )
(add-hook 'c-mode-common-hook 'my:set-c-common-style)

(defun my:kill-other-buffers ()
  "kill all other buffers"
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; cs mode
(add-hook 'csharp-mode-hook 'omnisharp-mode)
(eval-after-load
    'company
  '(add-to-list 'company-backends 'company-omnisharp))
(add-hook 'csharp-mode-hook #'company-mode)

(defun open-google-translate ()
  "Translate current word using Google Translator"
  (interactive)
  (let (sel-word-target-url)
    (setq sel-word
	  (if (and transient-mark-mode mark-active)
	      (buffer-substring-no-properties (region-beginning) (region-end))
	    (thing-at-point 'symbol)))
    (setq sel-word (replace-regexp-in-string " " "%20" sel-word))
    (setq target-url (concat "http://translate.google.com/#auto|ko|" sel-word))
    (browse-url target-url)))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(define-abbrev-table 'global-abbrev-table
  '(
    ("kinog" "kino811@gmail.com")
    ("kinon" "kino811@naver.com")
    ("kino8" "kino811")
    ))

(put 'set-goal-column 'disabled nil)

;; 
;; custom key-map
(global-set-key (kbd "C-<kanji>") 'set-mark-command)
(global-set-key (kbd "C-x C-<kanji>") 'pop-global-mark)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (2kbd "C-c t") 'open-google-translate)
;;
