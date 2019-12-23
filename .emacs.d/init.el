;;; package --- kino's init

(add-to-list 'load-path "~/.emacs.d/lisp")

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages") t)
  )

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default)))
 '(google-translate-default-source-language "auto")
 '(google-translate-default-target-language "ko")
 '(package-selected-packages
   (quote
    (autopair company-quickhelp python-black jedi google-translate powershell markdown-mode yasnippet-snippets yaml-mode wsd-mode which-key wgrep-helm wgrep-ag w3 use-package-chords swiper-helm solarized-theme rtags rg python-docstring pyenv-mode projectile-ripgrep prodigy plantuml-mode org-projectile omnisharp narrowed-page-navigation narrow-reindent multishell mark-multiple magit jupyter irony-eldoc iedit ibuffer-projectile helm-rg helm-projectile helm-gtags helm-company google-c-style flymake-yaml flymake-shell flymake-lua flymake-json flycheck-pycheckers flycheck-irony eyebrowse exec-path-from-shell evil elpy el-get ein edit-server dotnet company-shell company-lua company-jedi company-irony-c-headers company-irony company-glsl company-anaconda command-log-mode cmake-mode cmake-ide blacken avy airline-themes ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(let ((package-refreshed nil))
  (when (not (package-installed-p 'use-package))
    (package-refresh-contents)
    (setq-local package-refreshed t)
    
    (package-install 'use-package)
    )
  
  (dolist (package-symbol package-selected-packages)
    (when (not (package-installed-p package-symbol))
      (if (not package-refreshed)
	  (progn
	    (package-refresh-contents)
	    (setq-local package-refreshed t)
	    )
	)
      
      (package-install package-symbol)
      )
    )
  )

(when enable-multibyte-characters
  (set-language-environment "Korean"))

(prefer-coding-system 'utf-8)

(subword-mode t)

;; hide toolbar and menu
(tool-bar-mode -1)

;; don't show splash-screen
(setq inhibit-splash-screen t)

;; set theme
(load-theme 'solarized-dark t)

;; highlight brackets
(show-paren-mode t)

;; save/restore opend files and windows config
(desktop-save-mode t)

;; alias
(defalias 'list-buffers 'ibuffer)
(defalias 'yes-or-no-p 'y-or-n-p)

(which-key-mode t)

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

(use-package recentf
  :ensure t
  :config
  (recentf-mode t)
  )

(use-package powerline
  :ensure t
  :config
  (display-time-mode t)
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
	(if (file-directory-p "~/rc/.emacs.d")
	    (expand-file-name "~/rc/.emacs.d/plantuml.jar")
	  (expand-file-name "~/.emacs.d/plantuml.jar")))
  (add-hook 'org-babel-after-execute-hook
	    (lambda ()
	      (when org-inline-image-overlays
		(org-redisplay-inline-images))))
  (add-to-list 'org-structure-template-alist
	       '("u" "#+BEGIN_SRC plantuml :file ?.png\n
skinparam monochrome true\n
#+END_SRC"))

  (setq org-startup-with-inline-images t)
  )

(use-package swiper
  :ensure t
  :config
  (define-key global-map (kbd "C-c s s") 'swiper)
  )

(use-package iedit
  :ensure t
  )

(use-package avy
  :ensure t
  :bind
  ("C-c j c" . avy-goto-char)
  ("C-c j l" . avy-goto-line)
  :config
  (avy-setup-default)
  )

;; undo-tree-mode
(global-undo-tree-mode)

;; shader-mode
(add-to-list 'auto-mode-alist '("\\.shader\\'" . shader-mode))

(use-package magit
  :ensure t
  :init
  (setenv "GIT_ASKPASS" "git-gui--askpass")
  :bind
  ("C-x g" . magit-status)
  )

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings (kbd "C-c g r"))
  )

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
  )

(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-python-command "python3")
  ;; (define-key elpy-mode-map (kbd "C-M-i") 'company-jedi)
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)
    )
  (setq elpy-rpc-virtualenv-path 'system)
  )

(use-package ein
  :ensure t
  )

(use-package pyenv-mode
  :if (executable-find "pyenv")
  )

;; (setq python-shell-interpreter "jupyter")
(setq python-shell-interpreter "python")
(if (equal python-shell-interpreter "jupyter")
    (progn
      (setq python-shell-interpreter-args "console --simple-prompt"
	    python-shell-prompt-detect-failure-warning nil
	    )
      (add-to-list 'python-shell-completion-native-disabled-interpreters "jupyter")
      )
  )

;; python
(add-hook 'python-mode-hook
	  #'(lambda ()
	      (display-line-numbers-mode t)
	      (anaconda-mode t)
	      )
	  )

;; cursor line always highlighted
(when (display-graphic-p)
  (global-hl-line-mode t)
  )

;; when a file is updated outside emacse, make it update if it's already opend in emacs
(global-auto-revert-mode t)

(define-key minibuffer-local-map (kbd "C-c i b n") 'kino/insert-buffer-name)

;; company
(require 'cc-mode)
(use-package company
  :bind
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (company-quickhelp-mode t)
  (setq company-backends (delete 'company-semantic company-backends))
  (define-key c-mode-map (kbd "<tab>") 'company-complete)
  (define-key c++-mode-map (kbd "<tab>") 'company-complete)
  )

(use-package company-jedi
  :after company
  :config
  (add-to-list 'company-backends 'company-jedi)
  )

(use-package company-anaconda
  :after (anaconda-mode company)
  :config
  (add-to-list 'company-backend 'company-anaconda)
  )

(use-package company-irony
  :ensure t
  :after (company irony)
  :config
  (add-to-list 'company-backends 'company-irony)
  )

(defun kino/c-style-mode ()
  (irony-mode t)
  (setq-local c-basic-offset 4)
  (linum-mode t)
  )

(use-package irony
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'kino/c-style-mode)
  (add-hook 'c-mode-hook 'kino/c-style-mode)
  (add-hook 'objc-mode-hook 'kino/c-style-mode)
  (setq-default irony-cdb-compilation-databases
		'(irony-cdb-libclang
		  irony-cdb-clang-complete)
		)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (when (boundp 'w32-pipe-read-delay)
    (setq w32-pipe-read-delay 0)
    )
  (when (boundp 'w32-pipe-buffer-size)
    (setq irony-server-w32-pipe-buffer-size (* 64 1024))
    )
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

;; cs mode
(eval-after-load
    'company
  '(add-to-list 'company-backends #'company-omnisharp)
  )

(use-package omnisharp
  :bind
  (:map omnisharp-mode-map ("C-c r r" . 'omnisharp-run-code-action-refactoring))
  (:map omnisharp-mode-map ("C-c d d" . 'omnisharp-current-type-documentation))
  (:map omnisharp-mode-map ("C-c f u" . 'omnisharp-find-usages))
  (:map omnisharp-mode-map ("C-c m r" . 'omnisharp-rename))
  :config
  (define-key omnisharp-mode-map (kbd "C-c g d c") 'omnisharp-go-to-definition)
  (define-key omnisharp-mode-map (kbd "C-c g d o") 'omnisharp-go-to-definition-other-window)
  (define-key omnisharp-mode-map (kbd "C-c O N s e") 'omnisharp-solution-errors)
  )

(require 'helm)
(require 'helm-config)
(helm-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; bat-mode
(when (eq system-type 'windows-nt)
  (defun kino/call-process-shell-async-current-buffername ()
    "For bat-mode shell-command by current-buffername"
    (interactive)
    (call-process-shell-command (format "start cmd /c %s" (buffer-name))))
  (require 'bat-mode)
  (add-hook 'bat-mode-hook
	    (lambda ()
	      (local-set-key (kbd "<f5>") 'kino/call-process-shell-async-current-buffername))))

;; jupyter
(defun kino/jupyter-notebook ()
  (interactive)
  (if (eq system-type 'windows-nt)
	  (async-shell-command
	   (format "start cmd /k \"cd %s & jupyter notebook\"" (eval default-directory)))
    (if (eq system-type 'darwin)
		(async-shell-command "open jupyter notebook"))))

;; python simple server
(defun kino/open-server-working-dir-http ()
  (interactive)
  (let (shell-cmd)
    (setq shell-cmd "python3 -m http.server")
    (if (eq system-type 'windows-nt)
		(setq shell-cmd (concat "start " shell-cmd))
      (if (eq system-type 'darwin)
		  (setq shell-cmd (concat "open " shell-cmd))))
    (async-shell-command shell-cmd)))

(defun kino/open-server-working-dir-ftp ()
  (interactive)
  (let (shell-cmd)
    (setq shell-cmd "python3 -m pyftpdlib")
    (if (eq system-type 'windows-nt)
		(setq shell-cmd (concat "start " shell-cmd))
      (if (eq system-type 'darwin)
		  (setq shell-cmd (concat "open " shell-cmd))))
    (async-shell-command shell-cmd)))

(defun kino/set-c-common-style ()
  (c-set-style "google")
  (setq tab-width 4)
  (setq c-basic-offset 4))

(add-hook 'c-mode-common-hook 'kino/set-c-common-style)

(defun kino/kill-other-buffers ()
  "kill all other buffers"
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun kino/csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)
  (display-line-numbers-mode)
  )

(add-hook 'csharp-mode-hook 'kino/csharp-mode-setup t)

(defun kino/translate-at()
  "Translate current word using Google Translator"
  (interactive)
  (let (sel-word-target-url)
    (setq word
		  (if (and transient-mark-mode mark-active)
			  (buffer-substring-no-properties (region-beginning) (region-end))
			(thing-at-point 'symbol)))
    (setq word (replace-regexp-in-string " " "%20" word))
	(kino/translate-string word)))

(defun kino/translate-string(string)
  "translate input string"
  (interactive "sstring:")
  (kino/translate-string-google string))

(defun kino/translate-string-google(string)
  (setq target-url (concat "http://translate.google.com/#auto|ko|" string))
  (browse-url target-url))

(defun kino/insert-pair-char (arg char)
  "insert pair character"
  (interactive "P\ncCharacter: ")
  (message "%s" arg)
  (insert-pair arg char char))

(global-set-key (kbd "C-c t g a") 'kino/translate-at)
(global-set-key (kbd "C-c t g s") 'kino/translate-string)

(global-set-key (kbd "C-c i p c") 'kino/insert-pair-char)

(define-abbrev-table 'global-abbrev-table
  '(("kinog" "kino811@gmail.com")
    ("kinon" "kino811@naver.com")
    ("kino8" "kino811")))

;; insert buffer-name at minibuffer
(defun kino/insert-buffer-name ()
  (interactive)
  (insert (buffer-name (window-buffer (minibuffer-selected-window))))
  )

;; open gui file manager
(defun kino/open-file-manager-cwd ()
  (interactive)
  (if (eq system-type 'windows-nt)
      (async-shell-command "explorer .")
    (if (eq system-type 'darwin)
	(async-shell-command "open ."))
    )
  )

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  )

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "C-c o a") 'org-agenda)

(edit-server-start)
(server-start)

(require 'google-translate)
(require 'google-translate-default-ui)
(global-set-key "\C-cta" 'google-translate-at-point)
(global-set-key "\C-ctq" 'google-translate-query-translate)
(global-set-key (kbd "C-c t Q") 'google-translate-query-translate-reverse)

(require 'autopair)
(autopair-global-mode t) ;; to enable in all buffers
