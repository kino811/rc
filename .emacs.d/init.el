;;; package --- kino's init

(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/rc/.emacs.d/lisp")

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  )

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "ae65ccecdcc9eb29ec29172e1bfb6cadbe68108e1c0334f3ae52414097c501d2" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default))
 '(helm-gtags-prefix-key "tg")
 '(helm-gtags-suggested-key-mapping t)
 '(nxml-child-indent 4)
 '(org-agenda-files '("h:/work/ffo4/work/todo.org"))
 '(package-selected-packages
   '(ob-typescript ob-ipython evil-indent-textobject evil-surround evil-leader rainbow-delimiter rainbow-delimiters python-mode smex cmake-mode rtags shell-pop nhexl-mode lsp-mode auto-complete-nxml bm org-attach-screenshot htmlize ox-reveal emacsql-sqlite3 sqlite3 yasnippet-snippets which-key undo-tree spacemacs-theme solarized-theme rg quelpa-use-package pyenv-mode projectile prodigy powerline plantuml-mode p4 ox-confluence-en org-plus-contrib org-download omnisharp ns-auto-titlebar magit lua-mode lsp-ui lsp-ivy key-chord json-mode iy-go-to-char ivy-xref ivy-hydra irony-eldoc ini-mode iedit highlight-indent-guides helpful google-translate google-c-style flycheck-irony eyebrowse expand-region exec-path-from-shell evil eshell-toggle emacs-surround elpy ein edit-server dap-mode counsel company-jedi company-irony company-anaconda command-log-mode ccls browse-kill-ring autopair actionscript-mode)))

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

;; (if (executable-find "chrome")
;;     (progn (browse-url-generic-program "chrome"))
;;   (progn (setq browse-url-browser-function 'eww-browse-url)))

(when enable-multibyte-characters
  (set-language-environment "Korean")
  )

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-locale-environment "ko_KR.UTF-8")

(subword-mode t)

;; hide toolbar and menu
(tool-bar-mode -1)

;; don't show splash-screen
(setq inhibit-splash-screen t)

(global-visual-line-mode t)

;; turn off sound bell and use visible bell
(setq visible-bell t)

;; delete regin before yank
(delete-selection-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)

(setq make-backup-files nil)
(setq-default tab-width 4)

(desktop-save-mode t)

(defun my-find-fie-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only for speed."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (display-line-numbers-mode nil)))

(add-hook 'find-file-hook 'my-find-fie-check-make-large-file-read-only-hook)

;; 
;; Begin use-package
;;

;; (use-package quelpa
;;   :ensure t
;;   :config
;;   (setq quelpa-update-melpa-p nil)
;;   )

(use-package quelpa-use-package
  :ensure t
  :config
  (setq quelpa-update-melpa-p nil)
  )

(use-package which-key
  :ensure t
  :bind (("C-c x m" . 'which-key-show-major-mode)
		 (("C-c x x" . 'which-key-show-top-level)))
  :config
  (which-key-mode t)
  (which-key-add-key-based-replacements
    (kbd "C-c x") "execute"
    (kbd "C-c s") "search,select"
    (kbd "C-c j") "jump"
    (kbd "C-c w") "window"
    (kbd "C-c p") "project"
    (kbd "C-c t") "translate"
    (kbd "C-c i") "insert"
    (kbd "C-c i b") "buffer"
    (kbd "C-c o") "open"
    (kbd "C-c e") "edit"
    (kbd "C-c a") "autocomplete"
    (kbd "C-c &") "yasnippet")
  )

(use-package highlight-indent-guides
  :ensure t
  :hook ((prog-mode) . highlight-indent-guides-mode)
  :config
  )

(use-package paren
  :init
  (show-paren-mode t)
  )

(use-package hl-line
  :init
  (global-hl-line-mode t)
  )

(use-package windmove
  :ensure t
  :config
  (global-set-key (kbd "C-c w h") 'windmove-left)
  (global-set-key (kbd "C-c w l") 'windmove-right)
  (global-set-key (kbd "C-c w k") 'windmove-up)
  (global-set-key (kbd "C-c w j") 'windmove-down)
  )

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("LANG" "LC_ALL" "LDFLAGS" "CPPFLAGS" "CFLAGS"))
    (message "Initialized PATH and other variables from SHELL.")
	)
  )

;; 
;; set theme
(use-package solarized-theme
  :ensure t
  :config
  )

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-dark t)
  :config
  (setq spacemacs-theme-org-agenda-height nil)
  (setq spacemacs-theme-org-height nil)
  )

(if (display-graphic-p)
    (load-theme 'spacemacs-dark)
  )
;;

(use-package ns-auto-titlebar
  :ensure t
  :config
  (when (eq system-type 'darwin) (ns-auto-titlebar-mode))
  )

(use-package ivy
  :ensure t
  :config
  (ivy-mode t)
  )

(use-package ivy-hydra
  :ensure t)

(use-package ivy-xref
  :ensure t
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
  )

(use-package smex
  :ensure t
  :config
  (smex-initialize)
  )

(use-package prodigy
  :ensure t
  :bind ("C-c o p" . prodigy)
  :config
  (prodigy-define-service
    :name "Python app"
    :command "python"
    :args '("-m" "SimpleHTTPServer" "6001")
    :cwd "~/work/python-http-home"
    :tags '(work)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)
  )

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-c s s s") 'swiper)
  (global-set-key (kbd "C-c s s a") 'swiper-thing-at-point)
  )

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-c j c") 'avy-goto-char)
  (global-set-key (kbd "C-c j l") 'avy-goto-line)
  )

(use-package iedit
  :ensure t
  :config
  )

(use-package wgrep
  :ensure t
  :config
  )

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-flake8-maximum-line-length 124)
  )

(use-package magit
  :ensure t
  :bind
  ("C-c p g" . 'magit-status)
  :config
  )

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p p" . projectile-command-map)
  :config
  (projectile-mode t)
  (setq projectile-enable-caching t)
  (setq gc-cons-threshold 1000000000)
  (defun my-minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))
  (defun my-minibuffer-exit-hook ()
    (psetq gc-cons-threshold 800000)
	)
  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
  (when (memq window-system '(w32))
    (setq projectile-indexing-method 'alien)
    )
  )

(use-package recentf
  :ensure t
  :bind ("C-c o r" . 'recentf-open-files)
  :config
  (recentf-mode t)
  )

(use-package powerline
  :ensure t
  :config
  (powerline-default-theme)
  )

(use-package org
  :ensure t
  :ensure org-plus-contrib
  :bind (:map org-mode-map
			  ("C-c C-c" . (lambda ()
							 (interactive)
							 (org-ctrl-c-ctrl-c)
							 (org-display-inline-images)
							 ))
			  )
  :config
  (use-package ob-ipython
	:ensure t
	:config)
  (use-package ob-typescript
	:ensure t
	:config)
  
  (require 'ox-confluence)
  (org-babel-do-load-languages 'org-babel-load-languages
							   '((emacs-lisp . t)
								 (python . t)
								 (ipython . t)
								 (typescript . t)
								 (C . t)
								 (plantuml . t)
								 (shell . t)))
  (setq org-src-fontify-natively t)
  (setq org-todo-keywords
		'((sequencep "TODO" "PROGRESS" "WAITING" "DONE")))
  (setq org-plantuml-jar-path
		(if (file-directory-p "~/rc/.emacs.d")
			(expand-file-name "~/rc/.emacs.d/plantuml.jar")
		  (expand-file-name "~/.emacs.d/plantuml.jar")))
  (add-hook 'org-babel-after-execute-hook
			(lambda ()
			  (when org-inline-image-overlays
				(org-redisplay-inline-images))))
  (add-to-list 'org-structure-template-alist
			   '("u" . "src plantuml :file .png :exports plantuml"))
  (setq org-startup-with-inline-images t)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (global-set-key (kbd "C-c o a") 'org-agenda)
  (when (eq system-type 'darwin)
    (progn (set-face-font 'default "Monaco-12")
		   (set-fontset-font "fontset-default" '(#x1100 . #xffdc)
							 '("AppleGothic" . "unicode-bmp"))
		   (setq face-font-rescale-alist '(("AppleGothic" . 1.2))))
    )
  (when (eq system-type 'windows-nt)
    (progn (set-face-attribute 'default nil :family "Consolas")
		   (set-face-attribute 'default nil :height 100)
		   (set-fontset-font t 'hangul (font-spec :name "NanumBarunGothic"))
		   (setq face-font-rescale-alist '(("NanumBarunGothic" . 1.3)))
		   )
    )
  )

(use-package ox-confluence-en
  :quelpa (ox-confluence-en :fetcher github :repo "correl/ox-confluence-en")
  :config
  (require 'ox-confluence)
  (setq ox-confluence-en-use-plantuml-macro t)
  )

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings (kbd "C-c s r"))
  )

;; language server protocol
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp-deferred)
		 (c++-mode . lsp-deferred)
		 (csharp-mode . lsp-deferred)
		 (lsp-mode . (lambda ()
					   (let ((lsp-keymap-prefix "C-c c l"))
						 (lsp-enable-which-key-integration)))))
  :commands (lsp lsp-deferred)
  :config
  (setq lsp-print-io nil)
  ;; (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  )

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind (([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
		 ([remap xref-find-references] . lsp-ui-peek-find-references)
		 :map lsp-mode-map
		 ("C-S-<space>" . lsp-ui-doc-show)
		 )
  :config
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-sideline-show-diagnostics t)
  (setq lsp-ui-sideline-show-hover t)
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-update-mode 'line)
  (setq lsp-ui-sideline-ignore-duplicate t)

  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-show-directory t)

  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'top)
  (setq lsp-ui-doc-delay 0.2)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-use-childframe t)
  (setq lsp-ui-doc-include-signature t)

  (setq lsp-ui-flycheck-list-position 'right)
  )

;; cc language server
(use-package ccls
  :ensure t
  :config
  (when (eq system-type 'darwin)
    (setq ccls-args (list (concat "--init={"
								  (concat "\"clang\":{"
										  (concat "\"extraArgs\":["
												  ;; "\"/usr/local/opt/llvm/bin/clang++\","
												  ;; "\"-isystem/Library/Developer/CommandLineTools/usr/include/c++/v1\","
												  ;; "\"-isystem/usr/local/opt/llvm/include/c++/v1\","
												  ;; "\"-isystem/usr/local/opt/icu4c/include\","
												  ;; "\"-I.\","
												  "\"-std=c++17\""
												  "]")
										  ;; ","
										  ;; (concat "\"resourceDir\":"
										  ;; 	"\"/usr/local/Cellar/llvm/10.0.1/lib/clang/10.0.1\"")
										  "}")
								  "}")))
    )
  )

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :ensure t
  )

(use-package expand-region
  :ensure t
  :bind (("C-@" . er/expand-region))
  )

(use-package undo-tree
  :ensure t
  :bind (("C-x u" . undo-tree-visualize))
  :config
  (global-undo-tree-mode t)
  )

(use-package counsel
  :ensure t
  :config
  (counsel-mode t)
  )

(use-package actionscript-mode
  :quelpa ((actionscript-mode
			:fetcher github
			:repo "austinhaas/actionscript-mode")
		   )
  :config
  (add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode))
  )

(use-package display-line-numbers
  :ensure t
  :bind (("C-c s l" . display-line-numbers-mode))
  :hook ((prog-mode actionscript-mode) . display-line-numbers-mode)
  :config
  ;; (setq display-line-numbers-type 'relative)
  (global-display-line-numbers-mode t)
  )

(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c w e")) ;window layout
  :config
  (eyebrowse-mode t)

  ;; example title: EmacsConfig [1/4] | configuration.org
  (defun my-title-bar-format()
    (let* ((current-slot (eyebrowse--get 'current-slot))
           (window-configs (eyebrowse--get 'window-configs))
           (window-config (assoc current-slot window-configs))
           (window-config-name (nth 2 window-config))
           (num-slots (length window-configs)))
      (concat window-config-name " [" (number-to-string current-slot)
              "/" (number-to-string num-slots) "] | " "%b")))
  (if (display-graphic-p)
      (progn
        (setq frame-title-format
              '(:eval (my-title-bar-format)))))
  )

(use-package winner
  :ensure t
  :bind (("C-c w w u" . winner-undo)
		 ("C-c w w r" . winner-redo))
  :config
  (winner-mode t)
  )

(use-package json-mode
  :ensure t
  )

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
		 ("C-h F" . helpful-command)
		 ("C-h v" . helpful-variable)
		 ("C-h k" . helpful-key))
  )

(use-package emacs-surround
  :quelpa ((emacs-surround
			:fetcher github
			:repo "ganmacs/emacs-surround")
		   )
  :config
  (global-set-key (kbd "C-c e e") 'emacs-surround)
  (add-to-list 'emacs-surround-alist '("~" . ("~" . "~")))
  (add-to-list 'emacs-surround-alist '("=" . ("=" . "=")))
  (add-to-list 'emacs-surround-alist '("`" . ("`" . "`")))
  (add-to-list 'emacs-surround-alist '("<" . ("<" . ">")))
  (add-to-list 'emacs-surround-alist '("(" . ("(" . ")")))
  (add-to-list 'emacs-surround-alist '("{" . ("{" . "}")))
  )

(use-package p4
  :ensure t
  :config
  (p4-update-global-key-prefix 'p4-global-key-prefix (kbd "C-c p P"))
  )

(use-package treemacs
  :ensure t
  :bind (("C-c o t" . 'treemacs))
  :config
  )

(use-package evil
  :ensure t
  :config
  ;; (evil-mode t)
  (use-package evil-leader
	:ensure t
	:config
	(global-evil-leader-mode)
	)
  (use-package evil-surround
	:ensure t
	:config
	(global-evil-surround-mode)
	)
  (use-package evil-indent-textobject
	:ensure t
	)
  
  (dolist (mode '(ag-mode
				  flycheck-error-list-mode
				  git-rebase-mode))
	(add-to-list 'evil-emacs-state-modes mode))
  
  (add-hook 'occur-mode-hook
			(lambda ()
			  (evil-add-hjkl-bindings occur-mode-map 'emacs
				(kbd "/") 'evil-search-forward
				(kbd "n") 'evil-search-next
				(kbd "N") 'evil-search-previous
				(kbd "C-d") 'evil-scroll-down
				(kdb "C-u") 'evil-scroll-up
				(kdb "C-w C-w") 'other-window)))
  )

(use-package lua-mode
  :ensure t
  )

;; window 10 insert screenshot
(defun my-org-screenshot-w32 ()
  "Take a screenshot into a time stamped unique-named file in the same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  ;; using just clip board captured before.
  ;; (shell-command "snippingtool /clip")
  (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
  (insert (concat "[[file:" filename "]]"))
  (org-display-inline-images)
  )

(use-package org-download
  :ensure t
  :hook (dired-mode . org-download-enable)
  :config
  (cond
   ((eq system-type 'windows-nt)
    (progn
      (setq org-download-screenshot-method "imagemagick/convert")
      (global-set-key (kbd "C-c i s") 'my-org-screenshot-w32)))
   ((eq system-type 'darwin)
    (progn
      (setq org-download-screenshot-method "screencapture"))))
  )

(require 'google-translate)
(use-package google-translate
  :ensure t
  :bind (("C-c t g a" . 'google-translate-at-point)
		 ("C-c t g q" . 'google-translate-query-translate)
		 ("C-c t g t" . 'google-translate-smooth-translate))
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
  (setq google-translate-backend-method 'curl)
  )

(use-package eshell-toggle
  :ensure t
  :bind (("C-c o s e" . eshell-toggle))
  :config
  )

(defun current-buffer-name()
  "get current buffer name."
  (interactive)
  (insert (buffer-name (window-buffer (minibuffer-selected-window))))
  )

(defun current-buffer-file-name()
  "get current buffer name."
  (interactive)
  (insert (buffer-file-name (window-buffer (minibuffer-selected-window))))
  )

(global-set-key (kbd "C-c i b n") 'current-buffer-name)
(global-set-key (kbd "C-c i b p") 'current-buffer-file-name)

(put 'upcase-region 'disabled nil)

(defun my-c++-mode-hook ()
  (setq c-basic-offset 4)
  )

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

(use-package browse-kill-ring
  :ensure t
  :bind (("C-c o k" . browse-kill-ring))
  :config
  )

(use-package edit-server
  :ensure t
  :config
  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . markdown-mode)))
  (edit-server-start)
  )

(use-package ini-mode
  :ensure t
  )

(use-package command-log-mode
  :ensure t
  :custom
  (command-log-mode-key-binding-open-log "C-c o c l")
  )

(use-package plantuml-mode
  :ensure t
  :bind (:map plantuml-mode-map
			  ("C-M-i" . plantuml-complete-symbol))
  :config
  (setq plantuml-jar-path 
		(if (file-directory-p "~/rc/.emacs.d")
			(expand-file-name "~/rc/.emacs.d/plantuml.jar")
		  (expand-file-name "~/.emacs.d/plantuml.jar")))
  (setq plantuml-default-exec-mode 'jar)
  (setq plantuml-indent-level 4)
  )

;; git clone https://github.com/hakimel/reveal.js ~/reveal.js
(when (and (not (f-directory? "~/reveal.js"))
		   (executable-find "git"))
  (shell-command-to-string "cd ~ && git clone https://github.com/hakimel/reveal.js reveal.js")
  )

(use-package ox-reveal
  :ensure t
  :config
  (cond
   ((eq system-type 'windows-nt)
	(progn
	  (setq org-reveal-root "file:///c:/Users/myjung/reveal.js")))
   ((eq system-type 'darwin)
	(progn
	  (setq org-reveal-root "file:///Users/kino811/reveal.js"))))
  )

(use-package htmlize
  :ensure t
  )

(use-package bm
  :ensure t
  :bind(("C-c k k t" . bm-toggle)
		("C-c k k n" . bm-next)
		("C-c k k p" . bm-previous)
		("C-c k k s a" . bm-show-all)
		("C-c k k b s s" . bm-show))
  )

(use-package python
  :ensure t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  )

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode org-mode) . rainbow-delimiters-mode)
  )

;; https://www.voidtools.com/ko-kr/downloads/
(when (eq system-type 'windows-nt)
  (setq everything-cmd "C:\\Program Files (x86)\\Everything\\es.exe")
  (setq everything-ffap-integration nil)
  (global-set-key (kbd "C-c s e") 'everything)
  (require 'everything)
  )

(server-start)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)

(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun kill-other-buffers-test ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
		(delq (current-buffer)
			  (buffer-list))))

