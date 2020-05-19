;;; package --- kino's init

(add-to-list 'load-path "~/.emacs.d/lisp")

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "ae65ccecdcc9eb29ec29172e1bfb6cadbe68108e1c0334f3ae52414097c501d2" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default)))
 '(google-translate-default-source-language "auto")
 '(google-translate-default-target-language "ko")
 '(helm-gtags-prefix-key "tg")
 '(helm-gtags-suggested-key-mapping t)
 '(org-agenda-files (quote ("~/work/todo/todo.org")))
 '(package-selected-packages
   (quote
    (emacs-surround true highlight-indent-guides material-theme spacemacs-theme helpful ns-auto-titlebar json-mode actionscript-mode quelpa-use-package helm counsel company-lsp ivy-xref org-plus-contrib google-translate multi-term lsp-ivy undo-tree-mode shader-mode markdown-mode+ edit-indirect flycheck-iron swiper powerline key-chord expand-region iy-go-to-char ccls dap-mode treemacs lsp-treemacs lsp-ui lsp-mode ggtags autopair python-black jedi powershell markdown-mode yasnippet-snippets yaml-mode wsd-mode which-key wgrep-ag w3 use-package-chords solarized-theme rtags rg python-docstring pyenv-mode projectile-ripgrep prodigy plantuml-mode org-projectile omnisharp narrowed-page-navigation narrow-reindent multishell mark-multiple magit jupyter irony-eldoc iedit ibuffer-projectile google-c-style flymake-yaml flymake-shell flymake-lua flymake-json flycheck-pycheckers flycheck-irony eyebrowse exec-path-from-shell evil elpy el-get ein edit-server dotnet company-shell company-lua company-jedi company-irony-c-headers company-irony company-glsl company-anaconda command-log-mode cmake-mode cmake-ide blacken avy airline-themes ace-jump-mode))))
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
  (set-language-environment "Korean"))

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
(unbind-key "S-SPC")

;; turn off sound bell and use visible bell
(setq visible-bell t)

;; delete regin before yank
(delete-selection-mode t)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'list-buffers 'ibuffer)

(desktop-save-mode t)

;; 
;; Begin use-package
;;
(use-package quelpa
  :ensure t
  :config
  )

(use-package quelpa-use-package
  :ensure t
  :config
  )

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package paren
  :init
  (show-paren-mode t))

(use-package hl-line
  :init
  (global-hl-line-mode t))

(use-package windmove
  :ensure t
  :config
  (global-set-key (kbd "C-c w h") 'windmove-left)
  (global-set-key (kbd "C-c w l") 'windmove-right)
  (global-set-key (kbd "C-c w k") 'windmove-up)
  (global-set-key (kbd "C-c w j") 'windmove-down)
  ;; (windmove-default-keybindings 'shift)
  )

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("LANG" "LC_ALL" "LDFLAGS" "CPPFLAGS" "CFLAGS"))
    (message "Initialized PATH and other variables from SHELL.")
    )  
  )

;; 
;; set theme
(use-package solarized-theme
  :ensure t
  :config)

(use-package moe-theme
  :ensure t
  :config
  (if (not (display-graphic-p))
      (load-theme 'moe-dark)))

(use-package spacemacs-theme
  :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-dark t)
  :config
  (setq spacemacs-theme-org-agenda-height nil)
  (setq spacemacs-theme-org-height nil))

(if (display-graphic-p)
    (load-theme 'spacemacs-dark))
;;

(use-package ns-auto-titlebar
  :ensure t
  :config
  (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

(use-package ivy
  :ensure t
  :config
  (ivy-mode t))

(use-package ivy-xref
  :ensure t
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package prodigy
  :ensure t
  :bind ("C-c v p" . prodigy)
  :config
  (prodigy-define-service
    :name "Python app"
    :command "python"
    :args '("-m" "SimpleHTTPServer" "6001")
    :cwd "~/work/python-http-home"
    :tags '(work)
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t
    )
  )

(use-package swiper
  :ensure t
  :config
  (global-set-key (kbd "C-c s s") 'swiper)
  (global-set-key (kbd "C-c s a") 'swiper-all)
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

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind ("C-c c c" . company-complete)
  :config
  (setq company-minimum-prefix-length 1
	company-idle-delay 0.0))

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))

(use-package flycheck
  :ensure t
  :config
  )

(use-package multi-term
  :ensure t
  :config
  (setq mulcti-term-program "/bin/zsh")
  (global-set-key (kbd "C-c t m") 'multi-term)
  )

(use-package which-key
  :ensure t
  :config
  (which-key-mode t)
  )

(use-package magit
  :ensure t
  :bind
  ("C-x g" . 'magit-status)
  :config
  )

(use-package projectile
  :ensure t
  :config
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  
  (setq projectile-enable-caching t)
  (setq gc-cons-threshold 1000000000)
  (defun my-minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))
  (defun my-minibuffer-exit-hook ()
    (setq gc-cons-threshold 800000))
  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
  (when (memq window-system '(w32))
    (setq projectile-indexing-method 'alien)
    )
  )

(use-package recentf
  :ensure t
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
  :bind
  (:map org-mode-map
	("C-c C-c" . (lambda ()
		       (interactive)
		       (org-ctrl-c-ctrl-c)
		       (org-display-inline-images))))
  :config
  (require 'ox-confluence)
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 (python . t)
				 (C . t)
				 (plantuml . t)))
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
	       '("u" . "src plantuml :file .png"))
  (setq org-startup-with-inline-images t)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (global-set-key (kbd "C-c o a") 'org-agenda))

(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings (kbd "C-c g r")))

;; language server protocol
(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((prog-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; cc language server
(use-package ccls
  :ensure t
  :config
  (setq ccls-executable "/usr/local/bin/ccls"))

(use-package yasnippet
  :ensure t
  :init
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'org-mode-hook #'yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t)

(use-package expand-region
  :ensure t
  :bind (("C-c m e" . er/expand-region)))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode t)
  )

(use-package google-translate
  :ensure t
  :config
  (require 'google-translate-default-ui)
  (global-set-key (kbd "C-c d a") 'google-translate-at-point)
  (global-set-key (kbd "C-c d q") 'google-translate-query-translate))

(use-package counsel
  :ensure t
  :config
  (counsel-mode t))

(use-package actionscript-mode
  :quelpa ((actionscript-mode
	    :fetcher github
	    :repo "austinhaas/actionscript-mode")
	   :upgrade t)
  :config
  (add-to-list 'auto-mode-alist '("\\.as\\'" . actionscript-mode)))

(use-package display-line-numbers
  :ensure t
  :hook ((prog-mode . display-line-numbers-mode)
	 (actionscript-mode . display-line-numbers-mode)))

(use-package eyebrowse
  :ensure t
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
  :config
  (winner-mode t))

(use-package json-mode
  :ensure t)

(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
	 ("C-h F" . helpful-command)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)))

(use-package emacs-surround
  :quelpa ((emacs-surround
	    :fetcher github
	    :repo "ganmacs/emacs-surround")
	   :upgrade t)
  :config
  (global-set-key (kbd "C-q") 'emacs-surround)
  (add-to-list 'emacs-surround-alist '("~" . ("~" . "~")))
  (add-to-list 'emacs-surround-alist '("=" . ("=" . "=")))
  (add-to-list 'emacs-surround-alist '("`" . ("`" . "`"))))

(defun current-buffer-name()
  "get current buffer name"
  (interactive)
  (insert (buffer-name (window-buffer (minibuffer-selected-window)))))

(defun current-buffer-file-name()
  "get current buffer name"
  (interactive)
  (insert (buffer-file-name (window-buffer (minibuffer-selected-window)))))

(global-set-key (kbd "C-c i b n") 'current-buffer-name)
(global-set-key (kbd "C-c i b p") 'current-buffer-file-name)

(put 'upcase-region 'disabled nil)
