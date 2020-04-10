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
    ("2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "ae65ccecdcc9eb29ec29172e1bfb6cadbe68108e1c0334f3ae52414097c501d2" "26d49386a2036df7ccbe802a06a759031e4455f07bda559dcf221f53e8850e69" "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f" default)))
 '(google-translate-default-source-language "auto")
 '(google-translate-default-target-language "ko")
 '(helm-gtags-prefix-key "tg")
 '(helm-gtags-suggested-key-mapping t)
 '(package-selected-packages
   (quote
    (lsp-ivy undo-tree-mode shader-mode markdown-mode+ edit-indirect flycheck-iron swiper powerline key-chord expand-region iy-go-to-char ccls dap-mode treemacs lsp-treemacs company-lsp lsp-ui lsp-mode ggtags autopair python-black jedi google-translate powershell markdown-mode yasnippet-snippets yaml-mode wsd-mode which-key wgrep-ag w3 use-package-chords solarized-theme rtags rg python-docstring pyenv-mode projectile-ripgrep prodigy plantuml-mode org-projectile omnisharp narrowed-page-navigation narrow-reindent multishell mark-multiple magit jupyter irony-eldoc iedit ibuffer-projectile google-c-style flymake-yaml flymake-shell flymake-lua flymake-json flycheck-pycheckers flycheck-irony eyebrowse exec-path-from-shell evil elpy el-get ein edit-server dotnet company-shell company-lua company-jedi company-irony-c-headers company-irony company-glsl company-anaconda command-log-mode cmake-mode cmake-ide blacken avy airline-themes ace-jump-mode))))
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
(prefer-coding-system 'utf-8)
(set-locale-environment "ko_KR.UTF-8")

(subword-mode t)

;; hide toolbar and menu
(tool-bar-mode -1)

;; don't show splash-screen
(setq inhibit-splash-screen t)

(global-visual-line-mode t)
(unbind-key "S-SPC")
(defalias 'yes-or-no-p 'y-or-n-p)


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
  :ensure t)

(use-package moe-theme
  :ensure t
  :config
  (if (display-graphic-p)
      (load-theme 'solarized-dark)
    (load-theme 'moe-dark)
    )
  )

(use-package ivy
  :ensure t
  :config
  (ivy-mode t)
  )

(use-package prodigy
  :ensure t
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
  (global-set-key (kbd "C-c f c") 'avy-goto-char)
  (global-set-key (kbd "C-c f l") 'avy-goto-line)
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
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package flycheck
  :ensure t
  :config
  )

(use-package multi-term
  :ensure t
  :config
  )

(use-package which-key
  :ensure t
  :config
  (which-key-mode t)
  )

(use-package magit
  :ensure t
  :config
  )

(global-set-key (kbd "C-x C-b") 'ibuffer)
