;; This is Kino's .emacs

;; adding a package source
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/")
	     t)

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
 '(current-language-environment "Korean")
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "158013ec40a6e2844dbda340dbabda6e179a53e0aea04a4d383d69c329fba6e6" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d21135150e22e58f8c656ec04530872831baebf5a1c3688030d119c114233c24" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(org-agenda-files (quote ("~/work/emacs_practice.org")))
 '(package-selected-packages
   (quote
    (which-key multi-term wgrep iedit avy swiper prodigy eyebrowse projectile csharp-mode evil-surround evil-magit org-evil airline-themes powerline neotree magit evil solarized-theme jedi helm))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; 
;; Install Packages that not exist package.

;; (defconst kino/packages '(helm
;; 			  jedi
;; 			  solarized-theme
;; 			  evil
;; 			  magit
;; 			  neotree
;; 			  powerline))

(setq kino/need-package-refresh nil)

(dolist (pkg package-selected-packages) ;; (pkg kino/packages)
  (when (not (package-installed-p pkg))
    (setq kino/need-package-refresh t)))

(if kino/need-package-refresh
    (package-refresh-contents))

(dolist (pkg package-selected-packages) ;; (pkg kino/packages)
  (when (not (package-installed-p pkg))
    (package-install pkg)))
;; 


;; hide toolbar and menu
(tool-bar-mode -1)

(setq inhibit-splash-screen t)

;; themes
(add-to-list 'custom-theme-load-path 
	     "~/.emacs.d/elpa/solarized-theme-20170831.1159")
(load-theme 'solarized-dark t)

;; 
;; recentf
;; keep a list of recently opened files
(recentf-mode 1)
;; (global-set-key (kbd "<f7>") 'recentf-open-files)
;;

;; save/restore opend files and windows config
(desktop-save-mode t)
;; (setq desktop-save 'ask-if-new')
;; (setq desktop-aufto-save-timeout 30)
;; starting emacs without opening last session's files
;; $ emacs --no-desktop

;; 
;; alias
(defalias 'list-buffers 'ibuffer)
;; 

(require 'powerline)
(powerline-default-theme)


(require 'evil)
;(evil-mode t)
(global-evil-surround-mode 1)


(show-paren-mode t)


;; 
;; bat-mode
(defun kino/call-process-shell-async-current-buffername ()
  "for bat-mode shell-command by current-buffername"
  (interactive)
  (call-process-shell-command 
   (format "start cmd /c %s" (buffer-name))))

(defun kino/set-bat-mode-hook ()
  "set bat-mode hook"
  (interactive)
  (local-set-key (kbd "<f5>") 'kino/call-process-shell-async-current-buffername))

(require 'bat-mode)
(add-hook 'bat-mode-hook 'kino/set-bat-mode-hook)
;;


;; helm
(require 'helm-config)
(helm-mode 1)


;; windmove
(windmove-default-keybindings 'meta)


;; org-mode
(require 'org)
(require 'ob)

;; make org mode allow eval of some langs
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (C . t)))

(setq org-src-fontify-natively t)


;; 
;; custom key-map
(global-set-key (kbd "C-<kanji>") 'set-mark-command)

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x g") 'magit-status)
;;

