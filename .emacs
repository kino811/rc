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

(defconst kino/packages '(helm
			  icicles
			  jedi
			  markdown-mode
			  markdown-mode+
			  solarized-theme))
(dolist (pkg kino/packages)
  (when (not (package-installed-p pkg)))
  (package-install pkg))

;; 
;; hide toolbar and menu
(menu-bar-mode -1)
(tool-bar-mode -1)
;; 

;; 
;; themes
;; 
(add-to-list 'custom-theme-load-path 
	     "~/.emacs.d/elpa/solarized-theme-20170831.1159")
(load-theme 'solarized-dark t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(current-language-environment "UTF-8")
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(package-selected-packages
   (quote
    (company-jedi jedi helm icicles markdown-mode markdown-mode+))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; set icicles
(require 'icicles)
(icy-mode 1)



;; custom key-map
(global-set-key [C-kanji] 'set-mark-command)
