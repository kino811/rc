;; This is Kino's .emacs

;; adding a package source
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/")
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
			  markdown-mode+))
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
(add-to-list 'custom-theme-load-path 
	     "~/.emacs.d/themes/emacs-color-theme-solarized")

;; set solarized theme color
(set-frame-parameter nil 'background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(setq solarized-termcolors 256)

(load-theme 'solarized t)

;; 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (jedi helm icicles markdown-mode markdown-mode+))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; set icicles
(require 'icicles)
(icy-mode 1)
