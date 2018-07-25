;; This is Kino's .emacs


;; add load-path
(add-to-list 'load-path "~/.emacs.d/site-lisp")


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
 '(org-agenda-files (quote ("~/work/kaiser/todo.org")))
 '(package-selected-packages
   (quote
    (python-docstring company-glsl flymake-yaml yaml-mode flycheck-pycheckers flymake-json flymake-lua flymake-shell flycheck company-jedi company-lua company-shell company wgrep-ag wgrep-helm projectile-ripgrep swiper-helm ripgrep rg helm-rg ibuffer-projectile org-projectile helm-projectile yasnippet-snippets yasnippet mark-multiple ace-jump-mode autopair edit-server which-key multi-term wgrep iedit avy swiper prodigy eyebrowse projectile csharp-mode airline-themes powerline magit evil solarized-theme helm))))

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
(global-set-key (kbd "C-c r f") 'recentf-open-files)
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
;; (powerline-center-evil-theme)
(display-time-mode t)


(define-key global-map (kbd "RET") 'newline-and-indent)


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


(require 'edit-server)
(edit-server-start)


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


;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)


(which-key-mode)


;; swiper
(global-set-key (kbd "C-s") 'swiper)


;; avy
(global-set-key (kbd "C-:") 'avy-goto-char)


;; iedit-mode :: multi line edit
(require 'iedit)


;; wgrep
(require 'wgrep)


;; multi term
(require 'multi-term)
;; todo :: not work now.
;; (if (eq system-type 'windows-nt)
;;     (setq multi-term-program "c:/cygwin64/bin/bash.exe")
;;   )
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; undo-tree-mode
(global-undo-tree-mode)


;; shader-mode
(add-to-list 'auto-mode-alist '("\\.shader\\'" . shader-mode))


;; line number mode
(global-linum-mode)


;; magit
(setenv "GIT_ASKPASS" "git-gui--askpass")


;; jedi
(require 'company)
(defun my/python-mode-hook ()
  (with-eval-after-load 'company (add-to-list 'company-backends 'company-jedi))
  )

(add-hook 'python-mode-hook 'my/python-mode-hook)


;; rg
(rg-enable-default-bindings)


;; python simple server
(defun kino/open-server-working-dir-http ()
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

(defun kino/open-server-working-dir-ftp ()
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


;; 
;; custom key-map
(global-set-key (kbd "C-<kanji>") 'set-mark-command)
(global-set-key (kbd "C-x C-<kanji>") 'pop-global-mark)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x g") 'magit-status)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;;
(put 'upcase-region 'disabled nil)
