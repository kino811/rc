;;; emacs-custom.el --- Customize settings -*- lexical-binding: t -*-
;; Written by Emacs Customize (custom-file, set in myInit.org).
;; package-selected-packages here is what package-install-selected-packages
;; installs on a new machine.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(0blayout actionscript-mode agent-recall agent-shell back-button bm
			  browse-kill-ring cape centered-cursor-mode copyit
			  corfu-terminal counsel csv-mode default-text-scale deft
			  dired-open eat edit-server emacs-surround eshell-toggle
			  exec-path-from-shell expand-region eyebrowse figlet
			  flycheck glsl-mode gnuplot google-translate helpful
			  highlight-indent-guides iedit ini-mode ivy-hydra
			  ivy-xref json-mode lsp-ivy lsp-treemacs lsp-ui lua-mode
			  magit multiple-cursors newlisp-mode ob-ipython
			  ob-mermaid org-contrib org-download org-mind-map
			  org-sticky-header ox-reveal p4 pandoc-mode plantuml-mode
			  powerline powershell projectile pyvenv
			  quelpa-use-package rainbow-delimiters request rg
			  shift-number shrface solarized-theme spacemacs-theme
			  syntax-subword try undo-tree vertico vterm vundo
			  which-key yasnippet-snippets))
 '(safe-local-variable-values
   '((eval setq-local deft-directory
		   (expand-file-name
			(locate-dominating-file default-directory ".dir-locals.el")))
	 (eval setq-local org-roam-db-location
		   (expand-file-name "org-roam.db" org-roam-directory))
	 (eval setq-local org-roam-directory
		   (expand-file-name
			(locate-dominating-file default-directory ".dir-locals.el"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
