;;; quick-init.el --- Minimal fast config for terminal Emacs on Windows -*- lexical-binding: t -*-

;; Loaded by windows/bin/emacs-q as `emacs -nw -Q -l quick-init.el'.
;; A quick editor (like nvim) that starts without package.el or the full
;; myInit.org.  A few packages already installed by the full config are
;; loaded straight from ~/.emacs.d/elpa (each is skipped when missing).
;; It does not start a server or write shared state files (recentf,
;; savehist), so it never conflicts with the full Emacs.

;;; Code:

(defconst my/quick-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory that holds quick-init.el (the .emacs.d folder of the rc repo).")

(defun my/quick-use-elpa (name)
  "Put the newest installed elpa package NAME on `load-path'.
Also load its autoloads file and add it to `custom-theme-load-path'.
Return the package directory, or nil when NAME is not installed.
This is much faster than `package-initialize', which activates every
installed package."
  (let ((dir (car (last (sort (file-expand-wildcards
                               (expand-file-name
                                (concat name "-[0-9]*")
                                (expand-file-name "elpa" user-emacs-directory)))
                              #'string<)))))
    (when (and dir (file-directory-p dir))
      (add-to-list 'load-path dir)
      (add-to-list 'custom-theme-load-path dir)
      (load (expand-file-name (concat name "-autoloads") dir) t t)
      dir)))

;;;; Customization file

;; `emacs -Q' has no init file, so answers saved through Customize (for
;; example `!' on the "local variables may not be safe" prompt, which adds
;; to `safe-local-variable-values') were lost on exit and asked again next
;; time.  Keep them in a file of its own, apart from the full config's
;; ~/.emacs.d/init.el.  It lives in the home folder, not in the rc repo,
;; because it holds machine-specific values.
;;
;; `custom-file' (the function) refuses to save while `user-init-file' is
;; nil, which is the case under -Q, even when the variable `custom-file' is
;; set.  This file is the init file of emacs-q, so say so; saves still go to
;; `custom-file', never to this file.
(setq user-init-file (or load-file-name buffer-file-name))
(setq custom-file (expand-file-name "emacs-q-custom.el" user-emacs-directory))
(load custom-file t t)

;;;; Language

;; Korean locale also sets `default-input-method' to korean-hangul
;; (toggle with C-\).
(when enable-multibyte-characters
  (set-language-environment "UTF-8")
  (set-locale-environment "ko_KR.UTF-8"))
(prefer-coding-system 'utf-8)

;; Windows console support (UTF-8 console, WezTerm carrier keys, C-z,
;; input freeze guard).  Must come after `set-locale-environment'.
;; lisp/ is put on `load-path' only later, so give `require' the file.
(let ((w32-tty (expand-file-name "lisp/w32-tty" my/quick-directory)))
  (if (require 'w32-tty w32-tty t)
      (my/w32-tty-setup)
    (message "w32-tty.el not found: %s" w32-tty)))

;;;; Look

(when (my/quick-use-elpa "spacemacs-theme")
  (load-theme 'spacemacs-dark t))
(setq inhibit-startup-screen t
      ring-bell-function #'ignore)
(menu-bar-mode -1)
(column-number-mode 1)
(global-visual-line-mode 1)
(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
;; A 16-color terminal maps the default hl-line color to bright green,
;; which hides the text on the current line.  Use black there.  (Not
;; `custom-set-faces', which would end up saved in `custom-file'.)
(when (and (not (display-graphic-p)) (< (display-color-cells) 256))
  (set-face-attribute 'hl-line nil :background "black"))

;; Highlight trailing whitespace in code buffers.
(define-minor-mode my:trailing-whitespace-mode
  "Simple minor mode to highlight trailing white spaces."
  :variable show-trailing-whitespace)
(add-hook 'prog-mode-hook #'my:trailing-whitespace-mode)

;;;; Editing

(setq make-backup-files nil)
(setq-default tab-width 4)
(setopt use-short-answers t)
(global-auto-revert-mode 1)
(delete-selection-mode 1)
(winner-mode 1)
(which-key-mode 1)
(defalias 'list-buffers 'ibuffer)
(global-set-key (kbd "C-c r") #'revert-buffer)

;; Files with very long lines, and files over 1 MiB (read only, no undo,
;; fundamental-mode, no line numbers), stay responsive.
(global-so-long-mode 1)
(require 'large-file-setup (expand-file-name "lisp/large-file-setup" my/quick-directory) t)

;;;; Editing packages (same keys as the full config; load on first use)

(when (my/quick-use-elpa "expand-region")
  (if (display-graphic-p)
      (global-set-key (kbd "C-@") #'er/expand-region)
    ;; Windows console: wezterm.lua sends Ctrl+Shift+2 as C-S-<f6>.
    (when (eq system-type 'windows-nt)
      (global-set-key (kbd "<C-S-f6>") #'er/expand-region))))

(when (my/quick-use-elpa "avy")
  (global-set-key (kbd "C-c j j") #'avy-goto-char)
  (global-set-key (kbd "C-c j l") #'avy-goto-line))

(when (my/quick-use-elpa "multiple-cursors")
  (global-set-key (kbd "C-c e m c") #'mc/edit-lines))

(when (my/quick-use-elpa "browse-kill-ring")
  (global-set-key (kbd "C-c o k") #'browse-kill-ring))

;; The full config loads iedit at startup, which binds C-;.  Bind it here
;; instead so iedit loads only when used.
(when (my/quick-use-elpa "iedit")
  (global-set-key (kbd "C-;") #'iedit-mode))

(my/quick-use-elpa "vundo")             ; M-x vundo

(when (my/quick-use-elpa "rainbow-delimiters")
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode))

;;;; Completion and search (same keys as the full config)

;; ivy lets SPC insert a space, so C-x b can create a buffer with any
;; name (C-M-j uses the typed text as is).  Without ivy, fall back to the
;; built-in fido (there, M-j uses the typed text as is).
(if (my/quick-use-elpa "ivy")
    (progn
      (setq ivy-display-style 'fancy)
      (ivy-mode 1)
      (global-set-key (kbd "C-c C-r") #'ivy-resume))
  (fido-vertical-mode 1))

(when (my/quick-use-elpa "swiper")
  (global-set-key (kbd "C-c s s s") #'swiper)
  (global-set-key (kbd "C-c s s a") #'swiper-thing-at-point)
  (global-set-key (kbd "C-c s s e s") #'swiper-all)
  (global-set-key (kbd "C-c s s e a") #'swiper-all-thing-at-point))

;;;; Org display (org itself loads only when an .org file is opened)

(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook #'org-indent-mode)
(when (my/quick-use-elpa "org-sticky-header")
  (add-hook 'org-mode-hook #'org-sticky-header-mode))

;;;; Markdown (loads only when an .md file is opened)

(when (my/quick-use-elpa "markdown-mode")
  ;; Same keys as the full config: subtree editing on the org arrow keys,
  ;; C-c <arrow> left free.
  (with-eval-after-load 'markdown-mode
    (define-key markdown-mode-map (kbd "M-<left>") #'markdown-promote)
    (define-key markdown-mode-map (kbd "M-<right>") #'markdown-demote)
    (define-key markdown-mode-map (kbd "M-<up>") #'markdown-move-up)
    (define-key markdown-mode-map (kbd "M-<down>") #'markdown-move-down)
    (define-key markdown-mode-map (kbd "C-c <left>") nil)
    (define-key markdown-mode-map (kbd "C-c <right>") nil)
    (define-key markdown-mode-map (kbd "C-c <up>") nil)
    (define-key markdown-mode-map (kbd "C-c <down>") nil)))

;;;; Google Translate (C-c t t t / a / q; loads on first use)

(when (and (my/quick-use-elpa "popup")
           (my/quick-use-elpa "google-translate"))
  (require 'google-translate-setup
           (expand-file-name "lisp/google-translate-setup" my/quick-directory) t))

;;; quick-init.el ends here
