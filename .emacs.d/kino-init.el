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

(global-set-key (kbd "C-c t a") 'kino/translate-at)
(global-set-key (kbd "C-c t s") 'kino/translate-string)
(global-set-key (kbd "C-c i p c") 'kino/insert-pair-char)

(define-abbrev-table 'global-abbrev-table
  '(("kinog" "kino811@gmail.com")
    ("kinon" "kino811@naver.com")
    ("kino8" "kino811")))

