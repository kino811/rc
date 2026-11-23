;;; w32-tty.el --- Windows console Emacs (emacs -nw) support -*- lexical-binding: t -*-

;; Shared by myInit.org (full config) and quick-init.el (emacs-q).
;; Load this file, then call `my/w32-tty-setup'.  Call it after
;; `set-locale-environment', which resets the terminal coding system
;; to cp1252.  Everything here does nothing in GUI Emacs and on
;; macOS/Linux.

;;; Code:

(defconst my/w32-tty-directory
  ;; `file-truename': loaded through the ~/.emacs.d/lisp link, the path
  ;; must still resolve to the rc repo so ../../windows is found.
  (file-name-directory (file-truename (or load-file-name buffer-file-name)))
  "Directory that holds w32-tty.el (.emacs.d/lisp of the rc repo).")

(defun my/w32-tty-p ()
  "Return non-nil in a Windows console Emacs (emacs -nw)."
  (and (eq system-type 'windows-nt) (not (display-graphic-p))))

;; The console code page is 437, so Hangul and powerline glyphs are
;; shown as \uXXXX.  `chcp 65001' does not change the output code page
;; that Emacs reads, so switch the console to UTF-8 here.
(defun my/w32-tty-use-utf-8 ()
  "Switch a Windows console Emacs (emacs -nw) to UTF-8 input and output."
  (interactive)
  (when (and (my/w32-tty-p)
             (fboundp 'w32-set-console-output-codepage))
    (w32-set-console-codepage 65001)
    (w32-set-console-output-codepage 65001)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)))

;; The console (ConPTY) drops or changes the modifiers of some keys:
;; Ctrl+Enter and Ctrl+J both arrive as a plain RET, Ctrl+H arrives as
;; <backspace> (so the help prefix was unreachable), Shift+Enter arrives
;; as M-<return> (the same as Alt+Enter), and Ctrl+Shift+Backspace
;; arrives as C-\ (the same as the real Ctrl+\ key, i.e.
;; toggle-input-method).  Ctrl+Space and Ctrl+Shift+2 (C-@) lose their
;; Ctrl and are inserted as plain text.  Modified function keys do
;; survive, so wezterm.lua sends those instead and they are translated
;; back here.
;;
;;   C-S-<f9>  -> C-<return>   (Ctrl+Enter; Org: C-RET inserts a heading)
;;   C-S-<f10> -> C-j          (Ctrl+J; e.g. enters a directory in the minibuffer)
;;   C-S-<f11> -> C-h          (Ctrl+H; the help prefix; Backspace is unchanged)
;;   C-S-<f12> -> S-<return>   (Shift+Enter; Org: org-table-copy-down in a table)
;;   C-S-<f8>  -> C-S-<backspace>  (Ctrl+Shift+Backspace; kill-whole-line)
;;   C-S-<f7>  -> C-SPC        (Ctrl+Space; set-mark-command)
;;   C-S-<f6>  -> er/expand-region, bound in the expand-region section of
;;                myInit.org (Ctrl+Shift+2)
(defun my/w32-tty-translate-carrier-keys ()
  "Translate the WezTerm carrier keys back to the keys they stand for."
  (when (my/w32-tty-p)
    (define-key function-key-map (kbd "<C-S-f9>") (kbd "<C-return>"))
    (define-key function-key-map (kbd "<C-S-f10>") (kbd "C-j"))
    (define-key function-key-map (kbd "<C-S-f11>") (kbd "C-h"))
    (define-key function-key-map (kbd "<C-S-f12>") (kbd "<S-return>"))
    (define-key function-key-map (kbd "<C-S-f8>") (kbd "<C-S-backspace>"))
    (define-key function-key-map (kbd "<C-S-f7>") (kbd "C-SPC"))))

;; Windows terminal Emacs in a WezTerm + psmux pane sometimes stops
;; taking keys because the console input mode gets the VT input flag
;; (0x200).  emacs-input-guard.ps1 clears the flag for this Emacs only,
;; logs diagnostics to ~/.emacs-input-guard.log, and exits with Emacs,
;; so no logon auto-start entry is needed.
(defun my/w32-tty-start-input-guard ()
  "Start emacs-input-guard.ps1 for this Emacs process.
Windows terminal Emacs only.  Does nothing when the script or
powershell.exe is missing, or when the guard is already running."
  (interactive)
  (let ((script (expand-file-name "../../windows/emacs-input-guard.ps1"
                                  my/w32-tty-directory))
        (powershell (executable-find "powershell")))
    (cond
     ((not (my/w32-tty-p))
      (message "Input guard is only for Windows terminal Emacs"))
     ((not (and powershell (file-exists-p script)))
      (message "Input guard not started: %s or powershell.exe not found" script))
     ((process-live-p (get-process "emacs-input-guard"))
      (message "Input guard is already running"))
     (t
      (make-process
       :name "emacs-input-guard"
       :buffer nil
       :connection-type 'pipe
       :noquery t
       :command (list powershell "-NoProfile" "-NonInteractive"
                      "-ExecutionPolicy" "Bypass" "-File" script
                      "-TargetPid" (number-to-string (emacs-pid))))))))

(defun my/w32-tty-setup ()
  "Apply all Windows console Emacs settings.
Switch the console to UTF-8, disable C-z, translate the WezTerm
carrier keys, and start the input freeze guard (not in batch mode)."
  (when (my/w32-tty-p)
    (my/w32-tty-use-utf-8)
    ;; C-z suspends a Windows console Emacs into a subshell, which
    ;; resets the console code page and garbles Hangul.  psmux prefix
    ;; is also C-z, so it is easy to hit by accident (C-x C-z still works).
    (global-unset-key (kbd "C-z"))
    (my/w32-tty-translate-carrier-keys)
    (unless noninteractive
      (my/w32-tty-start-input-guard))))

(provide 'w32-tty)
;;; w32-tty.el ends here
