;;; large-file-setup.el --- Open large files in a fast read-only state -*- lexical-binding: t -*-

;; Shared by myInit.org (full config) and quick-init.el (emacs-q).
;; Non-image buffers over 1 MiB are opened read only, without undo, in
;; fundamental-mode and without line numbers, so they stay responsive.
;; Image buffers keep their image mode.  Tested by tests/large-file-tests.el.

;;; Code:

(defun my-find-fie-check-make-large-file-read-only-hook ()
  "Make non-image buffers over 1 MiB read only for speed."
  (when (and (not (derived-mode-p 'image-mode))
             (> (buffer-size) (* 1024 1024)))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    ;; A minor mode called from Lisp with a nil argument is turned ON, so
    ;; pass -1 to turn line numbers off.  This runs after `fundamental-mode',
    ;; which may turn them back on through `global-display-line-numbers-mode'.
    (display-line-numbers-mode -1)))

(add-hook 'find-file-hook #'my-find-fie-check-make-large-file-read-only-hook)

(provide 'large-file-setup)
;;; large-file-setup.el ends here
