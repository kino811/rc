;;; large-file-tests.el --- Large-file hook tests -*- lexical-binding: t; -*-

(require 'ert)

(let ((source (expand-file-name "../myInit.org"
                                (file-name-directory load-file-name))))
  (with-temp-buffer
    (insert-file-contents source)
    (search-forward "(defun my-find-fie-check-make-large-file-read-only-hook ")
    (goto-char (match-beginning 0))
    (eval (read (current-buffer)) t)))

(ert-deftest my/large-file-image-mode-preserved ()
  (with-temp-buffer
    (insert (make-string (1+ (* 1024 1024)) ?x))
    (setq major-mode 'image-mode)
    (buffer-enable-undo)
    (let ((undo-before buffer-undo-list))
      (my-find-fie-check-make-large-file-read-only-hook)
      (should (eq major-mode 'image-mode))
      (should-not buffer-read-only)
      (should (eq buffer-undo-list undo-before)))))

(ert-deftest my/large-file-text-fallback-preserved ()
  (with-temp-buffer
    (insert (make-string (1+ (* 1024 1024)) ?x))
    (text-mode)
    (buffer-enable-undo)
    (my-find-fie-check-make-large-file-read-only-hook)
    (should (eq major-mode 'fundamental-mode))
    (should buffer-read-only)
    (should (eq buffer-undo-list t))))

(ert-deftest my/large-file-text-at-threshold-unchanged ()
  (with-temp-buffer
    (insert (make-string (* 1024 1024) ?x))
    (text-mode)
    (buffer-enable-undo)
    (my-find-fie-check-make-large-file-read-only-hook)
    (should (eq major-mode 'text-mode))
    (should-not buffer-read-only)
    (should-not (eq buffer-undo-list t))))
