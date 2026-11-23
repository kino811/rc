;;; large-file-tests.el --- Large-file hook tests -*- lexical-binding: t; -*-

(require 'ert)

(load (expand-file-name "../lisp/large-file-setup.el"
                        (file-name-directory load-file-name))
      nil t)

(ert-deftest my/large-file-hook-registered ()
  (should (memq #'my-find-fie-check-make-large-file-read-only-hook
                find-file-hook)))

(ert-deftest my/large-file-line-numbers-off ()
  (with-temp-buffer
    (insert (make-string (1+ (* 1024 1024)) ?x))
    (text-mode)
    (display-line-numbers-mode 1)
    (my-find-fie-check-make-large-file-read-only-hook)
    (should-not display-line-numbers-mode)))

(ert-deftest my/large-file-line-numbers-off-with-global-mode ()
  (let ((was-on global-display-line-numbers-mode))
    (unwind-protect
        (with-temp-buffer
          (global-display-line-numbers-mode 1)
          (insert (make-string (1+ (* 1024 1024)) ?x))
          (text-mode)
          (my-find-fie-check-make-large-file-read-only-hook)
          (should-not display-line-numbers-mode))
      (global-display-line-numbers-mode (if was-on 1 -1)))))

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
