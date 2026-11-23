;;; html-single-file-tests.el --- Tests for lisp/html-single-file.el -*- lexical-binding: t; -*-

;; Run: emacs -Q --batch -l tests/html-single-file-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "../lisp/html-single-file.el"
                        (file-name-directory load-file-name))
      nil t)

(defconst html-single-file-tests--png
  (base64-decode-string
   "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==")
  "A 1x1 PNG image.")

(defmacro html-single-file-tests--with-dir (dir &rest body)
  "Run BODY with DIR bound to a new temporary directory."
  (declare (indent 1))
  `(let ((,dir (file-name-as-directory (make-temp-file "html-single-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,dir t))))

(defun html-single-file-tests--write (file content &optional binary)
  "Write CONTENT to FILE, creating its directory."
  (make-directory (file-name-directory file) t)
  (let ((coding-system-for-write (if binary 'binary 'utf-8)))
    (with-temp-file file
      (when binary (set-buffer-multibyte nil))
      (insert content))))

(defun html-single-file-tests--read (file)
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (buffer-string)))

(ert-deftest html-single-file-output-name ()
  (should (equal (my/html-single-file-output-name "c:/a/note.html")
                 "c:/a/note-single.html")))

(ert-deftest html-single-file-embeds-local-images-and-css ()
  (html-single-file-tests--with-dir dir
    (html-single-file-tests--write (expand-file-name "img/dot.png" dir)
                                   html-single-file-tests--png t)
    (html-single-file-tests--write (expand-file-name "my dir/dot 2.png" dir)
                                   html-single-file-tests--png t)
    (html-single-file-tests--write (expand-file-name "style.css" dir)
                                   "#content { max-width: 85%; }")
    (html-single-file-tests--write
     (expand-file-name "note.html" dir)
     (concat "<html><head><link rel=\"stylesheet\" href=\"style.css\" />"
             "<style>.title { color: red; }</style></head><body>"
             "<p>한글 본문</p>"
             "<img src=\"img/dot.png\" alt=\"dot\" />"
             "<img alt=\"two\" src='my%20dir/dot%202.png' />"
             "<img src=\"missing.png\" />"
             "<img src=\"https://example.com/a.png\" />"
             "<img src=\"data:image/png;base64,AAAA\" />"
             "</body></html>"))
    (let* ((result (my/html-single-file-embed (expand-file-name "note.html" dir)))
           (out (html-single-file-tests--read (plist-get result :output))))
      (should (equal (plist-get result :output)
                     (expand-file-name "note-single.html" dir)))
      ;; two images + one stylesheet
      (should (= (plist-get result :embedded) 3))
      (should (equal (plist-get result :missing) '("missing.png")))
      (should (= 2 (with-temp-buffer
                     (insert out)
                     (how-many "src=[\"']data:image/png;base64,iVBOR" (point-min) (point-max)))))
      (should (string-match-p "max-width: 85%" out))
      (should-not (string-match-p "<link" out))
      ;; Everything else is kept as is.
      (should (string-match-p "\\.title { color: red; }" out))
      (should (string-match-p "한글 본문" out))
      (should (string-match-p "alt=\"dot\"" out))
      (should (string-match-p "src=\"missing.png\"" out))
      (should (string-match-p "src=\"https://example.com/a.png\"" out))
      (should (string-match-p "src=\"data:image/png;base64,AAAA\"" out))
      ;; The source HTML is not modified.
      (should (string-match-p "src=\"img/dot.png\""
                              (html-single-file-tests--read
                               (expand-file-name "note.html" dir)))))))

(ert-deftest html-single-file-org-menu-entry ()
  (require 'ox-html)
  (let ((menu (org-export-backend-menu
               (org-export-get-backend 'html-single-file))))
    (should (eq (car menu) ?h))
    (should (equal (mapcar (lambda (entry) (list (car entry) (nth 2 entry)))
                           (nth 2 menu))
                   '((?s my/org-html-export-to-single-file)
                     (?S my/org-html-export-to-single-file-and-open))))))

(ert-deftest html-single-file-org-export-and-open ()
  (require 'ox-html)
  (html-single-file-tests--with-dir dir
    (html-single-file-tests--write (expand-file-name "note.org" dir)
                                   "* Head\nText\n")
    (let ((buffer (find-file-noselect (expand-file-name "note.org" dir)))
          (opened nil))
      (unwind-protect
          ;; Do not start a browser; record what would be opened.
          (cl-letf (((symbol-function 'org-open-file)
                     (lambda (path &rest _) (setq opened path))))
            (with-current-buffer buffer
              (my/org-html-export-to-single-file-and-open))
            (should (equal opened (expand-file-name "note-single.html" dir)))
            (should (file-exists-p opened)))
        (kill-buffer buffer)))))

(ert-deftest html-single-file-org-export-end-to-end ()
  (require 'ox-html)
  (html-single-file-tests--with-dir dir
    (html-single-file-tests--write (expand-file-name "img/dot.png" dir)
                                   html-single-file-tests--png t)
    (html-single-file-tests--write
     (expand-file-name "note.org" dir)
     "#+TITLE: Test\n* Head\nText\n\n[[file:img/dot.png]]\n**** Deep\nbody\n")
    (let ((org-html-head "<style type=\"text/css\">#content { max-width: 85%; }</style>")
          (buffer (find-file-noselect (expand-file-name "note.org" dir))))
      (unwind-protect
          (let* ((output (with-current-buffer buffer
                           (my/org-html-export-to-single-file)))
                 (out (html-single-file-tests--read output)))
            (should (equal output (expand-file-name "note-single.html" dir)))
            (should (file-exists-p (expand-file-name "note.html" dir)))
            (should (string-match-p "src=\"data:image/png;base64," out))
            (should (string-match-p "max-width: 85%" out))
            ;; Org's own structure is kept (pandoc dropped it).
            (should (string-match-p "class=\"org-ol\"" out)))
        (kill-buffer buffer)))))

;;; html-single-file-tests.el ends here
