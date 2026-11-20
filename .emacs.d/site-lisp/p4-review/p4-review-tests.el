;;; p4-review-tests.el --- Tests for p4-review -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'p4-review)
(defvar c++-mode-hook)

(defmacro p4-review-test--workspace (&rest body)
  "Run BODY in an isolated workspace with no running editor integration."
  (declare (indent 0) (debug t))
  `(let* ((directory (make-temp-file
                     (expand-file-name ".p4-review-test-" default-directory) t))
          (default-directory (file-name-as-directory directory))
          (p4-review--active-session nil))
     (unwind-protect
         (save-window-excursion ,@body)
       (when p4-review--active-session (p4-review-quit))
       (dolist (buffer (buffer-list))
         (when-let* ((path (buffer-file-name buffer))
                     ((file-in-directory-p path directory)))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory directory t))))

(defun p4-review-test--fixture (directory &optional shelved depot-path unmapped)
  "Return a fake backend for DIRECTORY, SHELVED mode and DEPOT-PATH.
When UNMAPPED, simulate a P4 client view that cannot map the review file."
  (setq depot-path (or depot-path "//depot/example.cpp"))
  (lambda (_session command arguments callback &optional tagged)
    (pcase command
      ("describe"
       (should tagged)
       (should (member arguments
                       (if shelved '(("-s" "123") ("-s" "-S" "123"))
                         '(("-s" "123")))))
       (funcall callback
                (json-serialize
                 `((change . "123") (status . ,(if shelved "pending" "submitted"))
                   (desc . "Review example")
                   (depotFile0 . ,depot-path)
                   (action0 . "edit") (rev0 . "2") (type0 . "text")))
                nil))
      ("print"
       (should (equal (car arguments) "-q"))
       (funcall callback
                (if (equal (cadr arguments)
                           (concat depot-path (if shelved "#2" "#1")))
                    "int answer = 1;\n"
                  "int answer = 2;\n")
                nil))
      ("where"
       (if unmapped
           (funcall callback "" "File is not mapped in this client")
         (funcall callback
                  (json-serialize
                   `((path . ,(expand-file-name "example.cpp" directory))))
                  nil)))
      (_ (ert-fail (format "Unexpected backend command: %s" command))))))

(defun p4-review-test--review-json (&optional pending archive)
  "Return a review response with a latest PENDING version and optional ARCHIVE."
  (format
   (concat "{\"data\":{\"reviews\":[{\"id\":123,\"commits\":[234],\"versions\":["
           "{\"change\":234,\"pending\":false},"
           "{\"change\":456,\"pending\":%s%s}]}]}}")
   (if pending "true" "false")
   (if archive ",\"archiveChange\":789" "")))

(defun p4-review-test--review-backend (directory &optional pending)
  "Return a fake backend for review 123 resolving to a CL in DIRECTORY."
  (let ((file-backend (p4-review-test--fixture directory pending)))
    (lambda (session command arguments callback &optional tagged)
      (pcase command
        ("describe"
         (funcall callback
                  (if (equal (car (last arguments)) "123")
                      "{\"change\":\"123\",\"status\":\"pending\"}"
                    (json-serialize
                     `((change . "456") (status . ,(if pending "pending" "submitted"))
                       (desc . "Resolved example")
                       (depotFile0 . "//depot/example.cpp")
                       (action0 . "edit") (rev0 . "2") (type0 . "text"))))
                  nil))
        ("property"
         (should (equal arguments '("-l" "-n" "P4.Swarm.URL")))
         (funcall callback "{\"value\":\"https://swarm.invalid\"}" nil))
        ("info"
         (funcall callback
                  "{\"userName\":\"tester\",\"serverCluster\":\"fixture\"}" nil))
        ("tickets"
         (should-not tagged)
         (funcall callback "localhost:fixture (tester) fixture-ticket\n" nil))
        (_ (funcall file-backend session command arguments callback tagged))))))

(ert-deftest p4-review-test-json-errors-are-not-success-records ()
  (should (equal (p4-review--parse-records
                  "{\"change\":\"123\"}\n{\"severity\":0,\"data\":\"info\"}\n")
                 '(((change . "123")))))
  (should-error (p4-review--parse-records "{\"severity\":3,\"data\":\"denied\"}")
                :type 'p4-review-error)
  (should-error (p4-review--parse-records "not JSON")
                :type 'p4-review-error)
  (should-error (p4-review--parse-records "42")
                :type 'p4-review-error)
  (should-error (p4-review--parse-records "[{}]")
                :type 'p4-review-error)
  (should-error (p4-review--parse-records "{\"severity\":\"bad\"}")
                :type 'p4-review-error))

(ert-deftest p4-review-test-revision-specs ()
  (let ((session (p4-review--make-session :change "123"))
        (file (p4-review--make-file
               :path "//depot/example.cpp" :action "edit" :revision 4)))
    (should (equal (p4-review--specs session file)
                   '("//depot/example.cpp#3" "//depot/example.cpp#4")))
    (setf (p4-review--session-shelved session) t)
    (should (equal (p4-review--specs session file)
                   '("//depot/example.cpp#4" "//depot/example.cpp@=123")))
    (setf (p4-review--file-action file) "add")
    (should-not (car (p4-review--specs session file)))
    (setf (p4-review--file-action file) "move/add")
    (should-not (car (p4-review--specs session file)))
    (setf (p4-review--file-action file) "delete")
    (should-not (cadr (p4-review--specs session file)))
    (setf (p4-review--file-action file) "move/delete")
    (should-not (cadr (p4-review--specs session file)))
    (setf (p4-review--session-shelved session) nil
          (p4-review--file-action file) "edit"
          (p4-review--file-revision file) 1)
    (should-not (car (p4-review--specs session file)))))

(ert-deftest p4-review-test-describe-requires-revision-data ()
  (let ((record '((depotFile0 . "//depot/a.cpp") (action0 . "edit")
                  (rev0 . "3") (type0 . "text")
                  (depotFile1 . "//depot/b.cpp") (action1 . "add")
                  (rev1 . "1") (type1 . "binary"))))
    (let ((files (p4-review--files-from-record record)))
      (should (= (length files) 2))
      (should (= (p4-review--file-revision (car files)) 3))
      (should (p4-review--binary-p (cadr files)))))
  (should-error
   (p4-review--files-from-record
    '((depotFile0 . "//depot/a.cpp") (action0 . "edit")))
   :type 'p4-review-error))

(ert-deftest p4-review-test-real-diff-and-deletion-anchors ()
  (let* ((old "a\nb\nc\n")
         (new "a\nB\nc\n")
         (hunks (p4-review--diff old new)))
    (should (= (length hunks) 1))
    (should (member "-b" (p4-review--hunk-lines (car hunks))))
    (should (member "+B" (p4-review--hunk-lines (car hunks)))))
  (let ((hunk (car (p4-review--diff "gone\n" ""))))
    (should (zerop (p4-review--hunk-new-count hunk)))
    (should (= (p4-review--hunk-line hunk) 1)))
  (should-not (p4-review--diff "same\n" "same\n"))
  (should (p4-review--diff "no newline" "no newline\n")))

(ert-deftest p4-review-test-overlays-preserve-source-text ()
  (with-temp-buffer
    (insert "a\nB\nc\n")
    (let* ((session (p4-review--make-session))
           (file (p4-review--make-file
                  :hunks (p4-review--diff "a\nb\nc\n" (buffer-string))))
           (before (buffer-string)))
      (p4-review--decorate session file)
      (unwind-protect
          (progn
            (should (equal before (buffer-string)))
            (should (= (length (p4-review--session-overlays session)) 1))
            (should (seq-some
                     (lambda (overlay)
                       (string-prefix-p
                        "-b\n" (substring-no-properties
                                (or (overlay-get overlay 'before-string) ""))))
                     (p4-review--session-overlays session))))
        (mapc #'delete-overlay (p4-review--session-overlays session))))))

(ert-deftest p4-review-test-refined-replacement-aligns-and-preserves-source ()
  (with-temp-buffer
    (insert "\tvalue = Call(side, msg.mPos);\n")
    (set-buffer-modified-p nil)
    (let* ((session (p4-review--make-session))
           (file (p4-review--make-file
                  :hunks (p4-review--diff
                          "\tvalue = Call(side, shotPos);\n" (buffer-string))))
           (hunk (car (p4-review--file-hunks file)))
           (position (point))
           (text (buffer-string)))
      (unwind-protect
          (progn
            (p4-review--decorate session file)
            (p4-review--refine-displayed-hunk session hunk)
            (let* ((base (seq-find (lambda (item)
                                    (overlay-get item 'p4-review-deleted-lines))
                                  (p4-review--session-overlays session)))
                   (deleted (overlay-get base 'before-string))
                   (old-start (string-match "shotPos" deleted))
                   (new-start (string-match "msg.mPos" text))
                   (fine (seq-find
                          (lambda (item)
                            (eq (overlay-get item 'face) 'p4-review-refine-added))
                          (p4-review--session-overlays session))))
              (should (equal (get-text-property old-start 'face deleted)
                             'p4-review-refine-removed))
              (should (= (overlay-start fine) (1+ new-start)))
              (should (= (overlay-end fine) (+ 1 new-start (length "msg.mPos"))))
              (should (> (overlay-get fine 'priority) 0))
              (should (equal (substring-no-properties deleted)
                             "-\tvalue = Call(side, shotPos);\n+"))
              (should (equal (get-text-property 0 'display deleted)
                             '((margin left-margin) "-")))
              (should (equal (get-text-property (1- (length deleted))
                                                'display deleted)
                             '((margin left-margin) "+"))))
            (should (equal (buffer-string) text))
            (should (= (point) position))
            (should-not (buffer-modified-p)))
        (mapc #'delete-overlay (p4-review--session-overlays session))))))

(ert-deftest p4-review-test-refinement-compares-unequal-multiline-blocks ()
  (with-temp-buffer
    (insert "left(msg.mPos);\nextra();\nright(shooter->GetPos());\n")
    (let* ((session (p4-review--make-session))
           (file (p4-review--make-file
                  :hunks (p4-review--diff
                          "left(shotPos);\nright(shotPos);\n" (buffer-string))))
           (hunk (car (p4-review--file-hunks file))))
      (unwind-protect
          (progn
            (p4-review--decorate session file)
            (p4-review--refine-displayed-hunk session hunk)
            (let ((fine (seq-filter
                         (lambda (item)
                           (eq (overlay-get item 'face) 'p4-review-refine-added))
                         (p4-review--session-overlays session))))
              (dolist (word '("msg.mPos" "shooter" "GetPos"))
                (goto-char (point-min))
                (search-forward word)
                (should (seq-some
                         (lambda (item)
                           (and (<= (overlay-start item) (- (point) (length word)))
                                (>= (overlay-end item) (point))))
                         fine)))
              (dolist (line '(1 3))
                (should-not (seq-some
                             (lambda (item)
                               (= (overlay-start item)
                                  (p4-review--line-position line)))
                             fine)))))
        (mapc #'delete-overlay (p4-review--session-overlays session))))))

(ert-deftest p4-review-test-refinement-is-lazy-and-cached-across-decoration ()
  (with-temp-buffer
    (let* ((gap (make-string 10 ?\n))
           (old (concat "first(oldValue);\n" gap "last(oldValue);\n"))
           (new (concat "first(newValue);\n" gap "last(newValue);\n"))
           (file (p4-review--make-file :hunks (p4-review--diff old new)))
           (first (car (p4-review--file-hunks file)))
           (last (cadr (p4-review--file-hunks file)))
           (session (p4-review--make-session))
           (refiner (symbol-function 'diff-refine-hunk))
           (calls 0))
      (insert new)
      (unwind-protect
          (cl-letf (((symbol-function 'diff-refine-hunk)
                     (lambda (&rest args)
                       (cl-incf calls)
                       (apply refiner args))))
            (p4-review--decorate session file)
            (should (= calls 0))
            (p4-review--refine-displayed-hunk session first)
            (p4-review--refine-displayed-hunk session first)
            (should (= calls 1))
            (should-not (gethash last p4-review--refinement-cache))
            (mapc #'delete-overlay (p4-review--session-overlays session))
            (setf (p4-review--session-overlays session) nil)
            (p4-review--decorate session file)
            (p4-review--refine-displayed-hunk session first)
            (should (= calls 1))
            (p4-review--refine-displayed-hunk session last)
            (should (= calls 2)))
        (mapc #'delete-overlay (p4-review--session-overlays session))))))

(ert-deftest p4-review-test-refinement-skips-pure-and-oversized-changes ()
  (dolist (example '(("" "new\n" nil)
                     ("old\n" "" nil)
                     ("oldValue();\n" "newValue();\n" t)))
    (pcase-let* ((`(,old ,new ,oversized) example)
                 (hunk (car (p4-review--diff old new)))
                 (p4-review-refine-max-chars (if oversized 1 30000)))
      (cl-letf (((symbol-function 'diff-refine-hunk)
                 (lambda (&rest _) (ert-fail "Unexpected refinement"))))
        (let ((spans (p4-review--hunk-refinement hunk)))
          (if oversized
              (should (eq spans 'skipped))
            (should (vectorp spans))
            (should (seq-every-p #'null spans))))))))

(ert-deftest p4-review-test-refinement-bounds-many-small-replacement-blocks ()
  (let* ((old (mapconcat (lambda (n) (format "oldValue%d();\ncontext%d();\n" n n))
                        (number-sequence 1 10) ""))
         (new (mapconcat (lambda (n) (format "newValue%d();\ncontext%d();\n" n n))
                        (number-sequence 1 10) ""))
         (hunks (p4-review--diff old new)))
    (should (= (length hunks) 1))
    (cl-letf (((symbol-function 'diff-refine-hunk)
               (lambda (&rest _) (ert-fail "Too many diff processes"))))
      (should (eq (p4-review--hunk-refinement (car hunks)) 'skipped)))))

(ert-deftest p4-review-test-scroll-refines-only-visible-hunks-and-removes-hooks ()
  (save-window-excursion
    (with-temp-buffer
      (let* ((gap (make-string 200 ?\n))
             (file (p4-review--make-file
                    :hunks (p4-review--diff
                            (concat "first(oldValue);\n" gap "last(oldValue);\n")
                            (concat "first(newValue);\n" gap "last(newValue);\n"))))
             (first (car (p4-review--file-hunks file)))
             (last (cadr (p4-review--file-hunks file)))
             (window (selected-window))
             (session (p4-review--make-session
                       :files (list file) :source-window window
                       :source-buffer (current-buffer))))
        (insert "first(newValue);\n" gap "last(newValue);\n")
        (set-window-buffer window (current-buffer))
        (p4-review--decorate session file)
        (setq p4-review--session session)
        (p4-review-source-mode 1)
        (unwind-protect
            (cl-letf (((symbol-function 'window-end)
                       (lambda (&rest _) (p4-review--line-position 4))))
              (run-hook-with-args 'window-scroll-functions window (point-min))
              (should (gethash first p4-review--refinement-cache))
              (should-not (gethash last p4-review--refinement-cache))
              (cl-letf (((symbol-function 'window-end)
                         (lambda (&rest _) (point-max))))
                (run-hook-with-args 'window-scroll-functions window
                                   (p4-review--line-position 200)))
              (should (gethash last p4-review--refinement-cache)))
          (p4-review-source-mode -1)
          (should-not (memq #'p4-review--refine-visible-hunks post-command-hook))
          (should-not (memq #'p4-review--refine-visible-hunks window-scroll-functions))
          (mapc #'delete-overlay (p4-review--session-overlays session)))))))

(ert-deftest p4-review-test-refinement-failure-is-reported-without-losing-lines ()
  (with-temp-buffer
    (insert "newValue();\n")
    (let* ((file (p4-review--make-file
                  :hunks (p4-review--diff "oldValue();\n" (buffer-string))))
           (hunk (car (p4-review--file-hunks file)))
           (session (p4-review--make-session))
           (calls 0)
           reported)
      (unwind-protect
          (progn
            (p4-review--decorate session file)
            (cl-letf (((symbol-function 'call-process)
                       (lambda (&rest _) (cl-incf calls) 2))
                      ((symbol-function 'p4-review--fail)
                       (lambda (_session text) (setq reported text))))
              (p4-review--refine-displayed-hunk session hunk)
              (p4-review--refine-displayed-hunk session hunk))
            (should (= calls 1))
            (should (string-match-p "GNU diff refinement failed" reported))
            (should (= (length (p4-review--session-overlays session)) 1))
            (should (equal (buffer-string) "newValue();\n")))
        (mapc #'delete-overlay (p4-review--session-overlays session))))))

(ert-deftest p4-review-test-addition-and-deletion-overlay-positions ()
  (dolist (example '(("a\nb\nc\n" "a\nc\n" 3)
                     ("a\nb\n" "a\n" 3)
                     ("gone\n" "" 1)
                     ("" "added" 1)))
    (pcase-let ((`(,old ,new ,position) example))
      (with-temp-buffer
        (insert new)
        (set-buffer-modified-p nil)
        (let ((session (p4-review--make-session))
              (file (p4-review--make-file :hunks (p4-review--diff old new))))
          (unwind-protect
              (progn
                (p4-review--decorate session file)
                (should (equal (buffer-string) new))
                (should-not (buffer-modified-p))
                (should (= (length (p4-review--session-overlays session)) 1))
                (should (= (overlay-start
                            (car (p4-review--session-overlays session)))
                           position)))
            (mapc #'delete-overlay (p4-review--session-overlays session))))))))

(ert-deftest p4-review-test-matching-does-not-revert-dirty-or-narrowed-buffers ()
  (p4-review-test--workspace
    (let* ((path (expand-file-name "example.cpp"))
           (file (p4-review--make-file :local-path path :new "review\n")))
      (write-region "disk\n" nil path nil 'silent)
      (let ((buffer (find-file-noselect path)))
        (with-current-buffer buffer
          (erase-buffer)
          (insert "unsaved\n"))
        (should-not (p4-review--matching-buffer file))
        (with-current-buffer buffer
          (should (equal (buffer-string) "unsaved\n"))
          (should (buffer-modified-p))
          (erase-buffer)
          (insert "review\n"))
        (should (eq buffer (p4-review--matching-buffer file)))
        (with-current-buffer buffer (narrow-to-region 1 4))
        (should-not (p4-review--matching-buffer file))))))

(ert-deftest p4-review-test-snapshot-keeps-language-mode-without-file-hooks ()
  (let* ((hooks-ran nil)
         (enable-dir-local-variables t)
         (c++-mode-hook (list (lambda () (setq hooks-ran t))))
         (session (p4-review--make-session :change "123"
                                           :directory default-directory))
         (file (p4-review--make-file
                :path "//depot/example.cpp" :new "int answer;\n"))
         (mode-selector (symbol-function 'set-auto-mode))
         buffer)
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'dir-locals-find-file)
                     (lambda (&rest _)
                       (ert-fail "Snapshot must not search directory locals")))
                    ((symbol-function 'set-auto-mode)
                     (lambda (&rest args)
                       (should-not (file-name-directory buffer-file-name))
                       (apply mode-selector args))))
            (setq buffer (p4-review--snapshot session file)))
          (with-current-buffer buffer
            (should (derived-mode-p 'c++-mode))
            (should-not buffer-file-name)
            (should-not hooks-ran)
            (should (equal (buffer-string) "int answer;\n"))
            (should-not (buffer-modified-p))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest p4-review-test-snapshot-preserves-inline-mode-hint ()
  (let* ((enable-local-variables t)
         (session (p4-review--make-session :change "123"
                                           :directory default-directory))
         (file (p4-review--make-file
                :path "//depot/example.h"
                :new "// -*- mode: c++ -*-\nclass Example {};\n"))
         (buffer (p4-review--snapshot session file)))
    (unwind-protect
        (with-current-buffer buffer
          (should (derived-mode-p 'c++-mode))
          (should-not buffer-file-name))
      (kill-buffer buffer))))

(ert-deftest p4-review-test-snapshot-rejects-workspace-lsp ()
  (with-temp-buffer
    (let ((p4-review--session
           (p4-review--make-session :source-buffer (current-buffer)
                                    :snapshot-buffer (current-buffer))))
      (should-error (p4-review--require-workspace-source) :type 'user-error)
      (setf (p4-review--session-snapshot-buffer p4-review--session) nil
            (p4-review--session-files p4-review--session)
            (list (p4-review--make-file :new "")))
      (should-not (p4-review--require-workspace-source))
      (insert "changed")
      (should-error (p4-review--require-workspace-source) :type 'user-error))))

(ert-deftest p4-review-test-hunk-navigation-crosses-files ()
  (with-temp-buffer
    (insert "one\n")
    (let* ((file (p4-review--make-file
                  :hunks (list (p4-review--make-hunk
                                :new-start 1 :new-count 1))))
           (p4-review--session
            (p4-review--make-session :source-buffer (current-buffer)
                                     :files (list file file)))
           target)
      (goto-char (point-min))
      (cl-letf (((symbol-function 'p4-review--select-file)
                 (lambda (_session index jump)
                   (setq target (list index jump)))))
        (p4-review-next-hunk)
        (should (equal target '(1 first)))
        (setf (p4-review--session-index p4-review--session) 1)
        (p4-review-previous-hunk)
        (should (equal target '(0 last)))))))

(ert-deftest p4-review-test-open-matching-workspace-and-restore-state ()
  (p4-review-test--workspace
    (set-window-margins (selected-window) 4 3)
    (let* ((path (expand-file-name "example.cpp"))
           (backend (p4-review-test--fixture directory))
           buffer)
      (write-region "int answer = 2;\n" nil path nil 'silent)
      (setq buffer (find-file-noselect path))
      (with-current-buffer buffer (setq buffer-read-only nil))
      (cl-letf (((symbol-function 'p4-review--run) backend))
        (let ((session (p4-review-open "123" directory)))
          (should (equal (window-margins
                          (p4-review--session-source-window session))
                         '(4 . 3)))
          (should (eq (p4-review--session-source-buffer session) buffer))
          (should-not (p4-review--session-snapshot-buffer session))
          (with-current-buffer buffer
            (should (derived-mode-p 'c++-mode))
            (should p4-review-source-mode)
            (should buffer-read-only)
            (should-not (buffer-modified-p)))
          (should (string-match-p "matches review"
                                   (p4-review--source-label session)))
          (p4-review-quit)
          (should (equal (window-margins (selected-window)) '(4 . 3)))
          (with-current-buffer buffer
            (should-not p4-review-source-mode)
            (should-not buffer-read-only)
            (should-not (buffer-modified-p))))))))

(ert-deftest p4-review-test-open-shelf-with-different-workspace ()
  (p4-review-test--workspace
    (let* ((path (expand-file-name "example.cpp"))
           (backend (p4-review-test--fixture directory t)))
      (write-region "workspace differs\n" nil path nil 'silent)
      (cl-letf (((symbol-function 'p4-review--run) backend))
        (let* ((session (p4-review-open "123" directory))
               (snapshot (p4-review--session-snapshot-buffer session)))
          (should (p4-review--session-shelved session))
          (should (buffer-live-p snapshot))
          (with-current-buffer snapshot
            (should (equal (buffer-string) "int answer = 2;\n"))
            (should buffer-read-only)
            (should-not buffer-file-name))
          (should (string-match-p "NOT attached" (p4-review--source-label session)))
          (p4-review-quit)
          (should-not (buffer-live-p snapshot))
          (with-temp-buffer
            (insert-file-contents path)
            (should (equal (buffer-string) "workspace differs\n"))))))))

(ert-deftest p4-review-test-open-balances-two-panels-and-restores-layout ()
  (p4-review-test--workspace
    (delete-other-windows)
    (skip-unless (>= (window-total-width) 80))
    (let* ((backend (p4-review-test--fixture directory))
           (p4-review-files-width 10)
           (frame-width (window-total-width))
           (original-window (selected-window))
           (original-buffer (window-buffer original-window)))
      (cl-letf (((symbol-function 'p4-review--run) backend))
        (dolist (details-width (list 10 frame-width))
          (let* ((p4-review-details-width details-width)
                 (session (p4-review-open "123" directory))
                 (files (p4-review--session-files-window session))
                 (source (p4-review--session-source-window session))
                 (right (p4-review--session-details-window session))
                 (widths (mapcar #'window-total-width (window-list))))
            (should (= (length widths) 2))
            (should (<= (- (apply #'max widths) (apply #'min widths)) 1))
            (should (< (car (window-edges files)) (car (window-edges source))))
            (should-not right)
            (should-not (get-buffer-window
                         (p4-review--session-details-buffer session) t))
            (should (eq (selected-window) files))
            (p4-review-quit)
            (should (equal (window-list) (list original-window)))
            (should (eq (window-buffer original-window) original-buffer))
            (should (= (window-total-width original-window) frame-width))))))))

(ert-deftest p4-review-test-opening-source-asynchronously-keeps-file-list-focus ()
  (p4-review-test--workspace
    (let* ((backend (p4-review-test--fixture directory))
           (pending nil))
      (cl-letf (((symbol-function 'p4-review--run)
                 (lambda (session command arguments callback &optional tagged)
                   (push (lambda ()
                           (funcall backend session command arguments callback tagged))
                         pending))))
        (let* ((session (p4-review-open "123" directory))
               (files (p4-review--session-files-window session)))
          (should (eq (selected-window) files))
          (should-not (p4-review--session-source-buffer session))
          (while pending
            (funcall (pop pending)))
          (should (buffer-live-p (p4-review--session-source-buffer session)))
          (should (eq (selected-window) files))
          (should (= (get-text-property (window-point files)
                                         'p4-review-file-index
                                         (window-buffer files))
                     0))
          (with-selected-window files
            (p4-review-select-file))
          (should (eq (selected-window) files))
          (should-not (get-buffer-window
                       (p4-review--session-details-buffer session) t)))))))

(ert-deftest p4-review-test-read-errors-remain-visible ()
  (p4-review-test--workspace
    (cl-letf (((symbol-function 'p4-review--run)
               (lambda (_session _command _arguments callback &optional _tagged)
                 (funcall callback "" "permission denied"))))
      (let ((session (p4-review-open "123" directory)))
        (should-not (p4-review--session-source-buffer session))
        (with-current-buffer (p4-review--session-files-buffer session)
          (should (string-match-p "ERROR: permission denied" (buffer-string))))))))

(ert-deftest p4-review-test-mutating-commands-are-rejected ()
  (should-error
   (p4-review--run (p4-review--make-session) "unshelve" nil #'ignore)
   :type 'p4-review-error))

(ert-deftest p4-review-test-cancel-invalidates-before-deleting-processes ()
  (let* ((session (p4-review--make-session :processes '(fake)))
         observed)
    (cl-letf (((symbol-function 'process-live-p) (lambda (_) t))
              ((symbol-function 'delete-process)
               (lambda (_) (setq observed (p4-review--session-generation session)))))
      (p4-review--cancel session))
    (should (= observed 1))
    (should-not (p4-review--session-processes session))))

(ert-deftest p4-review-test-invalid-change-does-not-open-ui ()
  (should-error (p4-review-open "123; edit file") :type 'user-error)
  (should-error (p4-review-open "0") :type 'user-error))

(ert-deftest p4-review-test-file-hooks-cannot-invalidate-workspace-match ()
  (p4-review-test--workspace
    (let* ((path (expand-file-name "example.cpp"))
           (file (p4-review--make-file :local-path path :new "review\n"))
           (find-file-hook
            (list (lambda () (goto-char (point-max)) (insert "hook changed\n")))))
      (write-region "review\n" nil path nil 'silent)
      (should-not (p4-review--matching-buffer file))
      (with-current-buffer (get-file-buffer path)
        (should (buffer-modified-p))
        (should (equal (buffer-string) "review\nhook changed\n"))))))

(ert-deftest p4-review-test-killing-panels-restores-source-state ()
  (p4-review-test--workspace
    (let ((backend (p4-review-test--fixture directory)))
      (write-region "int answer = 2;\n" nil "example.cpp" nil 'silent)
      (cl-letf (((symbol-function 'p4-review--run) backend))
        (let* ((session (p4-review-open "123" directory))
               (source (p4-review--session-source-buffer session)))
          (kill-buffer (p4-review--session-details-buffer session))
          (should-not p4-review--active-session)
          (should (p4-review--session-closed session))
          (with-current-buffer source
            (should-not buffer-read-only)
            (should-not p4-review-source-mode)))))))

(ert-deftest p4-review-test-window-setup-errors-clean-up ()
  (p4-review-test--workspace
    (let ((windows (window-list)))
      (cl-letf (((symbol-function 'split-window)
                 (lambda (&rest _) (error "Cannot split test window"))))
        (should-error (p4-review-open "123" directory)))
      (should-not p4-review--active-session)
      (should (equal windows (window-list))))))

(ert-deftest p4-review-test-callback-errors-are-not-parsing-errors ()
  (let ((calls 0))
    (cl-letf (((symbol-function 'p4-review--run)
               (lambda (_session _command _arguments callback &optional _tagged)
                 (funcall callback "{\"change\":\"123\"}" nil))))
      (should-error
       (p4-review--records
        nil "describe" nil
        (lambda (_records _error)
          (cl-incf calls)
          (signal 'p4-review-error '("callback failed"))))
       :type 'p4-review-error)
      (should (= calls 1)))))

(ert-deftest p4-review-test-process-start-errors-use-error-callback ()
  (let (reported)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest _)
                 (signal 'file-error '("Cannot start test process")))))
      (p4-review--run
       (p4-review--make-session :directory default-directory)
       "describe" '("-s" "123")
       (lambda (_text error-text) (setq reported error-text))))
    (should (string-match-p "Cannot start test process" reported))))

(ert-deftest p4-review-test-diff-errors-remain-visible ()
  (p4-review-test--workspace
    (let ((backend (p4-review-test--fixture directory)))
      (cl-letf (((symbol-function 'p4-review--run) backend)
                ((symbol-function 'p4-review--diff)
                 (lambda (_old _new) (user-error "GNU diff unavailable"))))
        (let ((session (p4-review-open "123" directory)))
          (should-not (p4-review--session-source-buffer session))
          (with-current-buffer (p4-review--session-files-buffer session)
            (should (string-match-p "ERROR: GNU diff unavailable"
                                     (buffer-string)))))))))

(ert-deftest p4-review-test-real-process-cleanup-errors-and-cancellation ()
  ;; Replace only the executable/argv at process creation; exercise the real
  ;; transport and sentinels without contacting Perforce.
  (let ((make-process-function (symbol-function 'make-process)))
    (dolist (scenario '(success failure cancel))
      (let* ((session (p4-review--make-session :directory default-directory))
             (expression
              (pcase scenario
                ('success "(princ \"test output\")")
                ('failure "(progn (message \"test denied\") (kill-emacs 2))")
                ('cancel "(sleep-for 30)")))
             (deadline (+ (float-time) 15))
             process-buffers called result problem)
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest arguments)
                     (setq process-buffers
                           (list (plist-get arguments :buffer)
                                 (plist-get arguments :stderr)))
                     (apply make-process-function
                            (plist-put
                             arguments :command
                             (list (expand-file-name invocation-name invocation-directory)
                                   "--batch" "-Q" "--eval" expression))))))
          (unwind-protect
              (progn
                (p4-review--run
                 session "describe" '("-s" "123")
                 (lambda (text error-text)
                   (setq called t result text problem error-text)))
                (when (eq scenario 'cancel) (p4-review--cancel session))
                (while (and (seq-some #'buffer-live-p process-buffers)
                            (< (float-time) deadline))
                  (accept-process-output nil 0.05))
                (should-not (seq-some #'buffer-live-p process-buffers))
                (should-not (p4-review--session-processes session))
                (pcase scenario
                  ('success
                   (should called)
                   (should (equal result "test output"))
                   (should-not problem))
                  ('failure
                   (should called)
                   (should (string-match-p "test denied" problem)))
                  ('cancel (should-not called))))
            (p4-review--cancel session)
            (dolist (buffer process-buffers)
              (when (buffer-live-p buffer) (kill-buffer buffer)))))))))

(ert-deftest p4-review-test-review-input-and-version-resolution ()
  (should (equal (p4-review--parse-input
                  "https://swarm.invalid/reviews/123#comments")
                 '("123" "https://swarm.invalid")))
  (should-error (p4-review--parse-input "http://swarm.invalid/reviews/123")
                :type 'user-error)
  (let ((submitted (p4-review--parse-review (p4-review-test--review-json) "123"))
        (pending (p4-review--parse-review (p4-review-test--review-json t) "123"))
        (archived (p4-review--parse-review
                   (p4-review-test--review-json t t) "123")))
    (should (equal (plist-get submitted :change) "456"))
    (should-not (plist-get submitted :shelved))
    (should (plist-get pending :shelved))
    (should (equal (plist-get pending :change) "456"))
    (should (equal (plist-get archived :change) "789"))
    (should (= (plist-get submitted :version) 2)))
  (dolist (json '("{}" "{\"data\":42}" "{\"data\":{\"reviews\":[42]}}"
                  "{\"data\":{\"reviews\":[{\"id\":123,\"versions\":[]}]}}"))
    (should-error (p4-review--parse-review json "123") :type 'p4-review-error))
  (should-error (p4-review--parse-review (p4-review-test--review-json) "999")
                :type 'p4-review-error))

(ert-deftest p4-review-test-tickets-are-scoped-to-server-and-user ()
  (let ((text (concat "localhost:other (tester) other-ticket\n"
                      "localhost:fixture (someone) someone-ticket\n"
                      "localhost:fixture (tester) correct-ticket\n")))
    (should (equal (p4-review--ticket
                    text '((userName . "tester") (serverCluster . "fixture")))
                   '("tester" . "correct-ticket")))
    (should-error
     (p4-review--ticket text '((userName . "missing") (serverCluster . "fixture")))
     :type 'p4-review-error))
  (should-error
   (p4-review--run (p4-review--make-session) "property" '("-a" "-n" "x") #'ignore)
   :type 'p4-review-error))

(ert-deftest p4-review-test-resolve-review-number-url-and-pending-version ()
  (dolist (input '("123" "https://swarm.invalid/reviews/123"))
    (dolist (pending '(nil t))
      (p4-review-test--workspace
        (let ((backend (p4-review-test--review-backend directory pending))
              (requests 0))
          (cl-letf (((symbol-function 'p4-review--run) backend)
                    ((symbol-function 'p4-review--process)
                     (lambda (_session _program arguments callback _label input)
                       (cl-incf requests)
                       (should (member "https://swarm.invalid/api/v11/reviews/123"
                                       arguments))
                       (should-not (member "--location" arguments))
                       (should-not (string-match-p "fixture-ticket" input))
                       (should-not (string-match-p
                                    (base64-encode-string "tester:fixture-ticket" t)
                                    (prin1-to-string arguments)))
                       (should (string-match-p
                                (base64-encode-string "tester:fixture-ticket" t) input))
                       (funcall callback
                                (concat (p4-review-test--review-json pending)
                                        "\nP4_REVIEW_HTTP:200")
                                nil))))
            (let ((session (p4-review-open input directory)))
              (should (equal (p4-review--session-review-id session) "123"))
              (should (equal (p4-review--session-change session) "456"))
              (should (= (p4-review--session-review-version session) 2))
              (should (eq (p4-review--session-shelved session) pending))
              (should (= (length (p4-review--session-files session)) 1))
              (p4-review-refresh)
              (should (= requests 2))
              (should (equal (p4-review--session-review-id session) "123")))))))))

(ert-deftest p4-review-test-review-url-cannot-redirect-ticket-to-another-server ()
  (p4-review-test--workspace
    (let ((backend (p4-review-test--review-backend directory)))
      (cl-letf (((symbol-function 'p4-review--run) backend)
                ((symbol-function 'p4-review--process)
                 (lambda (&rest _) (ert-fail "Must not send authentication"))))
        (let ((session
               (p4-review-open "https://other.invalid/reviews/123" directory)))
          (should (string-match-p "does not match"
                                   (p4-review--session-status session))))))))

(ert-deftest p4-review-test-review-http-errors-are-visible ()
  (dolist (status '("401" "404" "302"))
    (p4-review-test--workspace
      (let ((backend (p4-review-test--review-backend directory)))
        (cl-letf (((symbol-function 'p4-review--run) backend)
                  ((symbol-function 'p4-review--process)
                   (lambda (_s _program _args callback _label _input)
                     (funcall callback (concat "{}\nP4_REVIEW_HTTP:" status) nil))))
          (let ((session (p4-review-open "123" directory)))
            (should-not (p4-review--session-files session))
            (should (string-match-p (concat "HTTP " status)
                                     (p4-review--session-status session)))))))))

(ert-deftest p4-review-test-description-normalizes-only-display-line-endings ()
  (with-temp-buffer
    (let* ((description "First\r\nSecond\rThird\n")
           (session (p4-review--make-session
                     :change "123" :description description
                     :files-buffer (current-buffer))))
      (p4-review--render-files session)
      (should (zerop (cl-count 13 (buffer-string))))
      (should (string-match-p "First\nSecond\nThird\n" (buffer-string)))
      (should (equal (p4-review--session-description session) description)))))

(ert-deftest p4-review-test-workspace-location-context-and-ambiguity ()
  (dolist (example
           '(("a\nneedle\nz\n" "a\nneedle\nz\n" 2 (2 . exact))
             ("a\nneedle\nz\n" "extra\nmore\na\nneedle\nz\n" 2 (4 . exact))
             ("extra\na\nneedle\nz\n" "a\nneedle\nz\n" 3 (2 . exact))
             ("a\nnew_call();\nz\n" "a\nold_call();\nz\n" 2 (2 . approximate))
             ("a\nremoved\nz\n" "a\nz\n" 2 nil)
             ("x\n" "y\n" 1 nil)))
    (pcase-let ((`(,review ,workspace ,line ,expected) example))
      (should (equal (p4-review--workspace-location review workspace line)
                     expected))))
  (let* ((review (mapconcat (lambda (n) (format "line %s\n" n))
                           (number-sequence 1 60) ""))
         (workspace (concat "prefix\n"
                            (replace-regexp-in-string
                             "line 20\n" "changed 20\n" review t t))))
    (should (equal (p4-review--workspace-location review workspace 50)
                   '(51 . exact)))))

(ert-deftest p4-review-test-workspace-jump-keeps-review-and-follows-line-column ()
  (dolist (prefix '("" "// extra local line\n"))
    (p4-review-test--workspace
      (let ((backend (p4-review-test--fixture directory)))
        (write-region (concat prefix "int answer = 2;\n")
                      nil "example.cpp" nil 'silent)
        (cl-letf (((symbol-function 'p4-review--run) backend))
          (let* ((session (p4-review-open "123" directory))
                 (source (p4-review--session-source-buffer session))
                 (source-window (p4-review--session-source-window session)))
            (with-selected-window source-window
              (goto-char 5)
              (p4-review-open-workspace-file)
              (should-not (eq (selected-window) source-window))
              (should (eq (window-buffer source-window) source))
              (should (= (window-point source-window) 5))
              (should (= (line-number-at-pos) (if (string-empty-p prefix) 1 2)))
              (should (= (current-column) 4))
              (should (equal buffer-file-name (expand-file-name "example.cpp")))
              (should-not (buffer-modified-p)))))))))

(ert-deftest p4-review-test-workspace-jump-missing-context-does-not-guess ()
  (p4-review-test--workspace
    (let ((backend (p4-review-test--fixture directory)))
      (write-region "unrelated\nworkspace\n" nil "example.cpp" nil 'silent)
      (let ((workspace (find-file-noselect "example.cpp")))
        (with-current-buffer workspace (goto-char 13))
        (cl-letf (((symbol-function 'p4-review--run) backend))
          (let* ((session (p4-review-open "123" directory))
                 (source-window (p4-review--session-source-window session))
                 notice)
            (with-selected-window source-window
              (cl-letf (((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (setq notice (apply #'format format-string args)))))
                (p4-review-open-workspace-file))
              (should (eq (current-buffer) workspace))
              (should (= (point) 13))
              (should (string-match-p "no corresponding code position" notice))
              (should (eq (window-buffer source-window)
                          (p4-review--session-source-buffer session))))))))))

(ert-deftest p4-review-test-local-root-resolves-without-current-client-mapping ()
  (dolist (client-root '(nil t))
    (dolist (unmapped '(nil t))
      (p4-review-test--workspace
        (let* ((client (expand-file-name "client" directory))
               (root (if client-root client directory))
               (path (expand-file-name "gameplay/source/example.cpp" client))
               (backend (p4-review-test--fixture
                         root nil "//depot/stream/client/gameplay/source/example.cpp"
                         unmapped)))
          (make-directory (file-name-directory path) t)
          (write-region "// local prefix\nint answer = 2;\n" nil path nil 'silent)
          ;; An unrelated current client mapping must not override the root.
          (write-region "other stream\n" nil
                        (expand-file-name "example.cpp" root) nil 'silent)
          (cl-letf (((symbol-function 'p4-review--run) backend)
                    ((symbol-function 'read-file-name)
                     (lambda (&rest _) (ert-fail "Unique match must not prompt"))))
            (let* ((session (p4-review-open "123" root))
                   (file (p4-review--current-file session))
                   (source (p4-review--session-source-buffer session))
                   (window (p4-review--session-source-window session)))
              (should (file-equal-p (p4-review--file-local-path file) path))
              (should-not (p4-review--file-mapping-error file))
              (with-selected-window window
                (goto-char 5)
                (p4-review-open-workspace-file)
                (should (file-equal-p buffer-file-name path))
                (should (= (line-number-at-pos) 2))
                (should (= (current-column) 4))
                (should (eq (window-buffer window) source))))))))))

(ert-deftest p4-review-test-ambiguous-local-root-prompts-instead-of-using-client ()
  (p4-review-test--workspace
    (let* ((root (expand-file-name "client" directory))
           (first (expand-file-name "src/example.cpp" root))
           (second (expand-file-name "client/src/example.cpp" root))
           (backend (p4-review-test--fixture
                     root nil "//depot/stream/client/src/example.cpp"))
           prompted)
      (dolist (path (list first second))
        (make-directory (file-name-directory path) t)
        (write-region "int answer = 2;\n" nil path nil 'silent))
      (write-region "other stream\n" nil
                    (expand-file-name "example.cpp" root) nil 'silent)
      (cl-letf (((symbol-function 'p4-review--run) backend)
                ((symbol-function 'completing-read)
                 (lambda (_prompt candidates &rest _)
                   (setq prompted t)
                   (should (= (length candidates) 2))
                   (should (seq-some (lambda (path) (file-equal-p path second))
                                     candidates))
                   second)))
        (let* ((session (p4-review-open "123" root))
               (file (p4-review--current-file session))
               (window (p4-review--session-source-window session)))
          (should-not prompted)
          (should-not (p4-review--file-local-path file))
          (should (string-match-p "Multiple local matches"
                                   (p4-review--file-mapping-error file)))
          (with-selected-window window
            (p4-review-open-workspace-file)
            (should prompted)
            (should (file-equal-p buffer-file-name second))
            (should-not (p4-review--file-mapping-error file))))))))

(ert-deftest p4-review-test-unmapped-source-prompts-and-remembers-selection ()
  (p4-review-test--workspace
    (let* ((path (expand-file-name "manual/chosen.cpp" directory))
           (backend (p4-review-test--fixture directory nil nil t))
           (prompts 0))
      (make-directory (file-name-directory path) t)
      (write-region "int answer = 2;\n" nil path nil 'silent)
      (cl-letf (((symbol-function 'p4-review--run) backend)
                ((symbol-function 'read-file-name)
                 (lambda (_prompt root _default mustmatch &rest _)
                   (cl-incf prompts)
                   (should (file-equal-p root directory))
                   (should mustmatch)
                   path)))
        (let* ((session (p4-review-open "123" directory))
               (file (p4-review--current-file session))
               (source (p4-review--session-source-buffer session))
               (window (p4-review--session-source-window session)))
          (should (= prompts 0))
          (should (string-match-p "C-c C-o selects a file"
                                   (p4-review--file-mapping-error file)))
          (with-selected-window window
            (goto-char 5)
            (p4-review-open-workspace-file)
            (should (file-equal-p buffer-file-name path))
            (should (= (current-column) 4))
            (should (eq (window-buffer window) source))
            (should (file-equal-p (p4-review--choose-local-source session file) path))
            (should (= prompts 1))
            (should-not (p4-review--file-mapping-error file))))))))

(ert-deftest p4-review-test-local-root-never-guesses-by-basename-or-traversal ()
  (p4-review-test--workspace
    (let ((path (expand-file-name "other/example.cpp" directory)))
      (make-directory (file-name-directory path) t)
      (write-region "unrelated\n" nil path nil 'silent)
      (make-directory (expand-file-name "client" directory))
      (should-not
       (p4-review--local-source-candidates
        directory "//depot/stream/client/src/example.cpp"))
      (should-not
       (p4-review--local-source-candidates
        directory "//depot/stream/client/../other/example.cpp")))))

(ert-deftest p4-review-test-manual-source-rejects-directories-without-caching ()
  (p4-review-test--workspace
    (let ((session (p4-review--make-session :directory directory))
          (file (p4-review--make-file :path "//depot/example.cpp")))
      (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) directory)))
        (should-error (p4-review--choose-local-source session file)
                      :type 'user-error)
        (should-not (p4-review--file-local-path file))))))

(provide 'p4-review-tests)
;;; p4-review-tests.el ends here
