;;; p4-review.el --- Read-only Perforce reviews with source navigation -*- lexical-binding: t; -*-

;; Author: kino811
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, vc
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Review a submitted or shelved changelist without syncing or unshelving.
;; Source buffers retain their language modes; overlays show changes without
;; altering the text.  Existing workspace buffers are used only when their text
;; matches the review.  Snapshots are never attached to a workspace LSP server.
;; This is an independent implementation, not a port of p4review.nvim.

;;; Code:

(require 'cl-lib)
(require 'diff)
(require 'diff-mode)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url-parse)
(require 'xref)

(defgroup p4-review nil
  "Read-only Perforce code reviews."
  :group 'tools)

(defcustom p4-review-files-width 30
  "Initial file-list width before balancing the review windows."
  :type 'integer)

(defcustom p4-review-details-width 42
  "Legacy automatic details-panel width.
Opening a review no longer creates a details window."
  :type 'integer)

(defcustom p4-review-refine-max-chars 30000
  "Maximum hunk size for automatic word-level highlighting.
Larger hunks retain line highlighting and margin markers."
  :type 'natnum)

(defconst p4-review--refine-max-blocks 8
  "Maximum number of replacement blocks automatically refined in one hunk.")

(defface p4-review-refine-added
  '((((class color) (background dark))
     :background "#285d36" :foreground "#ffffff" :weight bold)
    (((class color) (background light))
     :background "#a8e5b2" :foreground "#16351d" :weight bold)
    (t :inherit diff-refine-added :weight bold :underline t))
  "Face for changed words on the new side of a review.")

(defface p4-review-refine-removed
  '((((class color) (background dark))
     :background "#78393d" :foreground "#ffffff" :weight bold)
    (((class color) (background light))
     :background "#f4b5b8" :foreground "#421619" :weight bold)
    (t :inherit diff-refine-removed :weight bold :underline t))
  "Face for changed words on the old side of a review.")

(define-error 'p4-review-error "P4 review error")

(cl-defstruct (p4-review--file (:constructor p4-review--make-file))
  path action revision type old new loaded local-path mapping-error hunks)

(cl-defstruct (p4-review--hunk (:constructor p4-review--make-hunk))
  old-start old-count new-start new-count lines)

(cl-defstruct (p4-review--session (:constructor p4-review--make-session))
  change requested-change requested-url review-id review-version
  shelved directory description files (index 0) (generation 0)
  processes closed files-buffer details-buffer snapshot-buffer source-buffer
  source-read-only overlays source-window files-window details-window
  window-configuration status)

(defvar p4-review--active-session nil)
(defvar-local p4-review--session nil)
(defvar p4-review--refinement-cache
  (make-hash-table :test 'eq :weakness 'key)
  "Refined line spans keyed by cached review hunks.")
(defvar-local p4-review--refined-hunks nil
  "Hunks whose refinement has been applied to the current source buffer.")
(defvar-local p4-review--source-margins nil
  "Source window margins saved before displaying review markers.")
(defvar diff-refine-nonmodified)

;; Optional integration with the installed p4.el, without loading it.
(defvar p4-executable)
(defvar p4-modify-args-function)
(declare-function lsp "lsp-mode")
(declare-function lsp-workspaces "lsp-mode")
(declare-function eglot "eglot")
(declare-function eglot-managed-p "eglot")

(defun p4-review--current-session ()
  "Return the current review, or signal a user error."
  (or p4-review--session p4-review--active-session
      (user-error "No active P4 review")))

(defun p4-review--current-file (session)
  "Return SESSION's selected file."
  (nth (p4-review--session-index session)
       (p4-review--session-files session)))

(defun p4-review--cancel (session)
  "Invalidate callbacks and stop SESSION's pending requests."
  (cl-incf (p4-review--session-generation session))
  (dolist (process (p4-review--session-processes session))
    (when (process-live-p process)
      (delete-process process)))
  (setf (p4-review--session-processes session) nil))

(defun p4-review--run (session command arguments callback &optional tagged)
  "Run a read-only P4 COMMAND with ARGUMENTS for SESSION.
Call CALLBACK with (OUTPUT ERROR).  TAGGED requests JSON records.
Callbacks from a closed or superseded request are discarded."
  (unless (or (member command '("describe" "print" "where"))
              (and (equal command "property")
                   (equal arguments '("-l" "-n" "P4.Swarm.URL")))
              (and (equal command "info") (equal arguments '("-s")))
              (and (equal command "tickets") (null arguments)))
    (signal 'p4-review-error (list "Only read-only P4 commands are allowed")))
  (let* ((default-directory (p4-review--session-directory session))
         (program (or (and (boundp 'p4-executable) p4-executable)
                      (executable-find "p4")))
         (args (append (when tagged '("-Mj" "-ztag"))
                       (list command) arguments)))
    (unless program
      (user-error "Cannot find p4; configure p4.el or add p4 to PATH"))
    (when (and (boundp 'p4-modify-args-function) p4-modify-args-function)
      (setq args (funcall p4-modify-args-function args)))
    (p4-review--process
     session program args
     (if (equal command "tickets")
         (lambda (text error-text)
           (funcall callback text (and error-text "Cannot read cached P4 tickets")))
       callback)
     (concat "p4 " command))))

(defun p4-review--process (session program arguments callback label &optional input)
  "Run PROGRAM with ARGUMENTS for SESSION, calling CALLBACK with output/error.
LABEL identifies errors without exposing private INPUT sent over stdin."
  (let ((default-directory (p4-review--session-directory session))
        (generation (p4-review--session-generation session)))
    (let ((output (generate-new-buffer " *p4-review-output*"))
          (errors (generate-new-buffer " *p4-review-errors*")))
      (condition-case error-data
          (let ((process
                 (make-process
                  :name "p4-review" :buffer output :stderr errors
                  :command (cons program arguments) :connection-type 'pipe
                  :noquery t
                  :sentinel
                  (lambda (process _event)
                    (when (memq (process-status process) '(exit signal))
                      (let ((text (with-current-buffer output (buffer-string)))
                            (error-text
                             (with-current-buffer errors (buffer-string))))
                        (unwind-protect
                            (when (and
                                   (not (p4-review--session-closed session))
                                   (= generation
                                      (p4-review--session-generation session)))
                              (funcall callback text
                                       (unless (and
                                                (eq (process-status process) 'exit)
                                                (zerop (process-exit-status process)))
                                         (format "%s failed: %s" label
                                                 (string-trim
                                                  (if (string-empty-p error-text)
                                                      text error-text))))))
                          (setf (p4-review--session-processes session)
                                (delq process
                                      (p4-review--session-processes session)))
                          (kill-buffer output)
                          (kill-buffer errors))))))))
            (push process (p4-review--session-processes session))
            (when input
              (process-send-string process input)
              (process-send-eof process)))
        (file-error
         (kill-buffer output)
         (kill-buffer errors)
         (funcall callback "" (error-message-string error-data)))))))

(defun p4-review--parse-input (input)
  "Return (CHANGE SWARM-BASE) for a positive number or review URL INPUT."
  (setq input (string-trim (format "%s" input)))
  (cond
   ((string-match-p "\\`[1-9][0-9]*\\'" input) (list input nil))
   ((string-match
     "\\`\\(https://[^?#]+\\)/reviews/\\([1-9][0-9]*\\)/?\\(?:[?#].*\\)?\\'"
     input)
    (list (match-string 2 input) (match-string 1 input)))
   (t (user-error "Expected a positive CL/review number or HTTPS Swarm review URL"))))

(defun p4-review--parse-review (text requested)
  "Read the latest review version from JSON TEXT for REQUESTED review."
  (let* ((data (condition-case error-data
                  (json-parse-string text :object-type 'alist :array-type 'list
                                     :false-object :false :null-object nil)
                (json-parse-error
                 (signal 'p4-review-error
                         (list (error-message-string error-data))))))
         (payload (and (listp data) (alist-get 'data data)))
         (reviews (and (listp payload) (alist-get 'reviews payload)))
         (review (and (listp reviews) (car reviews)))
         (versions (and (listp review) (alist-get 'versions review)))
         (version (and (listp versions) (car (last versions))))
         (pending (if (listp version) (alist-get 'pending version :missing) :missing))
         (change (if (eq pending t)
                     (or (alist-get 'archiveChange version)
                         (alist-get 'change version))
                   (and (listp version) (alist-get 'change version)))))
    (unless (and (listp review)
                 (equal (format "%s" (alist-get 'id review)) requested)
                 version (memq pending '(t :false))
                 (string-match-p "\\`[1-9][0-9]*\\'" (format "%s" change)))
      (signal 'p4-review-error (list "Swarm returned incomplete review version data")))
    (list :id requested :version (length versions)
          :change (format "%s" change) :shelved (eq pending t))))

(defun p4-review--ticket (text info)
  "Find the current server and user's cached ticket in TEXT using INFO."
  (let* ((user (alist-get 'userName info))
         (cluster (alist-get 'serverCluster info))
         (address (alist-get 'serverAddress info))
         (servers (delq nil
                        (list address
                              (and (stringp cluster) (not (string-empty-p cluster))
                                   (concat "localhost:" cluster)))))
         ticket)
    (dolist (line (split-string text "\n" t))
      (when (and (string-match
                  "\\`\\([^ \t]+\\)[ \t]+(\\([^)\n]+\\))[ \t]+\\([^ \t\r\n]+\\)[ \t\r]*\\'"
                  line)
                 (member (match-string 1 line) servers)
                 (equal (match-string 2 line) user))
        (setq ticket (match-string 3 line))))
    (unless ticket
      (signal 'p4-review-error
              (list "No cached P4 ticket matches this workspace; run p4 login")))
    (cons user ticket)))

(defun p4-review--swarm-request (session base credentials callback)
  "Read SESSION's review from trusted BASE using CREDENTIALS.
Send authentication over stdin, never argv; do not follow redirects."
  (let* ((program (executable-find "curl"))
         (id (p4-review--session-requested-change session))
         (authorization
          (base64-encode-string
           (encode-coding-string (concat (car credentials) ":" (cdr credentials))
                                 'utf-8) t)))
    (if (not program)
        (funcall callback nil "Swarm lookup requires curl on PATH")
      (p4-review--process
       session program
       (list "--disable" "--config" "-" "--silent" "--show-error"
             "--max-time" "20" "--proto" "=https"
             "--header" "Accept: application/json"
             "--write-out" "\nP4_REVIEW_HTTP:%{http_code}"
             (format "%s/api/v11/reviews/%s" base id))
       (lambda (text error-text)
         (let (review)
           (unless error-text
             (condition-case error-data
                 (if (not (string-match "\nP4_REVIEW_HTTP:\\([0-9]+\\)\\'" text))
                     (signal 'p4-review-error (list "Missing Swarm HTTP status"))
                   (let ((status (match-string 1 text))
                         (body (substring text 0 (match-beginning 0))))
                     (unless (equal status "200")
                       (signal 'p4-review-error
                               (list (format "Swarm review %s: HTTP %s%s"
                                             id status
                                             (if (equal status "404")
                                                 "; pending CL has no shelf or review"
                                               "")))))
                     (setq review (p4-review--parse-review body id))))
               (p4-review-error (setq error-text (error-message-string error-data)))))
           (funcall callback review error-text)))
       "Swarm lookup"
       (format "header = \"Authorization: Basic %s\"\n" authorization)))))

(defun p4-review--resolve-swarm (session)
  "Resolve SESSION's requested review through its workspace's Swarm server."
  (p4-review--status session "Resolving Swarm review...")
  (p4-review--records
   session "property" '("-l" "-n" "P4.Swarm.URL")
   (lambda (records error-text)
     (let* ((value (alist-get 'value (car records)))
            (base (and (stringp value) (string-trim-right value "/+")))
            (url (and base (url-generic-parse-url base))))
       (cond
        (error-text (p4-review--fail session error-text))
        ((not (and url (equal (url-type url) "https") (url-host url)
                   (not (url-user url)) (not (url-password url))
                   (not (string-match-p "[?#\r\n]" base))))
         (p4-review--fail
          session "No valid HTTPS P4.Swarm.URL; pending CL has no shelved files"))
        ((and (p4-review--session-requested-url session)
              (not (equal base (p4-review--session-requested-url session))))
         (p4-review--fail session "Review URL does not match this workspace's P4.Swarm.URL"))
        (t
         (p4-review--records
          session "info" '("-s")
          (lambda (info info-error)
            (if info-error
                (p4-review--fail session info-error)
              (p4-review--run
               session "tickets" nil
               (lambda (text ticket-error)
                 (let (credentials)
                   (unless ticket-error
                     (condition-case error-data
                         (setq credentials (p4-review--ticket text (car info)))
                       (p4-review-error
                        (setq ticket-error (error-message-string error-data)))))
                   (if ticket-error
                       (p4-review--fail session ticket-error)
                     (p4-review--swarm-request
                      session base credentials
                      (lambda (review review-error)
                        (if review-error
                            (p4-review--fail session review-error)
                          (setf (p4-review--session-review-id session)
                                (plist-get review :id)
                                (p4-review--session-review-version session)
                                (plist-get review :version)
                                (p4-review--session-change session)
                                (plist-get review :change)
                                (p4-review--session-shelved session)
                                (plist-get review :shelved))
                          (p4-review--describe-resolved session)))))))))))))))))

(defun p4-review--parse-records (text)
  "Parse tagged P4 JSON TEXT, rejecting server and format errors."
  (let (records)
    (dolist (line (split-string text "\n" t "[ \t\r]+"))
      (let ((record
             (condition-case error-data
                 (json-parse-string line :object-type 'alist :array-type 'array)
               (json-parse-error
                (signal 'p4-review-error
                        (list (format "Invalid P4 JSON: %s"
                                      (error-message-string error-data))))))))
        (unless (listp record)
          (signal 'p4-review-error (list "P4 returned a non-object record")))
        (when (and (assq 'severity record)
                   (not (integerp (alist-get 'severity record))))
          (signal 'p4-review-error (list "P4 returned an invalid severity")))
        (cond
         ((>= (or (alist-get 'severity record) 0) 3)
          (signal 'p4-review-error
                  (list (or (alist-get 'data record) "P4 server error"))))
         ((not (assq 'severity record)) (push record records)))))
    (nreverse records)))

(defun p4-review--records (session command arguments callback)
  "Run tagged COMMAND with ARGUMENTS and pass records to CALLBACK."
  (p4-review--run
   session command arguments
   (lambda (text error-text)
     (let (records)
       (unless error-text
         (condition-case error-data
             (setq records (p4-review--parse-records text))
           (p4-review-error
            (setq error-text (error-message-string error-data)))))
       (funcall callback records error-text)))
   t))

(defun p4-review--files-from-record (record)
  "Build review files from a tagged describe RECORD."
  (let ((index 0) files path)
    (while (setq path (alist-get (intern (format "depotFile%d" index)) record))
      (let ((action (alist-get (intern (format "action%d" index)) record))
            (revision (alist-get (intern (format "rev%d" index)) record))
            (type (alist-get (intern (format "type%d" index)) record)))
        (unless (and (stringp path) (stringp action)
                     (or (and (integerp revision) (>= revision 0))
                         (and (stringp revision)
                              (string-match-p "\\`[0-9]+\\'" revision))))
          (signal 'p4-review-error (list "Incomplete file revision in describe")))
        (push (p4-review--make-file
               :path path :action action :revision
               (if (integerp revision) revision (string-to-number revision))
               :type type)
              files))
      (cl-incf index))
    (nreverse files)))

(defun p4-review--binary-p (file)
  "Return non-nil if FILE is not supported as source text."
  (string-match-p
   "\\`\\(?:u?binary\\|apple\\|resource\\|symlink\\)\\(?:\\+\\|\\'\\)"
   (or (p4-review--file-type file) "")))

(defun p4-review--specs (session file)
  "Return (BASE NEW) filespecs for FILE in SESSION.
An absent side of an addition or deletion is nil."
  (let* ((shelved (p4-review--session-shelved session))
         (revision (p4-review--file-revision file))
         (base (if shelved revision (1- revision)))
         (path (p4-review--file-path file))
         (action (p4-review--file-action file)))
    (list
     (unless (or (< base 1)
                 (member action '("add" "branch" "move/add" "import")))
       (format "%s#%d" path base))
     (unless (member action '("delete" "move/delete" "purge" "archive"))
       (if shelved
           (format "%s@=%s" path (p4-review--session-change session))
         (format "%s#%d" path revision))))))

(defun p4-review--parse-hunks (text)
  "Parse unified diff TEXT into hunks."
  (let (hunks current)
    (dolist (line (split-string text "\n"))
      (cond
       ((string-match
         "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
         line)
        (setq current
              (p4-review--make-hunk
               :old-start (string-to-number (match-string 1 line))
               :old-count (if (match-string 2 line)
                              (string-to-number (match-string 2 line)) 1)
               :new-start (string-to-number (match-string 3 line))
               :new-count (if (match-string 4 line)
                              (string-to-number (match-string 4 line)) 1)))
        (push current hunks))
       ((and current (string-match-p "\\`[-+ \\\\]" line))
        (push line (p4-review--hunk-lines current)))))
    (dolist (hunk hunks)
      (setf (p4-review--hunk-lines hunk)
            (nreverse (p4-review--hunk-lines hunk))))
    (nreverse hunks)))

(defun p4-review--diff (old new)
  "Compute unified hunks for OLD and NEW source text."
  (if (equal old new)
      nil
    (unless (executable-find diff-command)
      (user-error "Cannot find %s; install GNU diff" diff-command))
    (let ((old-buffer (generate-new-buffer " *p4-review-base*"))
          (new-buffer (generate-new-buffer " *p4-review-new*"))
          diff-buffer)
      (unwind-protect
          (progn
            (with-current-buffer old-buffer (insert old))
            (with-current-buffer new-buffer (insert new))
            (setq diff-buffer (diff-no-select old-buffer new-buffer "-U3" t))
            (when-let* ((process (get-buffer-process diff-buffer)))
              (unless (memq (process-exit-status process) '(0 1))
                (signal 'p4-review-error (list "GNU diff failed"))))
            (with-current-buffer diff-buffer
              (or (p4-review--parse-hunks (buffer-string))
                  (signal 'p4-review-error
                          (list "GNU diff did not return a text diff")))))
        (kill-buffer old-buffer)
        (kill-buffer new-buffer)
        (when (buffer-live-p diff-buffer) (kill-buffer diff-buffer))))))

(defun p4-review--line-position (line)
  "Return the buffer position of LINE, clamped to the buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (max 0 (1- line)))
    (point)))

(defun p4-review--hunk-line (hunk)
  "Return the first navigation line of HUNK in the new source."
  (max 1 (+ (p4-review--hunk-new-start hunk)
            (if (zerop (p4-review--hunk-new-count hunk)) 1 0))))

(defun p4-review--word-span (text start end)
  "Expand changed START..END in TEXT to complete identifier fragments."
  (let ((word-p (lambda (index)
                  (string-match-p "[[:alnum:]_]"
                                  (substring text index (1+ index))))))
    (when (and (< start end) (funcall word-p start))
      (while (and (> start 0) (funcall word-p (1- start)))
        (cl-decf start)))
    (when (and (< start end) (funcall word-p (1- end)))
      (while (and (< end (length text)) (funcall word-p end))
        (cl-incf end)))
    (cons start end)))

(defun p4-review--hunk-refinement (hunk)
  "Return cached per-line word spans for HUNK, or `skipped'."
  (or (gethash hunk p4-review--refinement-cache)
      (let* ((lines (p4-review--hunk-lines hunk))
             (body (mapconcat #'identity lines "\n"))
             (blocks (let ((count 0) deleted)
                       (dolist (line lines count)
                         (pcase (aref line 0)
                           (?- (setq deleted t))
                           (?+ (when deleted (cl-incf count))
                               (setq deleted nil))
                           (?\s (setq deleted nil))))))
             (spans (make-vector (length lines) nil)))
        ;; Native refinement launches diff once per replacement block.
        (if (or (> (length body) p4-review-refine-max-chars)
                (> blocks p4-review--refine-max-blocks))
            (progn
              (setq spans 'skipped)
              (message "p4-review: Large or fragmented hunk; keeping line highlighting"))
          (when (and (seq-some (lambda (line) (eq (aref line 0) ?-)) lines)
                     (seq-some (lambda (line) (eq (aref line 0) ?+)) lines))
            (condition-case err
                (with-temp-buffer
                  (insert (format "--- old\n+++ new\n@@ -%d,%d +%d,%d @@\n"
                                  (p4-review--hunk-old-start hunk)
                                  (p4-review--hunk-old-count hunk)
                                  (p4-review--hunk-new-start hunk)
                                  (p4-review--hunk-new-count hunk))
                          body "\n")
                  (let ((diff-refine nil)
                        (diff-refine-nonmodified nil)
                        (runner (symbol-function 'call-process)))
                    (delay-mode-hooks (diff-mode))
                    (goto-char (point-min))
                    (forward-line 2)
                    ;; Built-in refinement does not check diff's exit status.
                    (cl-letf (((symbol-function 'call-process)
                               (lambda (&rest args)
                                 (let ((status (apply runner args)))
                                   (when (and (equal (car args) diff-command)
                                              (not (memq status '(0 1))))
                                     (signal 'p4-review-error
                                             (list "GNU diff refinement failed")))
                                   status))))
                      (diff-refine-hunk)))
                  (goto-char (point-min))
                  (forward-line 3)
                  (cl-loop
                   for entry in lines for index from 0 do
                   (let ((start (point))
                         (text (substring entry 1))
                         ranges)
                     (dolist (item (overlays-in start (line-end-position)))
                       (when (memq (overlay-get item 'face)
                                   '(diff-refine-added diff-refine-removed))
                         (let ((beg (max 0 (- (overlay-start item) start 1)))
                               (end (min (length text)
                                         (- (overlay-end item) start 1))))
                           (when (< beg end)
                             (push (p4-review--word-span text beg end) ranges)))))
                     (let (merged)
                       (dolist (range (sort ranges
                                            (lambda (a b) (< (car a) (car b)))))
                         (if (and merged (<= (car range) (cdar merged)))
                             (setcdr (car merged) (max (cdar merged) (cdr range)))
                           (push range merged)))
                       (aset spans index (nreverse merged))))
                   (forward-line 1)))
              (error
               (signal 'p4-review-error
                       (list (format "Cannot refine review hunk: %s"
                                     (error-message-string err))))))))
        (puthash hunk spans p4-review--refinement-cache))))

(defun p4-review--margin-marker (marker face)
  "Return MARKER displayed in the left margin with FACE."
  (propertize marker 'face face 'display `((margin left-margin) ,marker)))

(defun p4-review--deleted-text (deleted &optional spans)
  "Render DELETED (index . line) entries, optionally applying word SPANS."
  (mapconcat
   (lambda (entry)
     (let ((text (propertize
                  (concat (substring (cdr entry) 1) "\n")
                  'face 'diff-removed)))
       (when (vectorp spans)
         (dolist (range (aref spans (car entry)))
           (put-text-property (car range) (cdr range)
                              'face 'p4-review-refine-removed text)))
       (concat (p4-review--margin-marker "-" 'p4-review-refine-removed) text)))
   deleted ""))

(defun p4-review--decorate (session file)
  "Display FILE's changes with overlays in SESSION's source buffer."
  (setq p4-review--refined-hunks (make-hash-table :test 'eq))
  (dolist (hunk (p4-review--file-hunks file))
    (let ((line (max 1 (p4-review--hunk-new-start hunk)))
          removed)
      (cl-labels
          ((overlay (start end face &optional index deleted)
             (let ((item (make-overlay start end)))
               (overlay-put item 'window (p4-review--session-source-window session))
               (overlay-put item 'p4-review-hunk hunk)
               (overlay-put item 'p4-review-line-index index)
               (overlay-put item 'p4-review-deleted-lines deleted)
               (when face (overlay-put item 'face face))
               (overlay-put
                item 'before-string
                (concat (p4-review--deleted-text deleted)
                        (when index
                          (p4-review--margin-marker "+" 'p4-review-refine-added))))
               (push item (p4-review--session-overlays session))))
           (flush ()
             (when removed
               (overlay (p4-review--line-position line)
                        (p4-review--line-position line) nil nil (nreverse removed))
               (setq removed nil))))
        (cl-loop for entry in (p4-review--hunk-lines hunk) for index from 0 do
          (pcase (aref entry 0)
            (?- (push (cons index entry) removed))
            (?+
                (overlay (p4-review--line-position line)
                         (p4-review--line-position (1+ line)) 'diff-added
                         index (nreverse removed))
                (setq removed nil)
                (cl-incf line))
            (?\s (flush) (cl-incf line))))
        (flush)))))

(defun p4-review--refine-displayed-hunk (session hunk)
  "Apply HUNK's cached word spans to SESSION without modifying source text."
  (unless (gethash hunk p4-review--refined-hunks)
    (condition-case err
        (let ((spans (p4-review--hunk-refinement hunk)))
          (when (vectorp spans)
            (dolist (item (p4-review--session-overlays session))
              (when (eq (overlay-get item 'p4-review-hunk) hunk)
                (let ((index (overlay-get item 'p4-review-line-index))
                      (deleted (overlay-get item 'p4-review-deleted-lines)))
                  (when deleted
                    (overlay-put
                     item 'before-string
                     (concat (p4-review--deleted-text deleted spans)
                             (when index
                               (p4-review--margin-marker
                                "+" 'p4-review-refine-added)))))
                  (when index
                    (dolist (range (aref spans index))
                      (let ((fine (make-overlay
                                   (+ (overlay-start item) (car range))
                                   (+ (overlay-start item) (cdr range)))))
                        (overlay-put fine 'window
                                     (p4-review--session-source-window session))
                        (overlay-put fine 'face 'p4-review-refine-added)
                        (overlay-put fine 'priority 100)
                        (push fine (p4-review--session-overlays session)))))))))
          (puthash hunk t p4-review--refined-hunks))
      (p4-review-error
       (puthash hunk t p4-review--refined-hunks)
       (p4-review--fail session (error-message-string err))))))

(defun p4-review--refine-visible-hunks (&optional window start)
  "Refine visible review hunks in WINDOW, optionally beginning at START."
  (when-let* ((session p4-review--session)
              (source-window (p4-review--session-source-window session))
              ((window-live-p source-window))
              ((or (not window) (eq window source-window)))
              ((eq (window-buffer source-window) (current-buffer)))
              (file (p4-review--current-file session))
              ((hash-table-p p4-review--refined-hunks)))
    (let ((first (line-number-at-pos (or start (window-start source-window))))
          (last (line-number-at-pos (or (window-end source-window t)
                                        (window-point source-window)))))
      (dolist (hunk (p4-review--file-hunks file))
        (let ((line (p4-review--hunk-line hunk)))
          (when (and (<= line last)
                     (>= (+ line (p4-review--hunk-new-count hunk)) first))
            (p4-review--refine-displayed-hunk session hunk)))))))

(defun p4-review--release-source (session)
  "Remove SESSION's overlays and restore its previous source buffer."
  (mapc #'delete-overlay (p4-review--session-overlays session))
  (setf (p4-review--session-overlays session) nil)
  (when (buffer-live-p (p4-review--session-source-buffer session))
    (with-current-buffer (p4-review--session-source-buffer session)
      (p4-review-source-mode -1)
      (when (and p4-review--source-margins
                 (window-live-p (p4-review--session-source-window session)))
        (set-window-margins (p4-review--session-source-window session)
                            (car p4-review--source-margins)
                            (cdr p4-review--source-margins)))
      (setq p4-review--source-margins nil
            p4-review--refined-hunks nil)
      (remove-hook 'kill-buffer-hook #'p4-review--buffer-killed t)
      (setq buffer-read-only (p4-review--session-source-read-only session))
      (setq p4-review--session nil)))
  (when (buffer-live-p (p4-review--session-snapshot-buffer session))
    (kill-buffer (p4-review--session-snapshot-buffer session)))
  (setf (p4-review--session-source-buffer session) nil
        (p4-review--session-snapshot-buffer session) nil))

(defun p4-review--buffer-killed ()
  "Close the review when one of its buffers is killed directly."
  (when (and p4-review--session
             (not (p4-review--session-closed p4-review--session)))
    (let ((session p4-review--session)
          (buffer (current-buffer)))
      (when (eq buffer (p4-review--session-files-buffer session))
        (setf (p4-review--session-files-buffer session) nil))
      (when (eq buffer (p4-review--session-details-buffer session))
        (setf (p4-review--session-details-buffer session) nil))
      (when (eq buffer (p4-review--session-source-buffer session))
        (setf (p4-review--session-source-buffer session) nil))
      (when (eq buffer (p4-review--session-snapshot-buffer session))
        (setf (p4-review--session-snapshot-buffer session) nil))
      (p4-review-quit))))

(defun p4-review--buffer-matches-p (buffer file)
  "Return non-nil if BUFFER's accessible text matches the complete FILE."
  (and file (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (not (buffer-narrowed-p))
              (equal (buffer-substring-no-properties (point-min) (point-max))
                     (p4-review--file-new file))))))

(defun p4-review--local-source-candidates (directory depot-path)
  "Find existing sources under DIRECTORY using anchored DEPOT-PATH suffixes.
An anchor is DIRECTORY's name or one of its immediate subdirectories.
Never search recursively by basename or discard unmatched intermediate paths."
  (let* ((root (file-name-as-directory (expand-file-name directory)))
         (root-name (file-name-nondirectory (directory-file-name root)))
         (ignore-case (file-name-case-insensitive-p root))
         (parts (split-string depot-path "/" t))
         (tail parts)
         candidates)
    (unless (seq-some (lambda (part) (member part '("." ".."))) parts)
      (while (cdr tail)
        (let ((part (car tail)))
          (dolist (relative
                   (list
                    (when (eq t (compare-strings
                                 part nil nil root-name nil nil ignore-case))
                      (cdr tail))
                    (when (file-directory-p (expand-file-name part root))
                      tail)))
            (when relative
              (let ((path (expand-file-name (string-join relative "/") root)))
                (when (and (file-regular-p path)
                           (file-in-directory-p path root))
                  (cl-pushnew path candidates :test #'file-equal-p))))))
        (setq tail (cdr tail))))
    (nreverse candidates)))

(defun p4-review--choose-local-source (session file)
  "Resolve FILE for SESSION, asking when local mapping is absent or ambiguous."
  (let* ((root (p4-review--session-directory session))
         (cached (p4-review--file-local-path file))
         (candidates (unless (and cached (file-regular-p cached))
                       (p4-review--local-source-candidates
                        root (p4-review--file-path file))))
         (path
          (cond
           ((and cached (file-regular-p cached)) cached)
           ((length= candidates 1) (car candidates))
           (candidates
            (completing-read "Local source (multiple matches): "
                             candidates nil t))
           (t (read-file-name "Local source file (no automatic match): "
                              root nil t)))))
    (unless (file-regular-p path)
      (user-error "Select an existing local source file"))
    (setq path (expand-file-name path))
    (setf (p4-review--file-local-path file) path
          (p4-review--file-mapping-error file) nil)
    path))

(defun p4-review--matching-buffer (file)
  "Return a workspace buffer only if its current text matches FILE.
Never revert or replace a user's existing buffer."
  (let ((path (p4-review--file-local-path file)))
    (when (and path (file-regular-p path))
      (let ((buffer
             (or (get-file-buffer path)
                 (when (with-temp-buffer
                         (insert-file-contents path)
                         (equal (buffer-string) (p4-review--file-new file)))
                   (find-file-noselect path)))))
        ;; File hooks may change text after the disk comparison.
        (when (p4-review--buffer-matches-p buffer file) buffer)))))

(defun p4-review--snapshot (session file)
  "Create an unvisited, language-mode snapshot of FILE for SESSION."
  (let ((buffer
         (generate-new-buffer
          (format "*P4 %s: %s*" (p4-review--session-change session)
                  (file-name-nondirectory (p4-review--file-path file))))))
    (with-current-buffer buffer
      (insert (if (p4-review--binary-p file)
                  "[Binary or special file: source preview unavailable]\n"
                (p4-review--file-new file)))
      ;; A depot path is not a filesystem path (it resembles UNC on Windows).
      (let ((buffer-file-name
             (file-name-nondirectory (p4-review--file-path file)))
            (enable-dir-local-variables nil))
        (delay-mode-hooks (set-auto-mode)))
      (setq default-directory (p4-review--session-directory session))
      (setq buffer-undo-list t)
      (set-buffer-modified-p nil))
    (setf (p4-review--session-snapshot-buffer session) buffer)
    buffer))

(defun p4-review--source-label (session)
  "Describe the accuracy and LSP status of SESSION's source."
  (cond
   ((not (buffer-live-p (p4-review--session-source-buffer session))) "Loading")
   ((eq (p4-review--session-source-buffer session)
        (p4-review--session-snapshot-buffer session))
    "Review snapshot; workspace LSP is NOT attached")
   (t
    (with-current-buffer (p4-review--session-source-buffer session)
      (concat "Workspace text matches review; "
              (cond
               ((and (bound-and-true-p lsp-mode)
                     (fboundp 'lsp-workspaces) (lsp-workspaces)) "LSP connected")
               ((and (fboundp 'eglot-managed-p) (eglot-managed-p))
                "Eglot connected")
               (t "start LSP explicitly with C-c C-l")))))))

(defun p4-review--title (session)
  "Describe SESSION's requested review and resolved changelist."
  (concat
   (when (p4-review--session-review-id session)
     (format "Review %s v%s -> "
             (p4-review--session-review-id session)
             (p4-review--session-review-version session)))
   (format "CL %s" (p4-review--session-change session))
   (when (p4-review--session-description session)
     (if (p4-review--session-shelved session) " (shelved)" " (submitted)"))))

(defun p4-review--render-files (session)
  "Render SESSION's file list."
  (with-current-buffer (p4-review--session-files-buffer session)
    (let ((inhibit-read-only t)
          selected-position)
      (erase-buffer)
      (insert (format "%s\n%s\n\n"
                      (p4-review--title session)
                      (or (p4-review--session-status session) "")))
      (insert (replace-regexp-in-string
               "\r\n?" "\n" (or (p4-review--session-description session) "") t t)
              "\n\n")
      (cl-loop for file in (p4-review--session-files session)
               for index from 0
               do (let ((start (point)))
                    (when (= index (p4-review--session-index session))
                      (setq selected-position start))
                    (insert (format "%s %-11s %s\n"
                                    (if (= index (p4-review--session-index session))
                                        ">" " ")
                                    (p4-review--file-action file)
                                    (file-name-nondirectory
                                     (p4-review--file-path file))))
                    (add-text-properties start (point)
                                         (list 'p4-review-file-index index
                                         'mouse-face 'highlight
                                         'rear-nonsticky t))))
      (insert "\nRET: select  n/p: file\n]/[: hunk  g: refresh\nD: details  q: close\n")
      (when selected-position (goto-char selected-position)))))

(defun p4-review--render-details (session)
  "Render version and source context for SESSION."
  (with-current-buffer (p4-review--session-details-buffer session)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (format "%s\n%s\n\n"
                      (p4-review--title session)
                      (or (p4-review--session-status session) "")))
      (when-let* ((file (p4-review--current-file session)))
        (insert (p4-review--file-path file) "\n\n"
                (p4-review--source-label session) "\n\n")
        (when (p4-review--file-mapping-error file)
          (insert "Workspace mapping unavailable:\n"
                  (p4-review--file-mapping-error file) "\n\n"))
        (let ((specs (p4-review--specs session file)))
          (insert (format "Base: %s\nNew:  %s\n\n"
                          (or (car specs) "(empty)") (or (cadr specs) "(empty)"))))
        (insert "C-c C-n / C-p: hunk\nC-c C-f / C-b: file\n"
                "M-.: definition  M-?: references\nM-,: return from xref\n"
                "C-c C-o: open workspace file\nC-c C-l: start language server\n"
                "C-c C-d: details  C-c C-q: close\n\n"
                "Snapshots never impersonate a workspace LSP document.\n"
                "Workspace includes and reference indexes may differ from the review.\n"
                "Shelves are read live; refresh after reshelving.\n"
                "Swarm comments and AI reviews are not implemented yet.\n")))))

(defun p4-review--status (session text)
  "Display SESSION status TEXT."
  (setf (p4-review--session-status session) text)
  (p4-review--render-files session)
  (p4-review--render-details session))

(defun p4-review--fail (session error-text)
  "Report ERROR-TEXT visibly for SESSION."
  (p4-review--status session (concat "ERROR: " error-text))
  (message "p4-review: %s" error-text))

(defun p4-review--goto-hunk (session hunk)
  "Move SESSION's source window to HUNK."
  (let ((window (p4-review--session-source-window session)))
    (when (window-live-p window)
      (with-current-buffer (p4-review--session-source-buffer session)
        (p4-review--refine-displayed-hunk session hunk)
        (let ((position (p4-review--line-position (p4-review--hunk-line hunk))))
          (set-window-point window position)
          (when (eq window (selected-window)) (goto-char position)))))))

(defun p4-review--show-file (session file &optional jump)
  "Display FILE's source and overlays for SESSION.
JUMP is `first' or `last' when crossing a file boundary by hunk."
  (let ((buffer (or (and (not (p4-review--binary-p file))
                         (p4-review--matching-buffer file))
                    (p4-review--snapshot session file))))
    (setf (p4-review--session-source-buffer session) buffer)
    (with-current-buffer buffer
      (setf (p4-review--session-source-read-only session) buffer-read-only)
      (setq buffer-read-only t)
      (p4-review-source-mode 1)
      (setq p4-review--session session)
      (add-hook 'kill-buffer-hook #'p4-review--buffer-killed nil t)
      (p4-review--decorate session file))
    (when (window-live-p (p4-review--session-source-window session))
      (let ((window (p4-review--session-source-window session)))
        (with-current-buffer buffer
          (setq p4-review--source-margins (window-margins window))
          (set-window-buffer window buffer)
          (set-window-margins window
                              (max 2 (or (car p4-review--source-margins) 0))
                              (cdr p4-review--source-margins)))))
    (p4-review--status
     session (if (eq buffer (p4-review--session-snapshot-buffer session))
                 "Read-only SNAPSHOT (no LSP)"
               "Read-only WORKSPACE match"))
    (let ((hunks (p4-review--file-hunks file)))
      (if hunks
          (p4-review--goto-hunk session (if (eq jump 'last) (car (last hunks))
                                        (car hunks)))
        (when jump
          (let ((next (+ (p4-review--session-index session)
                         (if (eq jump 'last) -1 1))))
            (if (and (>= next 0) (< next (length (p4-review--session-files session))))
                (p4-review--select-file session next jump)
              (message "p4-review: No more hunks"))))))))

(defun p4-review--load-file (session file &optional jump)
  "Load FILE's review contents and mapping for SESSION, then display it."
  (if (p4-review--file-loaded file)
      (p4-review--show-file session file jump)
    (let* ((specs (p4-review--specs session file))
           (sides (list (cons 'old (car specs)) (cons 'new (cadr specs)))))
      (cl-labels
          ((fetch (remaining)
             (if (null remaining)
                 (condition-case error-data
                     (progn
                       (setf (p4-review--file-hunks file)
                             (p4-review--diff (p4-review--file-old file)
                                              (p4-review--file-new file)))
                       (p4-review--records
                        session "where" (list (p4-review--file-path file))
                        (lambda (records error-text)
                          (let* ((record (seq-find
                                          (lambda (entry)
                                            (and (not (assq 'unmap entry))
                                                 (stringp (alist-get 'path entry))))
                                          records))
                                 (candidates
                                  (p4-review--local-source-candidates
                                   (p4-review--session-directory session)
                                   (p4-review--file-path file)))
                                 (ambiguous (length> candidates 1))
                                 (path (if candidates
                                           (unless ambiguous (car candidates))
                                         (alist-get 'path record))))
                            (setf (p4-review--file-local-path file)
                                  path
                                  (p4-review--file-mapping-error file)
                                  (cond
                                   (ambiguous
                                    "Multiple local matches; C-c C-o selects a file")
                                   ((and path (file-regular-p path)) nil)
                                   (t (concat
                                       "No local source found; C-c C-o selects a file"
                                       (when error-text
                                         (concat "\nP4 mapping: " error-text)))))
                                  (p4-review--file-loaded file) t))
                          (p4-review--show-file session file jump))))
                   ((p4-review-error user-error)
                    (p4-review--fail session (error-message-string error-data))))
               (let* ((side (caar remaining))
                      (spec (cdar remaining))
                      (save-side
                       (lambda (text)
                         (if (eq side 'old)
                             (setf (p4-review--file-old file) text)
                           (setf (p4-review--file-new file) text))
                         (fetch (cdr remaining)))))
                 (if (or (not spec) (p4-review--binary-p file))
                     (funcall save-side "")
                   (p4-review--run
                    session "print" (list "-q" spec)
                    (lambda (text error-text)
                      (if error-text (p4-review--fail session error-text)
                        (funcall save-side text)))))))))
        (fetch sides)))))

(defun p4-review--select-file (session index &optional jump)
  "Select file INDEX in SESSION, optionally positioning at JUMP."
  (unless (and (>= index 0) (< index (length (p4-review--session-files session))))
    (user-error "No more files"))
  (p4-review--cancel session)
  (p4-review--release-source session)
  (setf (p4-review--session-index session) index)
  (p4-review--status session "Loading file...")
  (p4-review--load-file session (p4-review--current-file session) jump))

(defun p4-review-select-file ()
  "Open the review file at point."
  (interactive)
  (let ((index (get-text-property (line-beginning-position)
                                  'p4-review-file-index)))
    (unless (integerp index) (user-error "No review file on this line"))
    (p4-review--select-file (p4-review--current-session) index)))

(defun p4-review-next-file ()
  "Open the next review file."
  (interactive)
  (let ((session (p4-review--current-session)))
    (p4-review--select-file session (1+ (p4-review--session-index session)))))

(defun p4-review-previous-file ()
  "Open the previous review file."
  (interactive)
  (let ((session (p4-review--current-session)))
    (p4-review--select-file session (1- (p4-review--session-index session)))))

(defun p4-review--move-hunk (direction)
  "Move to a hunk in DIRECTION, continuing across files."
  (let* ((session (p4-review--current-session))
         (file (p4-review--current-file session))
         (buffer (p4-review--session-source-buffer session)))
    (unless (and file (buffer-live-p buffer))
      (user-error "Wait for the review source to load"))
    (with-current-buffer buffer
      (let* ((window (p4-review--session-source-window session))
             (position (if (window-live-p window) (window-point window) (point)))
             (hunks (p4-review--file-hunks file))
             (target (seq-find
                      (lambda (hunk)
                        (let ((location (p4-review--line-position
                                         (p4-review--hunk-line hunk))))
                          (if (> direction 0) (> location position)
                            (< location position))))
                      (if (> direction 0) hunks (reverse hunks)))))
        (if target
            (p4-review--goto-hunk session target)
          (let ((next (+ (p4-review--session-index session) direction)))
            (unless (and (>= next 0) (< next (length (p4-review--session-files session))))
              (user-error "No more hunks"))
            (p4-review--select-file session next
                                    (if (> direction 0) 'first 'last))))))))

(defun p4-review-next-hunk ()
  "Go to the next hunk, continuing into the next file."
  (interactive)
  (p4-review--move-hunk 1))

(defun p4-review-previous-hunk ()
  "Go to the previous hunk, continuing into the previous file."
  (interactive)
  (p4-review--move-hunk -1))

(defun p4-review--require-workspace-source ()
  "Reject LSP operations on unvisited review snapshots."
  (let ((session (p4-review--current-session)))
    (unless (and (eq (current-buffer) (p4-review--session-source-buffer session))
                 (not (eq (current-buffer)
                          (p4-review--session-snapshot-buffer session))))
      (user-error "Review snapshot has no workspace LSP; C-c C-o opens workspace context"))
    (unless (p4-review--buffer-matches-p
             (current-buffer) (p4-review--current-file session))
      (user-error "Source no longer matches the review; refresh or reopen it"))))

(defun p4-review-find-definitions ()
  "Find definitions in a matching workspace source buffer."
  (interactive)
  (p4-review--require-workspace-source)
  (call-interactively #'xref-find-definitions))

(defun p4-review-find-references ()
  "Find references in a matching workspace source buffer."
  (interactive)
  (p4-review--require-workspace-source)
  (call-interactively #'xref-find-references))

(defun p4-review-start-lsp ()
  "Explicitly start the configured language server on matching source."
  (interactive)
  (p4-review--require-workspace-source)
  (cond
   ((fboundp 'lsp) (call-interactively #'lsp))
   ((fboundp 'eglot) (call-interactively #'eglot))
   (t (user-error "Load lsp-mode or Eglot and configure a language server first")))
  (p4-review--render-details (p4-review--current-session)))

(defun p4-review--workspace-location (review workspace line)
  "Map LINE in REVIEW text to WORKSPACE text.
Return (LINE . exact), (LINE . approximate), or nil if no correspondence
can be established.  Replaced lines need equal-sized blocks anchored by
unchanged context to qualify as approximate."
  (catch 'location
    (let ((offset 0))
      (dolist (hunk (p4-review--diff review workspace))
        (when (< line (p4-review--hunk-old-start hunk))
          (throw 'location (cons (+ line offset) 'exact)))
        (let ((old (p4-review--hunk-old-start hunk))
              (new (p4-review--hunk-new-start hunk))
              (entries (p4-review--hunk-lines hunk))
              (context (seq-some
                        (lambda (entry) (eq (aref entry 0) ?\s))
                        (p4-review--hunk-lines hunk))))
          (while entries
            (if (eq (aref (car entries) 0) ?\s)
                (progn
                  (when (= line old)
                    (throw 'location (cons new 'exact)))
                  (cl-incf old)
                  (cl-incf new)
                  (setq entries (cdr entries)))
              (let ((old-start old) (new-start new))
                (while (and entries (not (eq (aref (car entries) 0) ?\s)))
                  (pcase (aref (car entries) 0)
                    (?- (cl-incf old))
                    (?+ (cl-incf new)))
                  (setq entries (cdr entries)))
                (when (and (>= line old-start) (< line old))
                  (throw 'location
                         (when (and context (> new new-start)
                                    (= (- old old-start) (- new new-start)))
                           (cons (+ new-start (- line old-start))
                                 'approximate))))))))
        (cl-incf offset (- (p4-review--hunk-new-count hunk)
                          (p4-review--hunk-old-count hunk))))
      (cons (+ line offset) 'exact))))

(defun p4-review-open-workspace-file ()
  "Open local source separately from the review snapshot.
Use the selected source root or P4 mapping, asking if neither resolves a file.
Follow the review cursor using diff context, preserving the review window.
Report approximate or unavailable positions explicitly.  LSP results still
describe the workspace, not necessarily the review revision."
  (interactive)
  (let* ((session (p4-review--current-session))
         (file (p4-review--current-file session))
         (source (p4-review--session-source-buffer session))
         (source-window (p4-review--session-source-window session)))
    (unless file (user-error "No review file is selected"))
    (unless (and (buffer-live-p source) (window-live-p source-window)
                 (eq (window-buffer source-window) source))
      (user-error "Resume the review source before opening workspace context"))
    (let* ((path (p4-review--choose-local-source session file))
           (position (if (eq (current-buffer) source) (point)
                       (window-point source-window)))
           (coordinates
            (with-current-buffer source
              (save-restriction
                (widen)
                (save-excursion
                  (goto-char position)
                  (cons (line-number-at-pos) (current-column))))))
           (buffer (find-file-noselect path))
           (location
            (with-current-buffer buffer
              (save-restriction
                (widen)
                (p4-review--workspace-location
                 (p4-review--file-new file)
                 (buffer-substring-no-properties (point-min) (point-max))
                 (car coordinates)))))
           (target (and location
                        (with-current-buffer buffer
                          (save-restriction
                            (widen)
                            (save-excursion
                              (goto-char (p4-review--line-position (car location)))
                              (when (eq (cdr location) 'exact)
                                (move-to-column (cdr coordinates)))
                              (point)))))))
      (when (and target
                 (with-current-buffer buffer
                   (or (< target (point-min)) (> target (point-max)))))
        (user-error "Corresponding location is outside the narrowed workspace buffer"))
      (let ((window
             (with-selected-window source-window
               (display-buffer
                buffer '((display-buffer-reuse-window display-buffer-pop-up-window)
                         (inhibit-same-window . t))))))
        (unless (and (window-live-p window) (not (eq window source-window)))
          (user-error "Cannot display workspace source without replacing the review"))
        (select-window window)
        (when target (goto-char target))
        (set-window-point source-window position)
        (p4-review--render-details session)
        (message
         (concat
          (pcase (cdr location)
            ('exact "Workspace: corresponding code position")
            ('approximate "Workspace: approximate position; this line differs from review")
            (_ "Workspace: no corresponding code position found; cursor left unchanged"))
          "; LSP uses local code. M-x p4-review-resume returns to review"))))))

(defun p4-review-show-details ()
  "Show review version and source context details."
  (interactive)
  (let ((session (p4-review--current-session)))
    (p4-review--render-details session)
    (display-buffer (p4-review--session-details-buffer session))))

(defun p4-review-resume ()
  "Return to the active review's source window."
  (interactive)
  (let ((session (p4-review--current-session)))
    (unless (and (window-live-p (p4-review--session-source-window session))
                 (buffer-live-p (p4-review--session-source-buffer session)))
      (user-error "Review source is not currently displayed"))
    (select-window (p4-review--session-source-window session))
    (switch-to-buffer (p4-review--session-source-buffer session))))

(defun p4-review--accept-description (session record)
  "Load the files from the resolved describe RECORD into SESSION."
  (condition-case error-data
      (progn
        (unless record
          (signal 'p4-review-error (list "No changelist was returned")))
        (unless (equal (format "%s" (alist-get 'change record))
                       (p4-review--session-change session))
          (signal 'p4-review-error (list "P4 returned a different changelist")))
        (setf (p4-review--session-description session) (alist-get 'desc record)
              (p4-review--session-files session)
              (p4-review--files-from-record record))
        (if (p4-review--session-files session)
            (p4-review--select-file session 0)
          (p4-review--fail session "The resolved changelist has no reviewable files")))
    (p4-review-error
     (p4-review--fail session (error-message-string error-data)))))

(defun p4-review--describe-resolved (session)
  "Read SESSION's resolved review version, without resolving recursively."
  (p4-review--records
   session "describe"
   (append '("-s") (when (p4-review--session-shelved session) '("-S"))
           (list (p4-review--session-change session)))
   (lambda (records error-text)
     (cond
      (error-text (p4-review--fail session error-text))
      ((and (not (p4-review--session-shelved session))
            (not (equal (alist-get 'status (car records)) "submitted")))
       (p4-review--fail session "Resolved review CL is no longer submitted; refresh"))
      (t (p4-review--accept-description session (car records)))))))

(defun p4-review--resolve-number (session)
  "Detect submitted, shelved, or Swarm-backed numeric input for SESSION."
  (p4-review--records
   session "describe" (list "-s" (p4-review--session-requested-change session))
   (lambda (records error-text)
     (let ((record (car records)))
       (cond
        (error-text (p4-review--fail session error-text))
        ((equal (alist-get 'status record) "submitted")
         (p4-review--accept-description session record))
        ((equal (alist-get 'status record) "pending")
         (p4-review--records
          session "describe" (list "-s" "-S" (p4-review--session-change session))
          (lambda (shelf-records shelf-error)
            (cond
             (shelf-error (p4-review--fail session shelf-error))
             ((assq 'depotFile0 (car shelf-records))
              (setf (p4-review--session-shelved session) t)
              (p4-review--accept-description session (car shelf-records)))
             (t (p4-review--resolve-swarm session))))))
        (t (p4-review--fail session "No valid submitted or pending changelist returned")))))))

(defun p4-review-refresh ()
  "Resolve the original input again and reload its latest review version."
  (interactive)
  (let ((session (p4-review--current-session)))
    (p4-review--cancel session)
    (p4-review--release-source session)
    (setf (p4-review--session-files session) nil
          (p4-review--session-description session) nil
          (p4-review--session-review-id session) nil
          (p4-review--session-review-version session) nil
          (p4-review--session-change session)
          (p4-review--session-requested-change session)
          (p4-review--session-shelved session) nil)
    (p4-review--status session "Detecting changelist/review...")
    (if (p4-review--session-requested-url session)
        (p4-review--resolve-swarm session)
      (p4-review--resolve-number session))))

(defun p4-review-quit ()
  "Close the review, restore source state and the previous window layout."
  (interactive)
  (let ((session (p4-review--current-session)))
    (setf (p4-review--session-closed session) t)
    (p4-review--cancel session)
    (p4-review--release-source session)
    (when (window-configuration-p (p4-review--session-window-configuration session))
      (set-window-configuration (p4-review--session-window-configuration session)))
    (dolist (buffer (list (p4-review--session-files-buffer session)
                          (p4-review--session-details-buffer session)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))
    (setq p4-review--active-session nil)))

(defvar p4-review-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'p4-review-select-file)
    (define-key map (kbd "n") #'p4-review-next-file)
    (define-key map (kbd "p") #'p4-review-previous-file)
    (define-key map (kbd "]") #'p4-review-next-hunk)
    (define-key map (kbd "[") #'p4-review-previous-hunk)
    (define-key map (kbd "D") #'p4-review-show-details)
    (define-key map (kbd "g") #'p4-review-refresh)
    (define-key map (kbd "q") #'p4-review-quit)
    map))

(define-derived-mode p4-review-mode special-mode "P4-Review"
  "Mode for the read-only review file list and context panels."
  (setq-local truncate-lines t))

(defvar p4-review-source-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") #'p4-review-next-hunk)
    (define-key map (kbd "C-c C-p") #'p4-review-previous-hunk)
    (define-key map (kbd "C-c C-f") #'p4-review-next-file)
    (define-key map (kbd "C-c C-b") #'p4-review-previous-file)
    (define-key map (kbd "C-c C-o") #'p4-review-open-workspace-file)
    (define-key map (kbd "C-c C-l") #'p4-review-start-lsp)
    (define-key map (kbd "C-c C-d") #'p4-review-show-details)
    (define-key map (kbd "C-c C-q") #'p4-review-quit)
    (define-key map (kbd "M-.") #'p4-review-find-definitions)
    (define-key map (kbd "M-?") #'p4-review-find-references)
    map))

(define-minor-mode p4-review-source-mode
  "Read-only review navigation without replacing the source major mode."
  :lighter " P4-Review"
  :keymap p4-review-source-mode-map
  (if p4-review-source-mode
      (progn
        (add-hook 'post-command-hook #'p4-review--refine-visible-hunks nil t)
        (add-hook 'window-scroll-functions #'p4-review--refine-visible-hunks nil t))
    (remove-hook 'post-command-hook #'p4-review--refine-visible-hunks t)
    (remove-hook 'window-scroll-functions #'p4-review--refine-visible-hunks t)))

;;;###autoload
(defun p4-review-open (input &optional directory)
  "Open a CL/review number or HTTPS Swarm review URL INPUT read-only.
Resolve submitted and shelved contents automatically.  DIRECTORY chooses
the local source root and P4 command context, defaulting to `default-directory'.
The source root may be a workspace root or its client subdirectory.
Only one review is active at a time.  No language server is started
automatically, and no files are synced, unshelved, edited or saved."
  (interactive
   (list (read-string "CL/review number or Swarm URL: ")
         (read-directory-name
          "Local source root / P4 directory: "
          (if p4-review--active-session
              (p4-review--session-directory p4-review--active-session)
            default-directory)
          nil t)))
  (let* ((parsed (p4-review--parse-input input))
         (change (car parsed))
         (requested-url (cadr parsed)))
    (setq directory (file-name-as-directory
                     (expand-file-name (or directory default-directory))))
    (unless (file-directory-p directory)
      (user-error "Workspace directory does not exist"))
    (unless (or (and (boundp 'p4-executable) p4-executable)
                (executable-find "p4"))
      (user-error "Cannot find p4; configure p4.el or add p4 to PATH"))
    (when p4-review--active-session (p4-review-quit))
    (let* ((session (p4-review--make-session
                     :change change :requested-change change
                     :requested-url requested-url :directory directory
                     :window-configuration (current-window-configuration)
                     :files-buffer (generate-new-buffer "*P4 Review Files*")
                     :details-buffer (generate-new-buffer "*P4 Review Details*")))
           (source-window (selected-window))
           opened)
      (setq p4-review--active-session session)
      (unwind-protect
          (progn
            (dolist (buffer (list (p4-review--session-files-buffer session)
                                  (p4-review--session-details-buffer session)))
              (with-current-buffer buffer
                (p4-review-mode)
                (setq p4-review--session session)
                (add-hook 'kill-buffer-hook #'p4-review--buffer-killed nil t)))
            (delete-other-windows source-window)
            (setf (p4-review--session-source-window session) source-window
                  (p4-review--session-files-window session)
                  (split-window source-window
                                (- (min p4-review-files-width
                                        (max window-min-width
                                             (/ (window-total-width source-window) 3))))
                                'left))
            (set-window-buffer (p4-review--session-files-window session)
                               (p4-review--session-files-buffer session))
            (balance-windows)
            (select-window (p4-review--session-files-window session))
            (p4-review-refresh)
            (setq opened t)
            session)
        (unless opened (p4-review-quit))))))

(provide 'p4-review)
;;; p4-review.el ends here
