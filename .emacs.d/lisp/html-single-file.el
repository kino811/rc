;;; html-single-file.el --- Make a self-contained HTML file to send -*- lexical-binding: t -*-

;; Shared by myInit.org (full config) and quick-init.el (emacs-q).
;;
;; An exported HTML file refers to its images as separate files, which get
;; lost when the HTML alone is sent.  These commands write NAME-single.html
;; next to NAME.html with every local image (and local stylesheet) embedded,
;; so one file can be sent.
;;
;; The HTML itself is copied unchanged: only local <img src="..."> become
;; data: URIs and local <link rel="stylesheet" href="..."> become <style>.
;; (pandoc --embed-resources was tried first, but it re-renders the HTML and
;; drops Org's styles and structure.)  Remote (http/https) and data: URLs are
;; kept as they are, and a missing image is kept and reported.
;;
;;   M-x my/html-make-single-file   asks for the HTML file; the default is the
;;                                  file at point in dired, the .html of the
;;                                  current Org buffer, or the current file
;;   C-c C-e h s                    Org export menu: export to HTML, then make
;;                                  the single file
;;   C-c C-e h S                    the same, then open it (like h o)

;;; Code:

(require 'subr-x)
(autoload 'url-unhex-string "url-util")

(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))
(declare-function org-html-export-to-html "ox-html"
                  (&optional async subtreep visible-only body-only ext-plist))
(declare-function org-export-define-derived-backend "ox" (child parent &rest body))
(declare-function org-open-file "org" (path &optional in-emacs line search))

(defconst my/html-single-file-mime-types
  '(("png" . "image/png") ("jpg" . "image/jpeg") ("jpeg" . "image/jpeg")
    ("gif" . "image/gif") ("svg" . "image/svg+xml") ("webp" . "image/webp")
    ("bmp" . "image/bmp") ("ico" . "image/x-icon"))
  "MIME types of the image files that can be embedded, by extension.")

(defun my/html-single-file-output-name (html)
  "Return the single-file name for HTML: NAME.html -> NAME-single.html."
  (concat (file-name-sans-extension html) "-single.html"))

(defun my/html-single-file--local-path (url directory)
  "Return the local file that URL refers to, relative to DIRECTORY, or nil.
Return nil for remote, data: and fragment URLs."
  (let ((url (string-trim url)))
    (cond
     ((or (string-empty-p url)
          (string-match-p "\\`\\(?:data\\|https?\\|ftp\\|mailto\\):" url)
          (string-prefix-p "//" url)
          (string-prefix-p "#" url))
      nil)
     ((string-match "\\`file://\\(.*\\)" url)
      (let ((path (url-unhex-string (match-string 1 url))))
        ;; file:///C:/dir/a.png -> C:/dir/a.png
        (when (string-match-p "\\`/[A-Za-z]:" path)
          (setq path (substring path 1)))
        path))
     (t
      (expand-file-name (url-unhex-string
                         (car (split-string url "[?#]")))
                        directory)))))

(defun my/html-single-file--data-uri (file)
  "Return FILE as a base64 data: URI, or nil when its type is unknown."
  (when-let* ((mime (cdr (assoc (downcase (or (file-name-extension file) ""))
                                my/html-single-file-mime-types))))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (base64-encode-region (point-min) (point-max) t)
      (concat "data:" mime ";base64," (buffer-string)))))

(defun my/html-single-file--read-text (file)
  "Return the contents of text FILE, decoded as UTF-8."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (buffer-string)))

(defun my/html-single-file-embed (html &optional output)
  "Write OUTPUT, a copy of HTML with its local images and stylesheets embedded.
OUTPUT defaults to `my/html-single-file-output-name'.  Return a plist
\(:output FILE :embedded N :missing LIST)."
  (let* ((html (expand-file-name html))
         (output (expand-file-name (or output (my/html-single-file-output-name html))))
         (directory (file-name-directory html))
         (embedded 0)
         (missing nil)
         (case-fold-search t))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8))
        (insert-file-contents html))
      ;; <img ... src="local"> -> data: URI
      (goto-char (point-min))
      (while (re-search-forward
              "<img\\b[^>]*?\\bsrc=\\([\"']\\)\\([^\"']*\\)\\1" nil t)
        (let* ((beg (match-beginning 2))
               (end (match-end 2))
               (url (match-string 2))
               (file (my/html-single-file--local-path url directory)))
          (when file
            (let ((uri (and (file-readable-p file)
                            (my/html-single-file--data-uri file))))
              (if uri
                  (progn
                    (delete-region beg end)
                    (goto-char beg)
                    (insert uri)
                    (setq embedded (1+ embedded)))
                (push url missing))))))
      ;; <link rel="stylesheet" href="local.css"> -> <style>...</style>
      (goto-char (point-min))
      (while (re-search-forward "<link\\b[^>]*>" nil t)
        (let* ((tag (match-string 0))
               (beg (match-beginning 0))
               (end (match-end 0))
               (href (and (string-match-p "\\brel=[\"']?stylesheet" tag)
                          (string-match "\\bhref=\\([\"']\\)\\([^\"']*\\)\\1" tag)
                          (match-string 2 tag)))
               (file (and href (my/html-single-file--local-path href directory))))
          (when file
            (if (file-readable-p file)
                (progn
                  (delete-region beg end)
                  (goto-char beg)
                  (insert "<style type=\"text/css\">\n"
                          (my/html-single-file--read-text file)
                          "\n</style>")
                  (setq embedded (1+ embedded)))
              (push href missing)))))
      (let ((coding-system-for-write 'utf-8))
        (write-region nil nil output nil 'silent)))
    (list :output output :embedded embedded :missing (nreverse missing))))

(defun my/html-single-file--report (result)
  "Show RESULT of `my/html-single-file-embed' in the echo area."
  (let ((output (plist-get result :output))
        (missing (plist-get result :missing)))
    (message "Single HTML file: %s (%s, %d embedded%s)"
             (abbreviate-file-name output)
             (file-size-human-readable
              (file-attribute-size (file-attributes output)))
             (plist-get result :embedded)
             (if missing
                 (format ", %d NOT found: %s" (length missing)
                         (string-join missing ", "))
               ""))))

(defun my/html-single-file--default ()
  "Return the default HTML file for `my/html-make-single-file', or nil."
  (cond
   ((derived-mode-p 'dired-mode)
    (dired-get-filename nil t))
   ((and buffer-file-name (derived-mode-p 'org-mode))
    (concat (file-name-sans-extension buffer-file-name) ".html"))
   (buffer-file-name)))

;;;###autoload
(defun my/html-make-single-file (html)
  "Write HTML as NAME-single.html with its local images embedded.
Interactively, ask for the HTML file.  The default is the file at point
in dired, the .html of the current Org buffer (exported first when it
does not exist yet), or the current file."
  (interactive
   (let* ((default (my/html-single-file--default))
          (org-source (and (derived-mode-p 'org-mode) buffer-file-name)))
     (when (and org-source default (not (file-exists-p default))
                (y-or-n-p (format "%s does not exist.  Export it now? "
                                  (file-name-nondirectory default))))
       (require 'ox-html)
       (org-html-export-to-html))
     (list (read-file-name "HTML file: "
                           (and default (file-name-directory default))
                           default t
                           (and default (file-name-nondirectory default))))))
  (let ((output (my/html-single-file-output-name (expand-file-name html))))
    (unless (file-readable-p html)
      (user-error "Cannot read %s" html))
    (when (and (called-interactively-p 'any)
               (file-exists-p output)
               (not (y-or-n-p (format "%s exists.  Overwrite? "
                                      (file-name-nondirectory output)))))
      (user-error "Cancelled"))
    (let ((result (my/html-single-file-embed html output)))
      (my/html-single-file--report result)
      (plist-get result :output))))

(defun my/org-html-export-to-single-file
    (&optional async subtreep visible-only body-only)
  "Export the Org buffer to HTML, then write NAME-single.html next to it.
For the Org export menu (C-c C-e h s).  ASYNC is ignored: the single file
is made right after the HTML file is written.  SUBTREEP, VISIBLE-ONLY
and BODY-ONLY are passed to `org-html-export-to-html'."
  (when async
    (message "Single HTML file: exporting synchronously"))
  (let* ((html (expand-file-name
                (org-html-export-to-html nil subtreep visible-only body-only)))
         (result (my/html-single-file-embed html)))
    (my/html-single-file--report result)
    (plist-get result :output)))

(defun my/org-html-export-to-single-file-and-open
    (&optional async subtreep visible-only body-only)
  "Like `my/org-html-export-to-single-file', then open the single file.
For the Org export menu (C-c C-e h S).  The file is opened with
`org-open-file', the same way as \"As HTML file and open\" (h o)."
  (org-open-file
   (my/org-html-export-to-single-file async subtreep visible-only body-only)))

;; Add "s" and "S" to the HTML submenu of the Org export menu (C-c C-e h) without
;; touching ox-html: a derived back end whose menu entry is a numbered
;; sub-entry of ?h is merged into that menu, the way ox-beamer adds itself
;; to the LaTeX menu.
(with-eval-after-load 'ox-html
  (org-export-define-derived-backend 'html-single-file 'html
    :menu-entry
    '(?h 3 ((?s "As single HTML file (images embedded)"
                my/org-html-export-to-single-file)
            (?S "As single HTML file and open"
                my/org-html-export-to-single-file-and-open)))))

(provide 'html-single-file)
;;; html-single-file.el ends here
