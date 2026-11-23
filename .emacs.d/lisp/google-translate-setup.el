;;; google-translate-setup.el --- Google Translate commands and keys -*- lexical-binding: t -*-

;; Shared by myInit.org (full config) and quick-init.el (emacs-q).
;; The google-translate package (and its dependency popup) must already
;; be on `load-path' with its autoloads loaded; the full config gets
;; that from package.el, quick-init.el loads them from ~/.emacs.d/elpa.
;;
;; Keys:
;;   C-c t t t  popup translation; the direction (en->ko or ko->en) is
;;              picked from the text, and the result is also kept in
;;              *Google Translate* (use C-x b to revisit and copy it)
;;   C-c t t a  translate the word at point
;;   C-c t t q  translate a query typed in the minibuffer

;;; Code:

;; google-translate defines these variables, but only after the functions
;; below have `let'-bound or set them (the package loads on first use).
;; Declare them special so the bindings stay dynamic under lexical-binding.
(defvar google-translate-output-destination)
(defvar google-translate-last-translation-direction)
(defvar google-translate-translation-directions-alist)

;; `google-translate-smooth-translate' picks its direction from
;; `google-translate-last-translation-direction', an index into
;; `google-translate-translation-directions-alist'.  Setting that index
;; before the call switches the direction to match the text, while
;; leaving the minibuffer's own C-n/C-p direction cycling intact.
(defun my/translate--count-matches (regexp text)
  "Return how many characters in TEXT match REGEXP."
  (let ((count 0)
        (start 0))
    (while (string-match regexp text start)
      (setq count (1+ count))
      (setq start (match-end 0)))
    count))

(defun my/translate--korean-p (text)
  "Return non-nil when TEXT holds more Hangul than Latin characters."
  (> (my/translate--count-matches "[가-힣ㄱ-ㅎㅏ-ㅣ]" text)
     (my/translate--count-matches "[A-Za-z]" text)))

(defun my/translate--target-text ()
  "Return the text `google-translate-smooth-translate' would translate."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (or (current-word t t) "")))

(defun my/translate--direction-index (source target)
  "Return the index of SOURCE to TARGET in the directions alist, or nil."
  (seq-position google-translate-translation-directions-alist
                (cons source target)
                (lambda (item want)
                  (and (equal (car item) (car want))
                       (equal (cdr item) (cdr want))))))

(defvar my/translate-keep-result-buffer nil
  "Whether popup output should also update the translation buffer.")

(defun my/translate--keep-popup-result (gtos)
  "Keep GTOS in the translation buffer when requested by the popup command."
  (when my/translate-keep-result-buffer
    (with-current-buffer (get-buffer-create "*Google Translate*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (google-translate-buffer-insert-translation gtos)
        (goto-char (point-min))
        (set-buffer-modified-p nil)))))

(defun my/google-translate-smooth-translate-popup ()
  "Translate in a popup and retain the result, picking the direction from text."
  (interactive)
  (let* ((text (my/translate--target-text))
         (index (if (my/translate--korean-p text)
                    (my/translate--direction-index "ko" "en")
                  (my/translate--direction-index "en" "ko"))))
    (when index
      (setq google-translate-last-translation-direction index))
    (let ((google-translate-output-destination 'popup)
          (my/translate-keep-result-buffer t))
      (call-interactively #'google-translate-smooth-translate))))

(setq google-translate-translation-directions-alist '(("en" . "ko")
                                                      ("ko" . "en")))

;; The popup output function lives in google-translate-core-ui, which every
;; translate command loads first.
(with-eval-after-load 'google-translate-core-ui
  (advice-add 'google-translate-popup-output-translation :before
              #'my/translate--keep-popup-result))

(global-set-key (kbd "C-c t t a") #'google-translate-at-point)
(global-set-key (kbd "C-c t t q") #'google-translate-query-translate)
(global-set-key (kbd "C-c t t t") #'my/google-translate-smooth-translate-popup)

(provide 'google-translate-setup)
;;; google-translate-setup.el ends here
