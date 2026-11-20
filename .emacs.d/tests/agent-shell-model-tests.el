;;; agent-shell-model-tests.el --- Model change feedback tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'agent-shell)

(let ((source (expand-file-name "../myInit.org"
                                (file-name-directory load-file-name))))
  (with-temp-buffer
    (insert-file-contents source)
    (search-forward "(defun my/agent-shell-verify-model-change ")
    (goto-char (match-beginning 0))
    (eval (read (current-buffer)) t)))

(cl-defun my/agent-shell-model-test-request
    (&key response transport-error failure-callback (config-id "model"))
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (setq-local agent-shell--state
                (copy-tree
                 '((:session . ((:id . "test-session")))
                   (:config-options
                    . (((:id . "model") (:category . "model")
                        (:current-value . "sol")
                        (:options . (((:value . "sol") (:name . "Sol"))
                                     ((:value . "opus") (:name . "Opus")))))
                       ((:id . "mode") (:category . "mode")
                        (:current-value . "ask")))))))
    (let ((success-count 0)
          failures messages pending)
      (advice-add 'agent-shell--set-session-config-option :around
                  #'my/agent-shell-verify-model-change)
      (unwind-protect
          (cl-letf (((symbol-function 'agent-shell--send-request)
                     (lambda (&rest args) (setq pending args)))
                    ((symbol-function 'agent-shell--update-header-and-mode-line)
                     #'ignore)
                    ((symbol-function 'agent-shell--emit-event) #'ignore)
                    ((symbol-function 'message)
                     (lambda (format-string &rest args)
                       (push (apply #'format format-string args) messages))))
            (let ((on-success (lambda () (cl-incf success-count)))
                  (on-failure (when failure-callback
                                (lambda (error raw)
                                  (push (list error raw) failures)))))
              (if (equal config-id "model")
                  (agent-shell--config-option-set-model-id
                   :model-id "opus"
                   :on-success on-success :on-failure on-failure)
                (agent-shell--set-session-config-option
                 :config-id config-id :value "code"
                 :on-success on-success :on-failure on-failure)))
            ;; Complete after the advised call returns, as with a real ACP request.
            (if transport-error
                (funcall (plist-get pending :on-failure) transport-error "raw")
              (funcall (plist-get pending :on-success) response))
            (list :success-count success-count :failures failures
                  :messages messages
                  :current (map-elt
                            (agent-shell--config-option-get
                             :state agent-shell--state :id config-id)
                            :current-value)))
        (advice-remove 'agent-shell--set-session-config-option
                       #'my/agent-shell-verify-model-change)))))

(defun my/agent-shell-model-test-response (model)
  `((configOptions
     . [((id . "model") (category . "model") (type . "select")
         (currentValue . ,model)
         (options . [((value . "sol") (name . "Sol"))
                     ((value . "opus") (name . "Opus"))]))])))

(ert-deftest my/agent-shell-model-change-matching-response ()
  (let ((result (my/agent-shell-model-test-request
                 :response (my/agent-shell-model-test-response "opus"))))
    (should (= (plist-get result :success-count) 1))
    (should (equal (plist-get result :messages) '("Model: Opus")))
    (should (equal (plist-get result :current) "opus"))))

(ert-deftest my/agent-shell-model-change-mismatch-suppresses-success ()
  (let ((result (my/agent-shell-model-test-request
                 :response (my/agent-shell-model-test-response "sol"))))
    (should (= (plist-get result :success-count) 0))
    (should (equal (plist-get result :messages)
                   '("Model change not applied: requested Opus; current model: Sol")))
    (should (equal (plist-get result :current) "sol"))))

(ert-deftest my/agent-shell-model-change-mismatch-calls-failure ()
  (let* ((result (my/agent-shell-model-test-request
                  :response (my/agent-shell-model-test-response "sol")
                  :failure-callback t))
         (failures (plist-get result :failures)))
    (should (= (plist-get result :success-count) 0))
    (should-not (plist-get result :messages))
    (should (= (length failures) 1))
    (should (equal (map-elt (caar failures) 'message)
                   "Model change not applied: requested Opus; current model: Sol"))
    (should-not (cadar failures))))

(ert-deftest my/agent-shell-model-change-transport-error-unchanged ()
  (let* ((error '((code . -32603) (message . "Rejected")))
         (result (my/agent-shell-model-test-request
                  :transport-error error :failure-callback t)))
    (should (= (plist-get result :success-count) 0))
    (should (equal (plist-get result :failures) (list (list error "raw"))))
    (should (equal (plist-get result :current) "sol"))))

(ert-deftest my/agent-shell-model-change-acknowledgment-preserves-fallback ()
  (let ((result (my/agent-shell-model-test-request :response nil)))
    (should (= (plist-get result :success-count) 1))
    (should (equal (plist-get result :messages) '("Model: Opus")))
    (should (equal (plist-get result :current) "opus"))))

(ert-deftest my/agent-shell-model-change-other-options-unchanged ()
  (let ((result (my/agent-shell-model-test-request
                 :config-id "mode"
                 :response '((configOptions
                              . [((id . "mode") (category . "mode")
                                  (type . "select") (currentValue . "ask"))])))))
    (should (= (plist-get result :success-count) 1))
    (should-not (plist-get result :messages))
    (should (equal (plist-get result :current) "ask"))))
