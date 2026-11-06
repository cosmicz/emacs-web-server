;;; web-server-owner-test.el --- Owner-thread queue contract -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Contract tests for queued owner calls.  Real sockets are ephemeral and owned;
;; workers never wait for process output or close a descriptor.  The existing
;; thirteen deferral controls in web-server-test.el remain unchanged.

;;; Code:
(require 'web-server-test)
(defvar ws-owner-call-queue-limit)

(defun ws-owner-test--worker (function)
  "Call FUNCTION on a worker; return its value or captured condition.
Yield Lisp execution, without servicing owner process events, for at most
three seconds.  Every test also runs inside an outer process deadline."
  (let* ((result nil)
         (thread (make-thread
                  (lambda ()
                    (setq result
                          (condition-case err (funcall function)
                            ((error quit) err))))
                  "ws-owner-test-worker"))
         (deadline (+ (float-time) 3)))
    (unwind-protect
        (progn
          (while (and (thread-live-p thread) (< (float-time) deadline))
            (thread-yield))
          (should-not (thread-live-p thread))
          result)
      (when (thread-live-p thread) (thread-signal thread 'quit nil)))))

(defun ws-owner-test--refusal (value reason)
  "Assert VALUE is the typed pre-acceptance refusal with REASON."
  (should (eq (car-safe value) 'ws-owner-call-refused))
  (should (eq (cadr value) reason)))

(ert-deftest ws/owner-acceptance-is-not-the-callback-result ()
  (ws-test-deferred--with #'ignore server
    (let ((calls 0) (owner (current-thread)) (observed nil))
      (should (eq t (ws-call-on-owner
                     server (lambda () (cl-incf calls)
                              (setq observed (current-thread)) :callback-value))))
      (should (= calls 0))
      (should (ws-test-deferred--wait (lambda () (= calls 1))))
      (should (eq owner observed))
      (should (= calls 1)))))

(ert-deftest ws/owner-busy-queues-in-order-once ()
  (ws-test-deferred--with #'ignore server
    (let ((seen nil) (owner (current-thread)))
      (should
       (eq t (ws-owner-test--worker
              (lambda ()
                (ws-call-on-owner server (lambda () (push (list 1 (current-thread)) seen)))
                (ws-call-on-owner server (lambda () (push (list 2 (current-thread)) seen)))))))
      ;; The owner has yielded to the worker, but has not read process output.
      (should-not seen)
      (should (ws-test-deferred--wait (lambda () (= 2 (length seen)))))
      (should (equal (reverse seen) (list (list 1 owner) (list 2 owner)))))))

(ert-deftest ws/owner-waiting-thread-wakes-and-writes-reply ()
  (let ((state (list nil)))
    (ws-test-deferred--with (ws-test-deferred--handlers state) server
      (let* ((client (ws-test-deferred--client (ws-test-deferred--port server)
                                              (ws-test-deferred--get "/defer")))
             (owner (current-thread)) (armed nil) (accepted nil) (observed nil)
             (calls 0) (worker nil) (callback-at nil))
        (should (ws-test-deferred--wait (lambda () (plist-get (car state) :responder))))
        (let ((responder (plist-get (car state) :responder)))
          (unwind-protect
              (progn
                (setq worker
                      (make-thread
                       (lambda ()
                         (while (not armed) (thread-yield))
                         (setq accepted
                               (condition-case err
                                   (ws-call-on-owner
                                    server (lambda ()
                                             (setq callback-at (float-time)
                                                   observed (current-thread))
                                             (cl-incf calls)
                                             (should (eq t (ws-respond responder 200 nil "owner")))))
                                 (error err))))
                       "ws-owner-test-waiting-worker"))
                (setq armed t)
                ;; A genuine process wait; no polling timer supplies the wake.
                (let ((start (float-time)) (end nil))
                  (accept-process-output (ws-owner-wake-reader server) 2)
                  (setq end (float-time))
                  (should (< (- end start) 1))
                  (should (<= start callback-at end)))
                (should (ws-test-deferred--wait (lambda () (not (process-live-p client)))))
                (should (eq t accepted))
                (should (eq owner observed))
                (should (= 1 calls))
                (should (string-match-p "\r\n\r\nowner\\'" (process-get client :output)))
                (should (equal '(completed) (plist-get (car state) :closed))))
            (when (and worker (thread-live-p worker)) (thread-signal worker 'quit nil))))))))

(ert-deftest ws/owner-queue-bound-refuses-before-acceptance ()
  (let ((ws-owner-call-queue-limit 2))
    (ws-test-deferred--with #'ignore server
      (let ((seen nil))
        (should (eq t (ws-owner-test--worker
                       (lambda () (ws-call-on-owner server (lambda () (push 1 seen)))))))
        (should (eq t (ws-owner-test--worker
                       (lambda () (ws-call-on-owner server (lambda () (push 2 seen)))))))
        (ws-owner-test--refusal
         (ws-owner-test--worker
          (lambda () (ws-call-on-owner server (lambda () (push :refused seen)))))
         'queue-full)
        (should-not seen)
        (should (ws-test-deferred--wait (lambda () (= 2 (length seen)))))
        (should (equal '(1 2) (reverse seen)))))))

(ert-deftest ws/owner-callback-error-does-not-strand-admitted-work ()
  (ws-test-deferred--with #'ignore server
    (let ((calls 0) (reports nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (format &rest args) (push (apply #'format format args) reports))))
        (should (eq t (ws-call-on-owner server (lambda () (error "callback failure")))))
        (should (eq t (ws-call-on-owner server (lambda () (cl-incf calls)))))
        (should (ws-test-deferred--wait (lambda () (= calls 1)))))
      (should (= calls 1))
      (should (cl-some (lambda (text) (string-match-p "callback failure" text)) reports)))))

(ert-deftest ws/owner-reentrant-submission-is-not-lost ()
  (ws-test-deferred--with #'ignore server
    (let ((seen nil))
      (ws-call-on-owner server
                        (lambda () (push 1 seen)
                          (ws-call-on-owner server (lambda () (push 2 seen)))))
      (should (ws-test-deferred--wait (lambda () (= 2 (length seen)))))
      (should (equal '(1 2) (reverse seen))))))

(ert-deftest ws/owner-stop-discards-queued-and-fences-late-calls ()
  (let ((baseline (process-list)) (calls 0))
    (ws-test-deferred--with #'ignore server
      (should (eq t (ws-owner-test--worker
                     (lambda () (ws-call-on-owner server (lambda () (cl-incf calls)))))))
      (let ((wake (ws-owner-wake-process server)))
        (should (process-live-p wake))
        (ws-stop server)
        (ws-owner-test--refusal
         (ws-owner-test--worker
          (lambda () (ws-call-on-owner server (lambda () (cl-incf calls))))) 'stopped)
        (should (= calls 0))
        (should-not (process-live-p wake))
        (should-not (ws-owner-queue server))
        (should-not (cl-set-difference (process-list) baseline))))))

(ert-deftest ws/owner-stop-from-callback-discards-the-rest ()
  (ws-test-deferred--with #'ignore server
    (let ((seen nil))
      (ws-call-on-owner server (lambda () (push :stop seen) (ws-stop server)))
      (ws-call-on-owner server (lambda () (push :late seen)))
      (should (ws-test-deferred--wait (lambda () seen)))
      (should (equal '(:stop) seen))
      (should-not (ws-owner-queue server)))))

(ert-deftest ws/owner-stop-fences-on-close-submission ()
  (let ((responder nil) (refusal nil) (calls 0))
    (ws-test-deferred--with
        (lambda (request)
          (setq responder
                (ws-defer request :on-close
                          (lambda (_r _reason)
                            (setq refusal
                                  (condition-case err
                                      (ws-call-on-owner
                                       (plist-get (process-plist (ws-process request)) :server)
                                       (lambda () (cl-incf calls)))
                                    (error err)))))))
        server
      (ws-test-deferred--client (ws-test-deferred--port server)
                                (ws-test-deferred--get "/defer"))
      (should (ws-test-deferred--wait (lambda () responder)))
      (ws-stop server)
      (ws-owner-test--refusal refusal 'stopped)
      (should (= calls 0))
      (should (eq 'stopped (ws-responder-state responder))))))

(defun ws-owner-test--guard (operation)
  "Prove off-owner OPERATION refuses before any terminal mutation.
The mutation tripwires make the pre-implementation RED safe: a missing
guard cannot perform the known unsafe worker close on a real socket."
  (let ((state (list nil)))
    (ws-test-deferred--with (ws-test-deferred--handlers state) server
      (let ((client (ws-test-deferred--client (ws-test-deferred--port server)
                                             (ws-test-deferred--get "/defer"))))
        (should (ws-test-deferred--wait (lambda () (plist-get (car state) :responder))))
        (let* ((responder (plist-get (car state) :responder))
               (request (ws-responder-request responder))
               (connection (ws-process request))
               (requests (copy-sequence (ws-requests server)))
               (servers (copy-sequence ws-servers))
               (attempts 0) (result nil))
          (unwind-protect
              (progn
                (cl-letf (((symbol-function 'ws--responder-finish)
                           (lambda (&rest _) (cl-incf attempts) (error "unsafe finish tripwire")))
                          ((symbol-function 'ws-response-header)
                           (lambda (&rest _) (cl-incf attempts) (error "unsafe write tripwire")))
                          ((symbol-function 'delete-process)
                           (lambda (&rest _) (cl-incf attempts) (error "unsafe close tripwire"))))
                  (setq result (ws-owner-test--worker
                                (lambda () (funcall operation server responder)))))
                (should (eq 'ws-owner-thread-error (car-safe result)))
                (should (= 0 attempts))
                (should (process-live-p connection))
                (should (process-live-p client))
                (should (eq 'pending (ws-responder-state responder)))
                (should (equal requests (ws-requests server)))
                (should (equal servers ws-servers))
                (should-not (plist-get (car state) :closed))
                (should (equal "" (process-get client :output))))
            ;; The old ws-stop removes SERVER before the tripwire fires.
            ;; Restore only its cleanup eligibility after all assertions.
            (unless (memq server ws-servers) (push server ws-servers))))))))

(ert-deftest ws/owner-respond-refuses-off-thread-before-mutation ()
  (ws-owner-test--guard (lambda (_server responder) (ws-respond responder 200 nil "unsafe"))))

(ert-deftest ws/owner-abort-refuses-off-thread-before-mutation ()
  (ws-owner-test--guard (lambda (_server responder) (ws-responder-abort responder))))

(ert-deftest ws/owner-stop-refuses-off-thread-before-mutation ()
  (ws-owner-test--guard (lambda (server _responder) (ws-stop server))))

(ert-deftest ws/owner-wake-failure-refuses-and-rolls-back ()
  (ws-test-deferred--with #'ignore server
    (let ((calls 0) (original (symbol-function 'process-send-string)))
      (cl-letf (((symbol-function 'process-send-string)
                 (lambda (process text)
                   (if (eq process (ws-owner-wake-process server))
                       (error "owned wake write refused")
                     (funcall original process text)))))
        (ws-owner-test--refusal
         (ws-owner-test--worker
          (lambda () (ws-call-on-owner server (lambda () (cl-incf calls))))) 'wake-failed))
      (should-not (ws-owner-queue server))
      (should (= calls 0))
      ;; The failed admission cannot poison the following healthy one.
      (should (eq t (ws-call-on-owner server (lambda () (cl-incf calls)))))
      (should (ws-test-deferred--wait (lambda () (= calls 1)))))))

(ert-deftest ws/owner-coalesces-wakes-and-returns-to-baseline ()
  (let ((baseline (process-list)))
    (ws-test-deferred--with #'ignore server
      (let ((writes 0) (calls 0) (original (symbol-function 'process-send-string)))
        (cl-letf (((symbol-function 'process-send-string)
                   (lambda (process text)
                     (when (eq process (ws-owner-wake-process server)) (cl-incf writes))
                     (funcall original process text))))
          (should (eq t (ws-owner-test--worker
                         (lambda ()
                           (dotimes (_ 16)
                             (ws-call-on-owner server (lambda () (cl-incf calls))))
                           t))))
          (should (= calls 0))
          (should (= writes 1))
          (should (ws-test-deferred--wait (lambda () (= calls 16)))))
        (ws-stop server)
        (should-not (cl-set-difference (process-list) baseline))))))

(ert-deftest ws/owner-listener-and-accepted-socket-are-locked ()
  (let ((owner (current-thread)) (seen nil) (locks nil)
        (original (symbol-function 'set-process-thread)))
    ;; process-thread alone cannot witness the descriptor lock: make_process
    ;; initializes the Lisp slot even before set-process-thread sets fd locks.
    (cl-letf (((symbol-function 'set-process-thread)
               (lambda (process thread)
                 (prog1 (funcall original process thread)
                   (push (list process thread) locks)))))
      (ws-test-deferred--with
          (lambda (request)
            (setq seen (list (current-thread) (ws-process request))))
          server
        (dolist (process (list (ws-process server)
                              (ws-owner-wake-process server)
                              (ws-owner-wake-reader server)))
          (should (eq owner (process-thread process)))
          (should (member (list process owner) locks)))
        (ws-test-deferred--client (ws-test-deferred--port server)
                                  (ws-test-deferred--get "/sync"))
        (should (ws-test-deferred--wait (lambda () seen)))
        (should (eq owner (car seen)))
        (should (eq owner (process-thread (cadr seen))))
        (should (member (list (cadr seen) owner) locks))))))

(ert-deftest ws/owner-no-thread-path-is-inline-and-contained ()
  (let ((original (symbol-function 'fboundp)) (calls 0))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (symbol) (and (not (eq symbol 'make-thread))
                                     (funcall original symbol)))))
      (ws-test-deferred--with #'ignore server
        (should (eq t (ws-call-on-owner server (lambda () (cl-incf calls) :value))))
        (should (= calls 1))
        (should-not (ws-owner-wake-process server))
        (should (eq t (ws-call-on-owner server (lambda () (error "inline failure")))))
        (ws-stop server)
        (ws-owner-test--refusal
         (condition-case err (ws-call-on-owner server (lambda () (cl-incf calls)))
           (error err)) 'stopped)
        (should (= calls 1))))))

(ert-deftest ws/owner-start-failure-cleans-partial-channel ()
  (let ((baseline (process-list)) (servers ws-servers)
        (original (symbol-function 'make-network-process)) (attempts 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'make-network-process)
                     (lambda (&rest args)
                       (when (equal (plist-get args :name) "ws-owner-wake-sender")
                         (cl-incf attempts)
                         (error "injected owned sender setup failure"))
                       (apply original args))))
            (should-error (ws-start #'ignore 0)))
          (should (= attempts 1))
          (should (equal servers ws-servers))
          (should-not (cl-set-difference (process-list) baseline)))
      (dolist (server (cl-set-difference ws-servers servers)) (ws-stop server)))))

(ert-deftest ws/owner-unexpected-wake-peer-refuses-start-cleanly ()
  (let ((baseline (process-list)) (servers ws-servers)
        (original (symbol-function 'process-contact)) (checks 0))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'process-contact)
                     (lambda (process &optional key)
                       (if (eq key :remote)
                           (progn (cl-incf checks) [127 0 0 1 0])
                         (funcall original process key)))))
            (should-error (ws-start #'ignore 0)))
          (should (> checks 0))
          (should (equal servers ws-servers))
          (should-not (cl-set-difference (process-list) baseline)))
      (dolist (server (cl-set-difference ws-servers servers)) (ws-stop server)))))

(ert-deftest ws/owner-invalid-queue-limit-refuses-before-resource-allocation ()
  (dolist (limit '(0 -1 nil "2"))
    (let ((ws-owner-call-queue-limit limit)
          (baseline (process-list)) (servers ws-servers))
      (unwind-protect
          (progn
            (should-error (ws-start #'ignore 0))
            (should (equal servers ws-servers))
            (should-not (cl-set-difference (process-list) baseline)))
        (dolist (server (cl-set-difference ws-servers servers)) (ws-stop server))))))

(ert-deftest ws/owner-worker-submission-during-yielding-callback-is-not-lost ()
  ;; A one-call drain budget forces callback 2 into the following wake cycle.
  ;; Otherwise an implementation that drains the changing queue could mask a
  ;; wake flag cleared too late by running callback 2 in the first cycle.
  (let ((ws-owner-call-queue-limit 1))
    (ws-test-deferred--with #'ignore server
      (let ((owner (current-thread)) (submitter nil) (observed nil)
            (first-calls 0) (second-calls 0) (accepted nil))
        (should
         (eq t (ws-call-on-owner
                server
                (lambda ()
                  (cl-incf first-calls)
                  (setq accepted
                        (ws-owner-test--worker
                         (lambda ()
                           (setq submitter (current-thread))
                           (ws-call-on-owner
                            server (lambda ()
                                     (setq observed (current-thread))
                                     (cl-incf second-calls))))))))))
        (should (ws-test-deferred--wait (lambda () (= second-calls 1))))
        (should (eq t accepted))
        (should (= first-calls 1))
        (should (= second-calls 1))
        (should-not (eq owner submitter))
        (should (eq owner observed))))))

(ert-deftest ws/owner-capacity-returns-after-drain ()
  (let ((ws-owner-call-queue-limit 2))
    (ws-test-deferred--with #'ignore server
      (let ((calls 0))
        (dotimes (_ 2)
          (should (eq t (ws-owner-test--worker
                         (lambda () (ws-call-on-owner server (lambda () (cl-incf calls))))))))
        (should (ws-test-deferred--wait (lambda () (= calls 2))))
        (should (eq t (ws-owner-test--worker
                       (lambda () (ws-call-on-owner server (lambda () (cl-incf calls)))))))
        (should (ws-test-deferred--wait (lambda () (= calls 3))))
        (should (= calls 3))))))

(ert-deftest ws/owner-transient-listener-is-retired-before-start-returns ()
  (let ((baseline (process-list)))
    (ws-test-deferred--with #'ignore server
      ;; Check immediately, while SERVER is live and before any request/stop.
      (let ((created (cl-remove-if-not
                      #'process-live-p (cl-set-difference (process-list) baseline))))
        (should (= 3 (length created))) ; HTTP listener and two wake endpoints
        (should (memq (ws-process server) created))
        (should (memq (ws-owner-wake-process server) created))
        (should (memq (ws-owner-wake-reader server) created))
        (should-not (cl-find-if
                     (lambda (process)
                       (string-prefix-p "ws-owner-wake-listener" (process-name process)))
                     created))))))

(ert-deftest ws/owner-accepted-lock-precedes-first-filter-and-handler ()
  (let ((owner (current-thread)) (events nil) (connection nil)
        (lock (symbol-function 'set-process-thread))
        (filter (symbol-function 'ws-filter)))
    (cl-letf (((symbol-function 'set-process-thread)
               (lambda (process thread)
                 (prog1 (funcall lock process thread)
                   (push (list 'lock process (current-thread) thread) events))))
              ((symbol-function 'ws-filter)
               (lambda (process text)
                 (push (list 'filter process (current-thread)) events)
                 (funcall filter process text))))
      (ws-test-deferred--with
          (lambda (request)
            (setq connection (ws-process request))
            (push (list 'handler connection (current-thread)) events))
          server
        (ws-test-deferred--client (ws-test-deferred--port server)
                                  (ws-test-deferred--get "/sync"))
        (should (ws-test-deferred--wait (lambda () connection)))
        (let* ((ordered (reverse events))
               (position (lambda (kind)
                           (cl-position-if
                            (lambda (event)
                              (and (eq kind (car event)) (eq connection (cadr event))))
                            ordered)))
               (locked-at (funcall position 'lock))
               (filtered-at (funcall position 'filter))
               (handled-at (funcall position 'handler)))
          (should (integerp locked-at))
          (should (integerp filtered-at))
          (should (integerp handled-at))
          (should (< locked-at filtered-at handled-at))
          (dolist (index (list locked-at filtered-at handled-at))
            (should (eq owner (nth 2 (nth index ordered)))))
          (should (eq owner (nth 3 (nth locked-at ordered)))))))))

(defun ws-znrv-test--text (text)
  "Normalize only whitespace in public contract TEXT."
  (replace-regexp-in-string "[[:space:]]+" " " text))

(defun ws-znrv-test--setup-doc (text)
  "Assert TEXT discloses the actual setup wait and callback bound."
  (setq text (ws-znrv-test--text text))
  (should (string-match-p "filters, sentinels and timers" text))
  (should (string-match-p "other Lisp threads" text))
  (should (string-match-p "two-second deadline is checked between bounded waits" text))
  (should (string-match-p "callback runtime may extend" text)))

(defun ws-znrv-test--readme ()
  "Read the README belonging to the loaded fork."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "README" (file-name-directory (locate-library "web-server"))))
    (buffer-string)))

(ert-deftest ws/znrv-helper-doc-discloses-setup-reentry ()
  (ws-znrv-test--setup-doc (documentation 'ws--owner-open-channel)))

(ert-deftest ws/znrv-start-doc-discloses-setup-reentry ()
  (ws-znrv-test--setup-doc (documentation 'ws-start)))

(ert-deftest ws/znrv-readme-discloses-setup-reentry ()
  (ws-znrv-test--setup-doc (ws-znrv-test--readme)))

(ert-deftest ws/znrv-channel-loss-claim-requires-owner-observation ()
  (should (string-match-p "when the owner thread observes the loss"
                          (ws-znrv-test--text (ws-znrv-test--readme)))))

(ert-deftest ws/znrv-nil-request-retains-accept-failure-log ()
  (let ((log (generate-new-buffer " *ws-znrv-failure-log*")) (server nil))
    (unwind-protect
        (progn
          (setq server (ws-start #'ignore 0 log))
          (let* ((listener (ws-process server))
                 (callback (process-contact listener :log)))
            (should (functionp callback))
            (cl-letf (((symbol-function 'process-contact)
                       (lambda (&rest _) (ert-fail "No contact exists for a nil request")))
                      ((symbol-function 'ws--owner-lock-process)
                       (lambda (&rest _) (ert-fail "No request socket exists to lock")))
                      ((symbol-function 'format-time-string) (lambda (&rest _) "STAMP")))
              (funcall callback listener nil "accept failed: fixture\n"))
            (with-current-buffer log
              (should (equal "STAMP\tnil\tnil\taccept failed: fixture\n" (buffer-string))))))
      (when server (ws-stop server))
      (kill-buffer log))))

(ert-deftest ws/znrv-accepted-log-keeps-contact-and-lock-order ()
  (let* ((log (generate-new-buffer " *ws-znrv-accepted-log*"))
         (ws-test-deferred--clients nil) (state (list nil))
         (server (ws-start (ws-test-deferred--handlers state) 0 log)))
    (unwind-protect
        (progn
          (ws-test-deferred--client (ws-test-deferred--port server) (ws-test-deferred--get "/defer"))
          (should (ws-test-deferred--wait (lambda () (plist-get (car state) :request))))
          (let* ((child (ws-process (plist-get (car state) :request)))
                 (listener (ws-process server))
                 (callback (process-contact listener :log))
                 (contact (process-contact child))
                 (lock (symbol-function 'ws--owner-lock-process))
                 (locks nil))
            (with-current-buffer log (erase-buffer))
            (cl-letf (((symbol-function 'ws--owner-lock-process)
                       (lambda (srv process)
                         (with-current-buffer log (should (equal "" (buffer-string))))
                         (push process locks)
                         (funcall lock srv process)))
                      ((symbol-function 'format-time-string) (lambda (&rest _) "STAMP")))
              (funcall callback listener child "accepted fixture\n"))
            (should (equal (list child) locks))
            (with-current-buffer log
              (should (equal (format "STAMP\t%s\t%s\taccepted fixture\n"
                                     (car contact) (cadr contact))
                             (buffer-string))))))
      (mapc (lambda (p) (when (process-live-p p) (delete-process p))) ws-test-deferred--clients)
      (ws-stop server)
      (kill-buffer log))))

(ert-deftest ws/znrv-disabled-log-accept-failure-is-safe ()
  (ws-test-deferred--with #'ignore server
    (let* ((listener (ws-process server)) (callback (process-contact listener :log)))
      (should (functionp callback))
      (cl-letf (((symbol-function 'process-contact) (lambda (&rest _) (ert-fail "Unexpected contact lookup")))
                ((symbol-function 'ws--owner-lock-process) (lambda (&rest _) (ert-fail "Unexpected nil socket lock"))))
        (should-not (condition-case err
                        (funcall callback listener nil "accept failed: no log\n")
                      (error err)))))))

(provide 'web-server-owner-test)
;;; web-server-owner-test.el ends here
