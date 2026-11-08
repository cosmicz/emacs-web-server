;;; web-server-lifetime-test.el --- Dispatched request lifetime -*- lexical-binding: t; -*-
;; Copyright (C) 2026 OC (cosmicz@github)
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Real loopback controls.  The fixture records ordinary teardown BEFORE any
;; safety cleanup; a runner must reject candidate residue even if ERT passed.
;; The census is intentionally runner-enforced, so test-first baseline maps
;; keep healthy controls GREEN while reporting existing teardown metadata.
;; Interception controls require source or byte-compiled production, or native
;; subr trampolines. Evidence runners load the source and disable JIT.
;;; Code:
(require 'cl-lib)
(require 'ert)
(require 'web-server-frame-test)

(defvar ws-life-test--owned nil)
(defvar ws-life-test--census nil)

(defun ws-life-test--connections (server)
  "Find live pre-upgrade connections belonging to SERVER."
  (cl-remove-if-not
   (lambda (proc) (and (process-live-p proc)
                       (eq server (process-get proc :server))
                       (not (eq proc (ws-process server)))))
   (process-list)))

(defun ws-life-test--timers (before)
  "Find new non-runtime regular and idle timers absent from BEFORE."
  (cl-remove-if
   (lambda (timer) (or (memq timer before)
                       (eq (timer--function timer) 'undo-auto--boundary-timer)))
   (append timer-list timer-idle-list nil)))

(defmacro ws-life-test--with (server handler &rest body)
  "Run BODY on an ephemeral loopback SERVER using HANDLER.
Remember dispatched processes before WebSocket replaces their plist."
  (declare (indent 2))
  `(let ((,server nil) (ws-life-test--owned nil)
         (ws-test-deferred--clients nil)
         (timers-before (append timer-list timer-idle-list nil)))
     (setq ,server
           (ws-start (lambda (request)
                       (cl-pushnew (ws-process request) ws-life-test--owned)
                       (funcall ,handler request))
                     0 nil :host "127.0.0.1"))
     (unwind-protect (progn ,@body)
       (let ((owned (delete-dups
                     (append ws-life-test--owned
                             (ws-life-test--connections ,server)))))
         ;; Ordinary fixture cleanup; assertions in BODY precede this stop.
         (dolist (client ws-test-deferred--clients)
           (when (process-live-p client) (delete-process client)))
         (when (memq ,server ws-servers) (ws-stop ,server))
         (let* ((timers (ws-life-test--timers timers-before))
                (live (cl-remove-if-not #'process-live-p owned))
                (clients (cl-remove-if-not #'process-live-p ws-test-deferred--clients))
                (listener (process-live-p (ws-process ,server))))
           (push (list :requests (length (ws-requests ,server))
                       :registry (ws-frame-test--registry-count ,server)
                       :live-accepted (length live) :live-clients (length clients)
                       :listener-live (if listener t :json-false)
                       :new-timers (length timers)
                       :forced-processes (+ (length live) (length clients)
                                            (if listener 1 0)))
                 ws-life-test--census)
           ;; Never hide these interventions in the recorded row.
           (dolist (proc (append live clients (when listener (list (ws-process ,server)))))
             (ignore-errors (delete-process proc)))
           (mapc #'cancel-timer timers))))))

(defun ws-life-test--wait (predicate)
  "Service events until PREDICATE, with a bounded real-time deadline."
  (ws-test-deferred--wait predicate 5))

(defun ws-life-test--client (server &optional text)
  "Open an owned client to SERVER, optionally sending TEXT."
  (ws-test-deferred--client (ws-test-deferred--port server) text))

(defun ws-life-test--get ()
  "Return one complete empty-body request."
  (ws-test-deferred--get "/lifetime"))

(defun ws-life-test--open (server request-cell &optional text)
  "Open on SERVER and wait for REQUEST-CELL's handler observation."
  (let ((client (ws-life-test--client server (or text (ws-life-test--get)))))
    (should (ws-life-test--wait (lambda () (car request-cell))))
    (should (process-live-p client))
    client))

(defun ws-life-test--close (client server request)
  "Close CLIENT; require REQUEST to leave live SERVER before fixture cleanup."
  (delete-process client)
  (should (ws-life-test--wait (lambda () (not (process-live-p (ws-process request))))))
  (should (ws-life-test--wait (lambda () (not (memq request (ws-requests server)))))))

(defun ws-life-test--websocket (server request-cell messages)
  "Upgrade on SERVER and exchange a real masked frame before returning."
  (let ((client
         (ws-life-test--open
          server request-cell
          (concat "GET /socket HTTP/1.1\r\nHost: localhost\r\n"
                  "Upgrade: websocket\r\nConnection: Upgrade\r\n"
                  "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n"))))
    (should (ws-life-test--wait
             (lambda () (string-match-p "HTTP/1.1 101 " (process-get client :output)))))
    (should (eq #'ws-web-socket-filter (process-filter (ws-process (car request-cell)))))
    ;; Actual upgrade replaces :server, rather than leaving a convenient seam.
    (should-not (process-get (ws-process (car request-cell)) :server))
    (process-send-string client (unibyte-string #x81 #x82 1 2 3 4 (logxor ?h 1) (logxor ?i 2)))
    (should (ws-life-test--wait
             (lambda () (and (equal (car messages) '("hi"))
                             (string-suffix-p (ws-web-socket-frame "echo:hi")
                                              (process-get client :output))))))
    client))

(defun ws-life-test--websocket-handler (request-cell messages &optional prior)
  "Make an upgrade handler recording REQUEST-CELL and MESSAGES, then PRIOR."
  (lambda (request)
    (setcar request-cell request)
    (when (ws-web-socket-connect
           request (lambda (proc data)
                     (setcar messages (append (car messages) (list data)))
                     (process-send-string proc (ws-web-socket-frame (concat "echo:" data)))))
      (when prior (set-process-sentinel (ws-process request) prior))
      :keep-alive)))

(ert-deftest ws/lifetime-keep-alive-retained-while-live ()
  (let ((r (list nil)))
    (ws-life-test--with server (lambda (request) (setcar r request) :keep-alive)
      (let ((client (ws-life-test--open server r)))
        (should (process-live-p (ws-process (car r))))
        (should (equal (ws-requests server) (list (car r))))
        (should (= 0 (hash-table-count (ws-frame-test--registry server))))
        (should (process-live-p client))))))

(ert-deftest ws/lifetime-keep-alive-peer-close-prunes ()
  (let ((r (list nil)))
    (ws-life-test--with server (lambda (request) (setcar r request) :keep-alive)
      (let ((client (ws-life-test--open server r)))
        (should (memq (car r) (ws-requests server)))
        (ws-life-test--close client server (car r))
        (should (process-live-p (ws-process server)))
        (should-not (ws-requests server))))))

(ert-deftest ws/lifetime-websocket-retained-and-roundtrip ()
  (let ((r (list nil)) (messages (list nil)))
    (ws-life-test--with server (ws-life-test--websocket-handler r messages)
      (let ((client (ws-life-test--websocket server r messages)))
        (should (equal (ws-requests server) (list (car r))))
        (should (process-live-p client))
        (should (= 0 (hash-table-count (ws-frame-test--registry server))))))))

(ert-deftest ws/lifetime-websocket-peer-close-prunes ()
  (let ((r (list nil)) (messages (list nil)))
    (ws-life-test--with server (ws-life-test--websocket-handler r messages)
      (let ((client (ws-life-test--websocket server r messages)))
        (ws-life-test--close client server (car r))
        (should (process-live-p (ws-process server)))
        (should-not (ws-requests server))))))

(dolist (kind '(keep-alive websocket))
  (dolist (close-first '(nil t))
    (eval
     `(ert-deftest ,(intern (format "ws/lifetime-stop-%s-%s" kind (if close-first "dead-peer" "live-peer"))) ()
        (let ((r (list nil)) (messages (list nil)))
          (ws-life-test--with server
              ,(if (eq kind 'websocket)
                   '(ws-life-test--websocket-handler r messages)
                 '(lambda (request) (setcar r request) :keep-alive))
            (let ((client ,(if (eq kind 'websocket)
                               '(ws-life-test--websocket server r messages)
                             '(ws-life-test--open server r))))
              (should (memq (car r) (ws-requests server)))
              ,@(when close-first
                  '((delete-process client)
                    (should (ws-life-test--wait (lambda () (not (process-live-p (ws-process (car r)))))))))
              (ws-stop server)
              (should-not (ws-requests server))
              (should-not (process-live-p (ws-process (car r))))
              (should-not (process-live-p (ws-process server)))
              (should-not (memq server ws-servers))
              (should (ws-life-test--wait (lambda () (not (process-live-p client))))))))) t)))

(ert-deftest ws/lifetime-repeated-peer-close-does-not-accumulate ()
  (let ((r (list nil)))
    (ws-life-test--with server (lambda (request) (setcar r request) :keep-alive)
      (dotimes (_ 3)
        (setcar r nil)
        (let ((client (ws-life-test--open server r)))
          (should (= 1 (length (ws-requests server))))
          (ws-life-test--close client server (car r))
          (should-not (ws-requests server)))))))

(ert-deftest ws/lifetime-peer-close-preserves-sibling-and-stale-callback ()
  (let ((r (list nil)))
    (ws-life-test--with server (lambda (request) (setcar r request) :keep-alive)
      (let* ((a (ws-life-test--open server r)) (ra (car r))
             (pa (ws-process ra)) (saved (process-sentinel pa)))
        (setcar r nil)
        (let* ((b (ws-life-test--open server r)) (rb (car r)))
          (should (= 2 (length (ws-requests server))))
          (ws-life-test--close a server ra)
          (should (equal (ws-requests server) (list rb)))
          (when saved (funcall saved pa "closed again\n"))
          (should (equal (ws-requests server) (list rb)))
          (should (process-live-p b))
          (should (process-live-p (ws-process rb))))))))

(ert-deftest ws/lifetime-prior-terminal-observes-detached ()
  (let ((r (list nil)) (seen nil) (prior nil))
    (ws-life-test--with server
        (lambda (request)
          (setcar r request)
          (setq prior (lambda (proc _event)
                        (unless (process-live-p proc)
                          (push (length (ws-requests server)) seen))))
          (set-process-sentinel (ws-process request) prior)
          :keep-alive)
      (let* ((client (ws-life-test--open server r))
             (proc (ws-process (car r))) (saved (process-sentinel proc)))
        (delete-process client)
        (should (ws-life-test--wait (lambda () seen)))
        (funcall saved proc "closed again\n")
        (should (equal '(0) seen))
        (should-not (ws-requests server))
        (should (eq prior (process-sentinel (ws-process (car r)))))))))

(ert-deftest ws/lifetime-live-event-preserves-request-and-delegates ()
  (let ((r (list nil)) (seen nil))
    (ws-life-test--with server
        (lambda (request)
          (setcar r request)
          (set-process-sentinel (ws-process request)
                                (lambda (_proc event) (push event seen)))
          :keep-alive)
      (ws-life-test--open server r)
      (let ((proc (ws-process (car r))))
        (funcall (process-sentinel proc) proc "lifetime live event")
        (should (equal "lifetime live event" (car seen)))
        (should (process-live-p proc))
        (should (equal (ws-requests server) (list (car r))))))))

(ert-deftest ws/lifetime-stale-wrapper-does-not-clobber-replacement ()
  (let ((r (list nil)))
    (ws-life-test--with server (lambda (request) (setcar r request) :keep-alive)
      (let* ((client (ws-life-test--open server r)) (proc (ws-process (car r)))
             (saved (process-sentinel proc)) (replacement (lambda (_p _e) nil)))
        (set-process-sentinel proc replacement)
        (delete-process client)
        (should (ws-life-test--wait (lambda () (not (process-live-p proc)))))
        ;; A caller that superseded ownership did not chain it; invoke the
        ;; captured old event explicitly to test stale-owner conditional restore.
        (when saved (funcall saved proc "closed\n"))
        (should-not (ws-requests server))
        (should (eq replacement (process-sentinel proc)))))))

(ert-deftest ws/lifetime-peer-close-during-handler-pruned-at-handoff ()
  (let ((r nil) (closed-count 0))
    (ws-life-test--with server
        (lambda (request)
          (setq r request)
          (set-process-sentinel (ws-process request)
                                (lambda (proc _event)
                                  (unless (process-live-p proc) (cl-incf closed-count))))
          (delete-process (ws-process request))
          :keep-alive)
      (let ((client (ws-life-test--client server (ws-life-test--get))))
        (should (ws-life-test--wait (lambda () (and r (not (process-live-p client))))))
        (should (= 1 closed-count))
        (should-not (ws-requests server))))))

(ert-deftest ws/lifetime-peer-close-during-watcher-install ()
  (let ((handoff nil) (entered nil) (closed-count 0)
        (original (symbol-function 'set-process-sentinel)))
    (ws-life-test--with server
        (lambda (request)
          (set-process-sentinel (ws-process request)
                                (lambda (proc _event)
                                  (unless (process-live-p proc) (cl-incf closed-count))))
          (setq handoff t)
          :keep-alive)
      (let* ((client (ws-life-test--client server))
             (proc (progn
                     (should (ws-life-test--wait (lambda () (ws-life-test--connections server))))
                     (car (ws-life-test--connections server)))))
        (cl-letf (((symbol-function 'set-process-sentinel)
                   (lambda (p sentinel)
                     (when (and handoff (eq p proc) (not entered))
                       (setq entered t)
                       (delete-process p))
                     (funcall original p sentinel))))
          (ws-filter proc (ws-life-test--get)))
        (should entered)
        (should (= 1 closed-count))
        (should-not (ws-requests server))
        (should (ws-life-test--wait (lambda () (not (process-live-p client)))))))))

(dolist (after-install '(nil t))
  (eval
   `(ert-deftest ,(intern (format "ws/lifetime-watcher-error-%s-install" (if after-install "after" "before"))) ()
      (let ((handoff nil) (failed nil) (seen nil) (prior nil)
            (original (symbol-function 'set-process-sentinel)))
        (ws-life-test--with server
            (lambda (request)
                  (setq prior (lambda (proc _event)
                            (unless (process-live-p proc)
                              (push (length (ws-requests server)) seen))))
              (set-process-sentinel (ws-process request) prior)
              (setq handoff t)
              :keep-alive)
          (let* ((client (ws-life-test--client server))
                 (proc (progn
                         (should (ws-life-test--wait (lambda () (ws-life-test--connections server))))
                         (car (ws-life-test--connections server)))))
            (cl-letf (((symbol-function 'set-process-sentinel)
                       (lambda (p sentinel)
                         (if (and handoff (eq p proc) (not failed))
                             (progn
                               (setq failed t)
                               ,@(when after-install '((funcall original p sentinel)))
                               (signal 'file-error '("lifetime watcher setup")))
                           (funcall original p sentinel)))))
              (should-error (ws-filter proc (ws-life-test--get)) :type 'file-error))
            (should failed)
            (should (eq prior (process-sentinel proc)))
            (should-not (ws-requests server))
            (should-not (process-live-p proc))
            (should (equal '(0) seen))
            (should (process-live-p (ws-process server)))
            (should (ws-life-test--wait (lambda () (not (process-live-p client))))))))) t))

(ert-deftest ws/lifetime-stop-reentrant-prior-sees-detached ()
  (let ((r (list nil)) (seen nil) (reentered nil))
    (ws-life-test--with server
        (lambda (request)
          (setcar r request)
          (set-process-sentinel
           (ws-process request)
           (lambda (proc _event)
             (unless (process-live-p proc)
               (push (length (ws-requests server)) seen)
               (unless reentered (setq reentered t) (ws-stop server)))))
          :keep-alive)
      (ws-life-test--open server r)
      (setcar r nil)
      (ws-life-test--open server r)
      (should (= 2 (length (ws-requests server))))
      (ws-stop server)
      (should reentered)
      (should (equal '(0 0) seen))
      (should-not (ws-requests server))
      (should-not (process-live-p (ws-process server))))))

(ert-deftest ws/lifetime-stop-from-handler-does-not-reregister ()
  (let ((r nil))
    (ws-life-test--with server
        (lambda (request) (setq r request) (ws-stop server) :keep-alive)
      (let ((client (ws-life-test--client server (ws-life-test--get))))
        (should (ws-life-test--wait (lambda () (and r (not (process-live-p client))))))
        (should-not (ws-requests server))
        (should-not (process-live-p (ws-process server)))
        (should-not (memq server ws-servers))))))

(ert-deftest ws/lifetime-pruning-preserves-caller-held-request ()
  (let ((r (list nil)) (body "caller-owned-body"))
    (ws-life-test--with server (lambda (request) (setcar r request) :keep-alive)
      (let ((client (ws-life-test--open
                     server r (format "POST /lifetime HTTP/1.1\r\nHost: localhost\r\nContent-Length: %d\r\n\r\n%s"
                                      (length body) body))))
        (should (equal body (ws-body (car r))))
        (ws-life-test--close client server (car r))
        (should (equal body (ws-body (car r))))
        (should (assoc :POST (ws-headers (car r))))))))

(dolist (ending '(completed disconnected stopped))
  (eval
   `(ert-deftest ,(intern (format "ws/lifetime-deferred-%s-owns-sentinel" ending)) ()
      (let ((r (list nil)) (responder nil) (closed nil))
        (ws-life-test--with server
            (lambda (request)
              (setcar r request)
              (setq responder (ws-defer request :on-close
                                        (lambda (_r reason) (push reason closed)))))
          (let* ((client (ws-life-test--open server r)) (proc (ws-process (car r))))
            (should (eq #'ws--deferred-sentinel (process-sentinel proc)))
            (should (eq 'pending (ws-responder-state responder)))
            (should (equal (ws-requests server) (list (car r))))
            ,(pcase ending
               ('completed '(should (ws-respond responder 200 '(("Content-Length" . "2")) "ok")))
               ('disconnected '(delete-process client))
               ('stopped '(ws-stop server)))
            (should (ws-life-test--wait (lambda () (eq ',ending (ws-responder-state responder)))))
            (should (equal '(,ending) closed))
            (should-not (ws-requests server))
            (should-not (ws-responder-abort responder))
            (should-not (ws-respond responder 200 nil "late"))
            (should (equal '(,ending) closed)))))) t))

(ert-deftest ws/lifetime-partial-frame-retains-framing-sentinel ()
  (let ((calls 0))
    (ws-life-test--with server (lambda (_r) (cl-incf calls) :keep-alive)
      (let ((client (ws-life-test--client server "GET /partial HTTP/1.1\r\nHost:")))
        (should (ws-life-test--wait (lambda () (= 1 (length (ws-requests server))))))
        (let* ((request (car (ws-requests server))) (proc (ws-process request))
               (entry (gethash proc (ws-frame-test--registry server))))
          (should entry)
          (should (= 0 calls))
          (should (eq (process-sentinel proc) (ws--frame-entry-sentinel entry)))
          (delete-process client)
          (should (ws-life-test--wait (lambda () (and (null (ws-requests server))
                                                      (= 0 (hash-table-count (ws-frame-test--registry server))))))))))))

(ert-deftest ws/lifetime-normal-reply-still-closes ()
  (let ((calls 0))
    (ws-life-test--with server
        (lambda (r) (cl-incf calls)
          (ws-response-header (ws-process r) 200 '("Content-Length" . "2"))
          (process-send-string (ws-process r) "ok"))
      (let ((client (ws-life-test--client server (ws-life-test--get))))
        (should (ws-life-test--wait (lambda () (not (process-live-p client)))))
        (should (string-suffix-p "\r\n\r\nok" (process-get client :output)))
        (should (= 1 calls))
        (should-not (ws-requests server))))))

(ert-deftest ws/lifetime-keep-alive-outside-framing-cap ()
  (let ((r (list nil)) (ws-request-incomplete-limit 1))
    (ws-life-test--with server (lambda (request) (setcar r request) :keep-alive)
      (let ((a (ws-life-test--open server r)))
        (should (= 0 (hash-table-count (ws-frame-test--registry server))))
        (setcar r nil)
        (let ((b (ws-life-test--open server r)))
          (should (process-live-p a)) (should (process-live-p b))
          (should (= 2 (length (ws-requests server))))
          (should (= 0 (hash-table-count (ws-frame-test--registry server)))))))))

(ert-deftest ws/lifetime-mixed-stop-idempotent ()
  (let ((requests nil) (responder nil) (closed nil) (messages nil))
    (ws-life-test--with server
        (lambda (r)
          (push r requests)
          (pcase (cdr (assoc :GET (ws-headers r)))
            ("/defer" (setq responder (ws-defer r :on-close
                                                (lambda (_r reason) (push reason closed)))))
            ("/socket" (when (ws-web-socket-connect r (lambda (_p m) (push m messages))) :keep-alive))
            (_ :keep-alive)))
      (let* ((a (ws-life-test--client server (ws-test-deferred--get "/keep")))
             (b (ws-life-test--client server (ws-test-deferred--get "/defer")))
             (c (ws-life-test--client server
                                      (concat "GET /socket HTTP/1.1\r\nHost: localhost\r\n"
                                              "Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n")))
             (d (ws-life-test--client server "GET /partial HTTP/1.1\r\n")))
        (should (ws-life-test--wait (lambda () (and responder (= 4 (length (ws-requests server)))))))
        (should (= 1 (hash-table-count (ws-frame-test--registry server))))
        (ws-stop server)
        (should-not (ws-requests server))
        (should (= 0 (hash-table-count (ws-frame-test--registry server))))
        (should (eq 'stopped (ws-responder-state responder)))
        (should (equal '(stopped) closed))
        (should (ws-life-test--wait (lambda () (cl-every (lambda (p) (not (process-live-p p))) (list a b c d)))))
        (ws-stop server)
        (should-not (ws-requests server))
        (should (equal '(stopped) closed))))))


(dolist (kind '(keep-alive websocket))
  (eval
   `(ert-deftest ,(intern (format "ws/lifetime-stop-prunes-unobserved-dead-%s" kind)) ()
      (let ((r (list nil)) (messages (list nil)))
        (ws-life-test--with server
            ,(if (eq kind 'websocket)
                 '(ws-life-test--websocket-handler r messages)
               '(lambda (request) (setcar r request) :keep-alive))
          (let* ((client ,(if (eq kind 'websocket)
                              '(ws-life-test--websocket server r messages)
                            '(ws-life-test--open server r)))
                 (proc (ws-process (car r))))
            ;; Deliberately supersede the library watcher WITHOUT chaining it.
            (set-process-sentinel proc #'ignore)
            (delete-process client)
            (should (ws-life-test--wait (lambda () (not (process-live-p proc)))))
            (should (memq (car r) (ws-requests server)))
            (ws-stop server)
            (should-not (ws-requests server))
            (should-not (process-live-p (ws-process server))))))) t))

(ert-deftest ws/lifetime-thrown-keep-alive-peer-close-prunes ()
  (let ((r (list nil)))
    (ws-life-test--with server
        (lambda (request) (setcar r request) (throw 'close-connection :keep-alive))
      (let ((client (ws-life-test--open server r)))
        (should (memq (car r) (ws-requests server)))
        (ws-life-test--close client server (car r))
        (should (process-live-p (ws-process server)))
        (should-not (ws-requests server))))))

(provide 'web-server-lifetime-test)
;;; web-server-lifetime-test.el ends here
