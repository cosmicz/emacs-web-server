;;; web-server-frame-test.el --- Octet framing contract -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Complete framing and bounded admission regression controls.
(require 'web-server-test)
(require 'rx)
;; Production owns all framing settings; the provenance control checks this.
(defvar ws-frame-test--network-args nil)
(defvar ws-frame-test--log-buffer nil)

(defun ws-frame-test--put (state key value)
  (setcar state (plist-put (car state) key value)))
(defun ws-frame-test--get (state key)
  (plist-get (car state) key))
(defconst ws-frame-test--runtime-timer-functions '(undo-auto--boundary-timer)
  "Minimal allowlist of Emacs runtime timers outside this server's lifetime.
Redefining a primitive in the write-error control can initialize simple.el's
undo boundary timer.  Function identity is stable across variable rebinding;
all other new timers, including closures with nil args, remain in the census.")

(defun ws-frame-test--live-timers (before)
  "Return all new timers except the explicitly named Emacs runtime allowlist."
  (cl-remove-if (lambda (timer)
                  (memq (timer--function timer) ws-frame-test--runtime-timer-functions))
                (cl-set-difference (append timer-list timer-idle-list nil) before)))
(defun ws-frame-test--handler (state)
  (lambda (request)
    (ws-frame-test--put state :calls (1+ (or (ws-frame-test--get state :calls) 0)))
    (ws-frame-test--put state :request request)
    (ws-frame-test--put state :body (ws-body request))
    (ws-frame-test--put state :headers (copy-tree (ws-headers request)))
    (ws-response-header (ws-process request) 200 '("Content-Length" . "2"))
    (process-send-string (ws-process request) "OK")))

(defmacro ws-frame-test--with (state server &rest body)
  "Own a real server, clients, instrumentation and every newly made timer."
  (declare (indent 2))
  `(let* ((,state (list nil)) (,server nil)
          (ws-test-deferred--clients nil)
          (timers-before (append timer-list timer-idle-list nil))
          (original-filter (symbol-function 'ws-filter))
          (original-parse (symbol-function 'ws-parse-request)))
     (unwind-protect
         (cl-letf (((symbol-function 'ws-filter)
                    (lambda (proc bytes)
                      (let ((depth (or (ws-frame-test--get ,state :filter-depth) 0)))
                        (ws-frame-test--put ,state :filter-depth (1+ depth))
                        (ws-frame-test--put ,state :filter-thread (current-thread))
                        (unwind-protect (funcall original-filter proc bytes)
                          (when (eq ,server (process-get proc :server))
                            (ws-frame-test--put
                             ,state :received
                             (+ (or (ws-frame-test--get ,state :received) 0)
                                (string-bytes bytes))))
                          (ws-frame-test--put ,state :filter-depth depth)))))
                   ((symbol-function 'ws-parse-request)
                    (lambda (request)
                      (when (eq ,server (process-get (ws-process request) :server))
                        (ws-frame-test--put
                         ,state :max-pending
                         (max (or (ws-frame-test--get ,state :max-pending) 0)
                              (string-bytes (ws-pending request))))
                        (ws-frame-test--put ,state :seen-request request))
                      (funcall original-parse request))))
           (setq ,server (apply #'ws-start (ws-frame-test--handler ,state) 0
                                ws-frame-test--log-buffer :host "127.0.0.1"
                                ws-frame-test--network-args))
           (ignore timers-before) ; most controls do not inspect timer state
           ,@body)
       (mapc (lambda (p) (when (process-live-p p) (delete-process p)))
             ws-test-deferred--clients)
       (when (memq ,server ws-servers) (ws-stop ,server))
       ;; The per-test observer records timer/socket/registry leaks BEFORE
       ;; its forced safety cleanup.  Do not conceal them in this fixture.
       nil)))

(defun ws-frame-test--header (body &optional content-type extra)
  (concat "POST /frame HTTP/1.1\r\nHost: localhost\r\n"
          (when content-type (format "Content-Type: %s\r\n" content-type))
          (format "Content-Length: %d\r\n" (string-bytes body)) extra "\r\n"))
(defun ws-frame-test--feed (client bytes state)
  "Wait for actual receiver progress, never assume write equals read."
  (let ((target (+ (or (ws-frame-test--get state :received) 0)
                   (string-bytes bytes))))
    (process-send-string client bytes)
    (should (ws-test-deferred--wait
             (lambda () (>= (or (ws-frame-test--get state :received) 0) target)) 1))))
(defun ws-frame-test--closed (client)
  (should (ws-test-deferred--wait (lambda () (not (process-live-p client))) 1)))
(defun ws-frame-test--status (client code)
  (should (ws-test-deferred--wait
           (lambda () (string-match-p
                       (format "HTTP/[0-9.]+ %d " code)
                       (process-get client :output))) 1))
  (should (= 1 (ws-test-deferred--reply-count (process-get client :output)))))
(defun ws-frame-test--no-call (state)
  (should (= 0 (or (ws-frame-test--get state :calls) 0))))
(defun ws-frame-test--payload (kind)
  (pcase kind
    ('json (cons "application/json" (encode-coding-string "{\"text\":\"aé雪z\"}" 'utf-8 t)))
    ('url (cons "application/x-www-form-urlencoded" "a=one&b=two+words"))
    ('multipart (cons "multipart/form-data; boundary=frame-boundary"
                      (concat "--frame-boundary\r\nContent-Disposition: form-data; name=\"a\""
                              "\r\n\r\none\r\n--frame-boundary\r\n"
                              "Content-Disposition: form-data; name=\"b\"\r\n\r\ntwo"
                              "\r\n--frame-boundary--\r\n")))
    ('other (cons "application/octet-stream" (unibyte-string 0 255 13 10 42)))
    ('none (cons nil "untyped-body"))))
(defun ws-frame-test--schedule (kind schedule)
  (let* ((payload (ws-frame-test--payload kind))
         (body (cdr payload)) (header (ws-frame-test--header body (car payload))))
    (ws-frame-test--with state server
      (let ((client (ws-test-deferred--client (ws-test-deferred--port server))))
        (if (eq schedule 'one)
            (ws-frame-test--feed client (concat header body) state)
          (ws-frame-test--feed client header state)
          (ws-frame-test--no-call state)
          (if (eq schedule 'two)
              (ws-frame-test--feed client body state)
            (dotimes (i (length body))
              (ws-frame-test--feed client (substring body i (1+ i)) state)
              (when (< (1+ i) (length body))
                (ws-frame-test--no-call state)
                (let ((request (ws-frame-test--get state :seen-request)))
                  (should request)
                  (should-not (multibyte-string-p (ws-pending request)))
                  (should-not (ws-active request)))))))
        (ws-frame-test--status client 200)
        (ws-frame-test--closed client)
        (should (= 1 (ws-frame-test--get state :calls)))
        (should (equal body (ws-frame-test--get state :body)))
        (should-not (multibyte-string-p (ws-frame-test--get state :body)))
        (let ((headers (ws-frame-test--get state :headers)))
          (should (= 1 (cl-count :CONTENT-LENGTH headers :key #'car)))
          (when (eq kind 'url)
            (should (equal '("a" . "one") (assoc "a" headers)))
            (should (equal '("b" . "two words") (assoc "b" headers)))
            (should (= 1 (cl-count "a" headers :key #'car :test #'equal))))
          (when (eq kind 'multipart)
            (should (equal "one" (cdr (assq 'content (cdr (assoc "a" headers))))))
            (should (equal "two" (cdr (assq 'content (cdr (assoc "b" headers))))))))
        (should-not (ws-requests server))))))
(dolist (kind '(json url multipart other none))
  (dolist (schedule '(one two many))
    (eval `(ert-deftest ,(intern (format "ws/frame-%s-%s" kind schedule)) ()
             (ws-frame-test--schedule ',kind ',schedule)) t)))

(ert-deftest ws/frame-header-terminator-split ()
  (ws-frame-test--with state server
    (let ((client (ws-test-deferred--client (ws-test-deferred--port server)))
          (header (ws-frame-test--header "abc" "text/plain")))
      (ws-frame-test--feed client (substring header 0 (- (length header) 1)) state)
      (ws-frame-test--no-call state)
      (ws-frame-test--feed client (concat (substring header -1) "a") state)
      (ws-frame-test--no-call state)
      (ws-frame-test--feed client "bc" state)
      (ws-frame-test--status client 200)
      (should (equal "abc" (ws-frame-test--get state :body))))))

(ert-deftest ws/frame-larger-than-filter-chunk ()
  (let* ((ws-request-body-limit 200000)
         (body (encode-coding-string (concat (make-string 70000 ?x) "雪" (make-string 30000 ?y)) 'utf-8 t)))
    (ws-frame-test--with state server
      (let ((client (ws-test-deferred--client (ws-test-deferred--port server))))
        (ws-frame-test--feed client (ws-frame-test--header body "application/json") state)
        (ws-frame-test--no-call state)
        (cl-loop for start from 0 below (length body) by 16000
                 for end = (min (length body) (+ start 16000))
                 do (ws-frame-test--feed client (substring body start end) state)
                 when (< end (length body)) do (ws-frame-test--no-call state))
        (ws-frame-test--status client 200)
        (should (= 1 (ws-frame-test--get state :calls)))
        (should (equal body (ws-frame-test--get state :body)))))))

(defun ws-frame-test--refused (framing code &optional suffix)
  (ws-frame-test--with state server
    (let* ((client (ws-test-deferred--client (ws-test-deferred--port server)))
           (connection (ws-test-deferred--accepted-connection server)))
      ;; A real owned accepted socket with a deterministic completing argument:
      ;; explicit extras must not depend on how TCP happened to coalesce reads.
      (should connection)
      (ws-filter connection (concat "POST /frame HTTP/1.1\r\nHost: localhost\r\n"
                                    framing "\r\n" suffix))
      (ws-frame-test--status client code)
      (ws-frame-test--closed client)
      (ws-frame-test--no-call state)
      (should-not (ws-requests server))
      (should-not (ws-request-responder (ws-frame-test--get state :seen-request))))))
(dolist (row '((negative "Content-Length: -1\r\n" 400)
               (signed "Content-Length: +1\r\n" 400)
               (empty "Content-Length: \r\n" 400)
               (nondecimal "Content-Length: 1x\r\n" 400)
               (list "Content-Length: 1, 1\r\n" 400)
               (duplicate "Content-Length: 1\r\nContent-Length: 1\r\n" 400)
               (conflicting "Content-Length: 1\r\nContent-Length: 2\r\n" 400)
               (chunked "Transfer-Encoding: chunked\r\n" 501)
               (identity "Transfer-Encoding: identity\r\n" 501)
               (ambiguous "Content-Length: 0\r\nTransfer-Encoding: chunked\r\n" 400)
               (te-duplicate "Transfer-Encoding: chunked\r\nTransfer-Encoding: gzip\r\n" 400)
               (te-malformed "Transfer-Encoding: ,chunked\r\n" 400)
               (obs-fold "Content-Length: 0\r\n continued\r\n" 400)))
  (eval `(ert-deftest ,(intern (format "ws/frame-refuse-%s" (car row))) ()
           (ws-frame-test--refused ,(cadr row) ,(caddr row))) t))

(ert-deftest ws/frame-extra-coalesced-refused ()
  (ws-frame-test--refused "Content-Length: 0\r\n" 400 "GET /second HTTP/1.1\r\n\r\n"))
(ert-deftest ws/frame-body-absolute-limit ()
  (let ((ws-request-body-limit 7))
    (ws-frame-test--refused "Content-Length: 8\r\n" 413)))
(ert-deftest ws/frame-huge-decimal-limit ()
  (let ((ws-request-body-limit 7))
    (ws-frame-test--refused (concat "Content-Length: " (make-string 2000 ?9) "\r\n") 413)))
(ert-deftest ws/frame-header-absolute-limit ()
  (let ((ws-request-header-limit 80))
    (ws-frame-test--refused (concat "X-Padding: " (make-string 100 ?x) "\r\n") 431)))
(defun ws-frame-test--bounded-filter-call (state connection argument limit)
  "Observe retained data AND transient string copies in one real filter call.
ARGUMENT is constructed before the measurement.  String-chars counts all
allocated string characters, including temporaries discarded before parsing.
Allocation budget is k=3 times the configured header+body limits plus 8192
fixed string characters.  Three covers pending concat, header parsing and
an optional raw-target substring; the slack covers bounded reply overhead."
  (garbage-collect)
  (let ((allocation-limit (+ (* 3 (+ ws-request-header-limit ws-request-body-limit)) 8192))
        (before (nth 4 (memory-use-counts))))
    (ws-filter connection argument)
    (let ((allocated (- (nth 4 (memory-use-counts)) before))
          (sample (ws-frame-test--get state :max-pending)))
      (message "FRAME-ALLOCATION input=%d allocated=%d pending=%S limit=%d allocation-limit=%d"
               (length argument) allocated sample limit allocation-limit)
      ;; A parser bypass must not manufacture an apparent zero sample.
      (should sample)
      (should (<= sample limit))
      ;; Reject both a full copy and a capped prefix exceeding the configured
      ;; retained budget by 64 KiB, even if discarded before parsing.
      (should (< allocated allocation-limit)))))

(ert-deftest ws/frame-retained-buffer-bounded ()
  (let ((ws-request-header-limit 128) (ws-request-body-limit 32))
    (ws-frame-test--with state server
      (let* ((client (ws-test-deferred--client (ws-test-deferred--port server)))
             (connection (ws-test-deferred--accepted-connection server)))
        (ws-frame-test--bounded-filter-call
         state connection
         (concat (ws-frame-test--header "abc" "text/plain") (make-string 1000000 ?x)) 160)
        (ws-frame-test--status client 400)
        (ws-frame-test--no-call state)))))

(ert-deftest ws/frame-zero-and-missing-length ()
  (dolist (field '("Content-Length: 0\r\n" ""))
    (ws-frame-test--with state server
      (let ((client (ws-test-deferred--client (ws-test-deferred--port server))))
        (ws-frame-test--feed client (concat "POST /frame HTTP/1.1\r\n" field "\r\n") state)
        (ws-frame-test--status client 200)
        (should (= 1 (ws-frame-test--get state :calls)))
        (should (equal "" (ws-frame-test--get state :body)))))))
(ert-deftest ws/frame-ows-and-case-length ()
  (ws-frame-test--with state server
    (let ((client (ws-test-deferred--client (ws-test-deferred--port server))))
      (ws-frame-test--feed client "POST /frame HTTP/1.1\r\ncOnTeNt-LeNgTh:\t0003 \t\r\n\r\nabc" state)
      (ws-frame-test--status client 200)
      (should (equal "abc" (ws-frame-test--get state :body))))))
(ert-deftest ws/frame-exact-limit-and-frozen-byte-settings ()
  (let* ((body "1234567") (header (ws-frame-test--header body))
         (ws-request-body-limit 7) (ws-request-header-limit (length header)))
    (ws-frame-test--with state server
      (let ((ws-request-body-limit 0) (ws-request-header-limit 1)
            (client (ws-test-deferred--client (ws-test-deferred--port server))))
        (ws-frame-test--feed client (concat header body) state)
        (ws-frame-test--status client 200)
        (should (equal body (ws-frame-test--get state :body)))))))

(defun ws-frame-test--partial (state server)
  (let ((client (ws-test-deferred--client (ws-test-deferred--port server))))
    (ws-frame-test--feed client (concat (ws-frame-test--header "abcdef" "application/json") "a") state)
    (ws-frame-test--no-call state)
    client))
(ert-deftest ws/frame-incomplete-eof-retires ()
  (let ((ws-request-framing-timeout .5))
    (ws-frame-test--with state server
      (let ((client (ws-frame-test--partial state server)))
        (process-send-eof client)
        (should (ws-test-deferred--wait (lambda () (not (ws-requests server))) .3))
        (ws-frame-test--no-call state)
        (should-not (ws-frame-test--live-timers timers-before))))))



(ert-deftest ws/frame-stop-retires-incomplete ()
  (let ((ws-request-framing-timeout .3))
    (ws-frame-test--with state server
      (ws-frame-test--partial state server)
      (ws-stop server)
      (should-not (ws-requests server))
      (should-not (ws-frame-test--live-timers timers-before))
      (ws-frame-test--no-call state))))




(dolist (row '((body-negative ws-request-body-limit -1)
               (body-nil ws-request-body-limit nil)
               (body-fraction ws-request-body-limit 1.5)
               (header-zero ws-request-header-limit 0)
               (header-nil ws-request-header-limit nil)
               (time-zero ws-request-framing-timeout 0)
               (time-negative ws-request-framing-timeout -1)
               (time-nil ws-request-framing-timeout nil)
               (time-infinite ws-request-framing-timeout 1.0e+INF)
               (time-nan ws-request-framing-timeout 0.0e+NaN)
               (cap-zero ws-request-incomplete-limit 0)
               (cap-nil ws-request-incomplete-limit nil)
               (cap-negative ws-request-incomplete-limit -1)
               (cap-fraction ws-request-incomplete-limit 1.5)
               (cap-string ws-request-incomplete-limit "unlimited")))
  (eval `(ert-deftest ,(intern (format "ws/frame-setting-%s" (car row))) ()
           (let ((network-calls 0) (server nil) (condition nil)
                 (original-network (symbol-function 'make-network-process)))
             (cl-letf (((symbol-function 'make-network-process)
                        (lambda (&rest arguments) (cl-incf network-calls)
                          (apply original-network arguments))))
               (unwind-protect
                   (let ((,(cadr row) ,(caddr row)))
                     (setq condition
                           (condition-case error
                               (progn (setq server (ws-start #'ignore 0 nil :host "127.0.0.1")) nil)
                             (error error)))
                     (should (eq 'ws-request-setting-error (car-safe condition)))
                     (should (= 0 network-calls)))
                 (when server (ws-stop server)))))) t))

(ert-deftest ws/frame-missing-length-body-is-unframed ()
  (ws-frame-test--refused "" 400 "close-delimited-body"))
(ert-deftest ws/frame-incomplete-header-buffer-bounded ()
  (let ((ws-request-header-limit 128) (ws-request-body-limit 32))
    (ws-frame-test--with state server
      (let* ((client (ws-test-deferred--client (ws-test-deferred--port server)))
             (connection (ws-test-deferred--accepted-connection server)))
        (ws-frame-test--bounded-filter-call
         state connection
         (concat "POST /frame HTTP/1.1\r\nX: " (make-string 1000000 ?x)) 128)
        (ws-frame-test--status client 431)
        (ws-frame-test--no-call state)))))
(ert-deftest ws/frame-split-completion-preserves-deferred-disconnect ()
  (let ((responder nil) (closed nil))
    (ws-frame-test--with state server
      (setf (ws-handlers server)
            (lambda (request)
              (setq responder (ws-defer request :on-close (lambda (_responder reason) (push reason closed))))))
      (let ((client (ws-test-deferred--client (ws-test-deferred--port server))))
        (ws-frame-test--feed client (ws-frame-test--header "abc") state)
        (should-not responder)
        (ws-frame-test--feed client "abc" state)
        (should responder)
        (delete-process client)
        (should (ws-test-deferred--wait (lambda () closed) .5))
        (should (equal '(disconnected) closed))
        (should (eq 'disconnected (ws-responder-state responder)))
        (should-not (ws-requests server))
        (should-not (ws-frame-test--live-timers timers-before))))))



(ert-deftest ws/frame-header-terminal-octet-counts ()
  (let* ((header (ws-frame-test--header ""))
         (ws-request-header-limit (1- (string-bytes header))))
    (ws-frame-test--with state server
      (let ((client (ws-test-deferred--client (ws-test-deferred--port server))))
        (ws-frame-test--feed client header state)
        (ws-frame-test--status client 431)
        (ws-frame-test--no-call state)))))





(ert-deftest ws/frame-malformed-header-refused ()
  (ws-frame-test--refused "Not-A-Header\r\nContent-Length: 0\r\n" 400))


(ert-deftest ws/frame-valid-transfer-parameters-unsupported ()
  "Valid token and quoted parameter syntax is unsupported501, not malformed400.
See RFC9110 10.1.4/5.6.4 and RFC9112 6.1; none enters the handler."
  (dolist (value '("gzip; p=x, chunked" "gzip; p=\"x,y\", chunked" "gzip; p=\"x\\\"y\", chunked"))
    (ws-frame-test--refused (concat "Transfer-Encoding: " value "\r\n") 501)))

(ert-deftest ws/frame-lone-cr-header-refused ()
  "A bare CR inside a field value is malformed and never reaches a handler."
  (ws-frame-test--refused "X-Field: good\rbad\r\nContent-Length: 0\r\n" 400))

;;; deadline-controls.el --- Proposed input-path deadline controls -*- lexical-binding: t; -*-
;; Input-path deadline controls; no autonomous scheduling.

(ert-deftest ws/frame-clock-seam-observed ()
  (let ((now 1000) (reads 0))
    (cl-letf (((symbol-function 'ws--frame-now)
               (lambda () (cl-incf reads) now)))
      (ws-frame-test--with state server
        (let ((client (ws-test-deferred--client (ws-test-deferred--port server))))
          (ws-frame-test--feed client (concat (ws-frame-test--header "abc") "abc") state)
          (ws-frame-test--status client 200)
          (should (> reads 0))
          (should (= 1 (ws-frame-test--get state :calls))))))))

(ert-deftest ws/frame-deadline-starts-at-first-input ()
  (let ((now 1000) (ws-request-framing-timeout 30))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let ((client (car (ws-frame-test--zero-client server))))
          (setq now 1100)
          (ws-frame-test--feed client (concat (ws-frame-test--header "abc") "abc") state)
          (ws-frame-test--status client 200)
          (should (= 1 (ws-frame-test--get state :calls))))))))

(ert-deftest ws/frame-silent-peer-waits-for-more-input ()
  "Expiry does not autonomously close a silent peer; retained bytes stay bounded."
  (let ((now 1000) (ws-request-framing-timeout 30))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let ((client (ws-frame-test--partial state server)))
          (setq now 1100)
          (accept-process-output nil 0)
          (should (process-live-p client))
          (should (equal "" (process-get client :output)))
          (ws-frame-test--no-call state)
          (should (ws-frame-test--get state :max-pending))
          (should (<= (ws-frame-test--get state :max-pending)
                      (+ ws-request-header-limit ws-request-body-limit)))
          (should-not (ws-frame-test--live-timers timers-before))
          (ws-frame-test--feed client "bcdef" state)
          (ws-frame-test--status client 408)
          (ws-frame-test--closed client)
          (ws-frame-test--no-call state)
          (should-not (ws-requests server)))))))

(ert-deftest ws/frame-incomplete-header-expires-on-input ()
  (let ((now 1000) (ws-request-framing-timeout 30))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let ((client (ws-test-deferred--client (ws-test-deferred--port server))))
          (ws-frame-test--feed client "POST /frame HTTP/1.1\r\nHost:" state)
          (ws-frame-test--no-call state)
          (setq now 1100)
          (should (process-live-p client))
          (should (equal "" (process-get client :output)))
          (should-not (ws-frame-test--live-timers timers-before))
          (ws-frame-test--feed client " localhost\r\nContent-Length: 0\r\n\r\n" state)
          (ws-frame-test--status client 408)
          (ws-frame-test--closed client)
          (ws-frame-test--no-call state))))))

(ert-deftest ws/frame-slow-drip-does-not-renew-deadline ()
  (let ((now 1000) (ws-request-framing-timeout 30))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let ((client (ws-frame-test--partial state server)))
          (setq now 1020)
          (ws-frame-test--feed client "b" state)
          (ws-frame-test--no-call state)
          (setq now 1031)
          (ws-frame-test--feed client "cdef" state)
          (ws-frame-test--status client 408)
          (ws-frame-test--closed client)
          (ws-frame-test--no-call state)
          (should-not (ws-requests server)))))))

(ert-deftest ws/frame-late-completion-cannot-beat-deadline ()
  (let ((now 1000) (ws-request-framing-timeout 30))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let ((client (ws-frame-test--partial state server)))
          (setq now 1031)
          (ws-frame-test--feed client "bcdef" state)
          (ws-frame-test--status client 408)
          (ws-frame-test--closed client)
          (ws-frame-test--no-call state))))))

(ert-deftest ws/frame-input-deadline-setting-is-frozen ()
  (let ((now 1000) (ws-request-framing-timeout 30))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let* ((ws-request-framing-timeout 1000)
               (client (ws-frame-test--partial state server)))
          (setq now 1031)
          (ws-frame-test--feed client "bcdef" state)
          (ws-frame-test--status client 408)
          (ws-frame-test--no-call state))))))

(ert-deftest ws/frame-late-write-failure-retires ()
  (let ((now 1000) (ws-request-framing-timeout 30))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let ((client (ws-frame-test--partial state server))
              (original-send (symbol-function 'process-send-string)) (attempts 0))
          (setq now 1031)
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (proc text)
                       (if (eq server (process-get proc :server))
                           (progn (cl-incf attempts) (error "injected closed-write race"))
                         (funcall original-send proc text)))))
            (ws-frame-test--feed client "bcdef" state)
            (ws-frame-test--closed client))
          (should (= 1 attempts))
          (should (equal "" (process-get client :output)))
          (ws-frame-test--no-call state)
          (should-not (ws-requests server))
          (should-not (ws-frame-test--live-timers timers-before)))))))

(ert-deftest ws/frame-completed-deferred-request-has-no-framing-deadline ()
  (let ((now 1000) (ws-request-framing-timeout 30) (responder nil))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (setf (ws-handlers server) (lambda (request) (setq responder (ws-defer request))))
        (let ((client (ws-test-deferred--client (ws-test-deferred--port server))))
          (ws-frame-test--feed client (ws-frame-test--header "") state)
          (should responder)
          (setq now 1100)
          (ws-frame-test--feed client "later input" state)
          (should (process-live-p client))
          (should (equal "" (process-get client :output)))
          (should (eq 'pending (ws-responder-state responder)))
          (should-not (ws-frame-test--live-timers timers-before))
          (should (ws-respond responder 200 nil "done"))
          (ws-frame-test--status client 200)
          (should (eq 'completed (ws-responder-state responder))))))))

(ert-deftest ws/frame-partial-completion-creates-no-timer-before-defer ()
  (let ((responder nil))
    (ws-frame-test--with state server
      (setf (ws-handlers server) (lambda (request) (setq responder (ws-defer request))))
      (let ((client (ws-test-deferred--client (ws-test-deferred--port server))))
        (ws-frame-test--feed client (ws-frame-test--header "abc") state)
        (should-not responder)
        (should-not (ws-frame-test--live-timers timers-before))
        (ws-frame-test--feed client "abc" state)
        (should responder)
        (should-not (ws-frame-test--live-timers timers-before))
        (should (ws-respond responder 200 nil "done"))
        (ws-frame-test--status client 200)))))

(ert-deftest ws/frame-refusals-share-input-path-and-thread ()
  (dolist (code '(400 413 431 408))
    (let ((now 1000) (ws-request-framing-timeout 30)
          (ws-request-body-limit 7) (ws-request-header-limit 128))
      (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
        (ws-frame-test--with state server
          (let ((client (ws-test-deferred--client (ws-test-deferred--port server)))
                (writes nil) (deletes nil)
                (original-send (symbol-function 'process-send-string))
                (original-delete (symbol-function 'delete-process)))
            (cl-letf (((symbol-function 'process-send-string)
                       (lambda (proc text)
                         (when (and (processp proc) (eq server (process-get proc :server)))
                           (push (list (current-thread) (ws-frame-test--get state :filter-depth)) writes))
                         (funcall original-send proc text)))
                      ((symbol-function 'delete-process)
                       (lambda (proc)
                         (when (and (processp proc) (eq server (process-get proc :server)))
                           (push (list (current-thread) (ws-frame-test--get state :filter-depth)) deletes))
                         (funcall original-delete proc))))
              (if (= code 408)
                  (progn
                    (ws-frame-test--feed client (concat (ws-frame-test--header "abcdef") "a") state)
                    (ws-frame-test--no-call state)
                    (setq now 1031)
                    (ws-frame-test--feed client "bcdef" state))
                (ws-frame-test--feed
                 client
                 (concat "POST /frame HTTP/1.1\r\n"
                         (pcase code
                           (400 "Content-Length: -1\r\n")
                           (413 "Content-Length: 8\r\n")
                           (431 (concat "X-Padding: " (make-string 200 ?x) "\r\n")))
                         "\r\n") state))
              (ws-frame-test--status client code)
              (ws-frame-test--closed client))
            (ws-frame-test--no-call state)
            (should writes)
            (should deletes)
            (dolist (entry (append writes deletes))
              (should (eq (car entry) (ws-frame-test--get state :filter-thread)))
              (should (> (or (cadr entry) 0) 0)))
            (should-not (ws-requests server))))))))

;;; cap-controls.el --- Bounded timer-free framing admission -*- lexical-binding: t; -*-

(defun ws-frame-test--connections (server)
  "Return live accepted sockets belonging to SERVER, excluding its listener."
  (cl-remove-if-not (lambda (proc)
                      (and (eq server (process-get proc :server))
                           (not (eq proc (ws-process server)))
                           (process-live-p proc)))
                    (process-list)))
(defun ws-frame-test--zero-client (server)
  "Open a zero-byte peer; return (CLIENT . ACCEPTED-SERVER-PROCESS)."
  (let* ((before (ws-frame-test--connections server))
         (client (ws-test-deferred--client (ws-test-deferred--port server)))
         (accepted (ws-test-deferred--wait
                    (lambda () (cl-find-if (lambda (proc) (not (memq proc before)))
                                           (ws-frame-test--connections server))) 1)))
    (should accepted)
    (cons client accepted)))
(defun ws-frame-test--rejected-client (server &optional wire)
  "A cap refusal closes the new peer without HTTP bytes or request dispatch."
  (let ((client (ws-test-deferred--client (ws-test-deferred--port server) wire)))
    (ws-frame-test--closed client)
    (should (equal "" (process-get client :output)))
    client))
(defun ws-frame-test--successful-client (state server)
  (let ((client (ws-test-deferred--client (ws-test-deferred--port server))))
    (ws-frame-test--feed client (concat (ws-frame-test--header "abc") "abc") state)
    (ws-frame-test--status client 200)
    (ws-frame-test--closed client)
    client))

(ert-deftest ws/frame-default-cap-counts-zero-byte-peers ()
  "Default16 caps framing ownership, including accepted peers with no input."
  (ws-frame-test--with state server
    (let ((peers nil))
      (dotimes (_ 16) (push (car (ws-frame-test--zero-client server)) peers))
      (ws-frame-test--rejected-client server)
      (should (cl-every #'process-live-p peers))
      (should-not (ws-requests server))
      (ws-frame-test--no-call state))))

(ert-deftest ws/frame-cap-is-frozen-and-refusal-has-no-request-effect ()
  (let ((ws-request-incomplete-limit 1))
    (ws-frame-test--with state server
      (let* ((peer (ws-frame-test--zero-client server))
             (before (copy-sequence (ws-requests server)))
             (registry-before (ws-frame-test--registry-count server))
             (allocations 0) (original-instance (symbol-function 'make-instance))
             (ws-request-incomplete-limit 100))
        (cl-letf (((symbol-function 'make-instance)
                   (lambda (class &rest arguments)
                     (when (eq class 'ws-request) (cl-incf allocations))
                     (apply original-instance class arguments))))
          (ws-frame-test--rejected-client
           server (concat (ws-frame-test--header "abc") "abc")))
        (should (= 0 allocations))
        (should (equal registry-before (ws-frame-test--registry-count server)))
        (should (equal before (ws-requests server)))
        (should (process-live-p (car peer)))
        (ws-frame-test--no-call state)))))

(ert-deftest ws/frame-cap-counts-partial-body ()
  (let ((ws-request-incomplete-limit 1))
    (ws-frame-test--with state server
      (let ((client (ws-frame-test--partial state server)))
        (ws-frame-test--rejected-client server)
        (should (process-live-p client))
        (ws-frame-test--no-call state)))))

(ert-deftest ws/frame-admission-reclaims-expired-zero-byte-only ()
  (let ((now 1000) (ws-request-framing-timeout 30) (ws-request-incomplete-limit 2))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let ((a (car (ws-frame-test--zero-client server))))
          (setq now 1020)
          (let ((b (car (ws-frame-test--zero-client server))))
            (setq now 1031)
            (let ((c (car (ws-frame-test--zero-client server))))
              (ws-frame-test--closed a)
              (should (equal "" (process-get a :output)))
              (should (process-live-p b))
              (should (process-live-p c))
              (ws-frame-test--no-call state))))))))

(ert-deftest ws/frame-admission-uses-original-partial-deadline ()
  (let ((now 1000) (ws-request-framing-timeout 30) (ws-request-incomplete-limit 1))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let ((a (ws-frame-test--partial state server)))
          (setq now 1020)
          (ws-frame-test--feed a "b" state)
          (ws-frame-test--no-call state)
          (setq now 1031)
          (let ((b (car (ws-frame-test--zero-client server))))
            (ws-frame-test--closed a)
            (should (equal "" (process-get a :output)))
            (should (process-live-p b))
            (ws-frame-test--no-call state)))))))

(ert-deftest ws/frame-eof-release-permits-legitimate-request ()
  (let ((ws-request-incomplete-limit 1))
    (ws-frame-test--with state server
      (let* ((peer (ws-frame-test--zero-client server))
             (client (car peer)) (connection (cdr peer)))
        (delete-process client)
        (should (ws-test-deferred--wait (lambda () (not (process-live-p connection))) 1))
        (ws-frame-test--successful-client state server)
        (should (= 1 (ws-frame-test--get state :calls)))))))

(ert-deftest ws/frame-completion-release-permits-legitimate-request ()
  (let ((ws-request-incomplete-limit 1))
    (ws-frame-test--with state server
      (ws-frame-test--successful-client state server)
      (ws-frame-test--successful-client state server)
      (should (= 2 (ws-frame-test--get state :calls))))))

(ert-deftest ws/frame-repeat-release-does-not-release-another-peer ()
  (let ((ws-request-incomplete-limit 1))
    (ws-frame-test--with state server
      (let* ((a (ws-frame-test--zero-client server))
             (old-sentinel (process-sentinel (cdr a))))
        (should (functionp old-sentinel))
        (ws-frame-test--feed (car a) (concat (ws-frame-test--header "abc") "abc") state)
        (ws-frame-test--status (car a) 200)
        (ws-frame-test--closed (car a))
        (let ((b (car (ws-frame-test--zero-client server))))
          ;; Replay a queued old EOF callback after completion and reuse of the
          ;; released capacity.  It must not release B's independent slot.
          (when old-sentinel
            (funcall old-sentinel (cdr a) "connection closed\n")
            (funcall old-sentinel (cdr a) "connection closed\n"))
          (ws-frame-test--rejected-client server)
          (should (process-live-p b))
          (should (= 1 (ws-frame-test--get state :calls))))))))

(ert-deftest ws/frame-admission-does-not-evict-deferred-response ()
  (let ((now 1000) (ws-request-framing-timeout 30)
        (ws-request-incomplete-limit 1) (responder nil))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (setf (ws-handlers server) (lambda (request) (setq responder (ws-defer request))))
        (let ((a (ws-test-deferred--client (ws-test-deferred--port server))))
          (ws-frame-test--feed a (ws-frame-test--header "") state)
          (should responder)
          (setq now 1100)
          (let ((b (car (ws-frame-test--zero-client server))))
            (ws-frame-test--rejected-client server)
            (should (process-live-p a))
            (should (process-live-p b))
            (should (eq 'pending (ws-responder-state responder)))
            (should (ws-respond responder 200 nil "done"))
            (ws-frame-test--status a 200)
            (should (eq 'completed (ws-responder-state responder)))))))))

(ert-deftest ws/frame-admission-does-not-evict-dispatching-handler ()
  (let ((now 1000) (ws-request-framing-timeout 30) (ws-request-incomplete-limit 1))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (setf (ws-handlers server)
              (lambda (request)
                (setq now 1100)
                (let ((b (car (ws-frame-test--zero-client server))))
                  (should (process-live-p (ws-process request)))
                  (should (process-live-p b)))
                (funcall (ws-frame-test--handler state) request)))
        (ws-frame-test--successful-client state server)
        (should (= 1 (ws-frame-test--get state :calls)))))))

(ert-deftest ws/frame-stop-releases-zero-byte-and-partial-sockets ()
  (let ((ws-request-incomplete-limit 2))
    (ws-frame-test--with state server
      (let ((a (car (ws-frame-test--zero-client server)))
            (b (ws-frame-test--partial state server)))
        (ws-stop server)
        (ws-frame-test--closed a)
        (ws-frame-test--closed b)
        (should-not (ws-frame-test--connections server))
        (should-not (ws-requests server))
        (should-not (ws-frame-test--live-timers timers-before))
        (ws-frame-test--no-call state)))))

(ert-deftest ws/frame-framing-eof-preserves-network-sentinel ()
  (let* ((notices 0) (ws-request-incomplete-limit 1)
         (ws-frame-test--network-args
          (list :sentinel (lambda (proc _event)
                            (unless (process-live-p proc) (cl-incf notices))))))
    (ws-frame-test--with state server
      (let ((a (car (ws-frame-test--zero-client server))))
        (delete-process a)
        (should (ws-test-deferred--wait (lambda () (= 1 notices)) 1))
        (ws-frame-test--successful-client state server)
        (should (= 1 (ws-frame-test--get state :calls)))))))

;;; cap-extra-controls.el --- Admission specification delta -*- lexical-binding: t; -*-
(defun ws-frame-test--registry-count (server)
  "Return the real proposed registry size, nil when the baseline lacks it."
  (when (slot-exists-p server 'frame-connections)
    (hash-table-count (slot-value server 'frame-connections))))

(ert-deftest ws/frame-default-pending-bound-derived ()
  ;; Fallback values alone prove nothing.  All four settings must originate
  ;; in the actual production module, before any default arithmetic counts.
  (dolist (variable '(ws-request-body-limit ws-request-header-limit
                                            ws-request-framing-timeout ws-request-incomplete-limit))
    (should (equal (symbol-file variable 'defvar)
                   (symbol-file 'ws-start 'defun))))
  (should (= 30 ws-request-framing-timeout))
  (should (= 16 ws-request-incomplete-limit))
  (should (= 8388608 ws-request-body-limit))
  (should (= 65536 ws-request-header-limit))
  (let ((pending-bound (* ws-request-incomplete-limit
                          (+ ws-request-body-limit ws-request-header-limit))))
    (should (= (* 129 1024 1024) pending-bound))))

(ert-deftest ws/frame-cap-counts-partial-header ()
  (let ((ws-request-incomplete-limit 1))
    (ws-frame-test--with state server
      (let ((client (ws-test-deferred--client (ws-test-deferred--port server))))
        (ws-frame-test--feed client "POST /frame HTTP/1.1\r\nHost:" state)
        (ws-frame-test--rejected-client server)
        (should (process-live-p client))
        (ws-frame-test--no-call state)))))

(ert-deftest ws/frame-refusal-release-permits-legitimate-request ()
  (let ((ws-request-incomplete-limit 1))
    (ws-frame-test--with state server
      (let ((client (ws-test-deferred--client (ws-test-deferred--port server))))
        (ws-frame-test--feed client "POST /frame HTTP/1.1\r\nContent-Length: invalid\r\n\r\n" state)
        (ws-frame-test--status client 400)
        (ws-frame-test--closed client)
        (ws-frame-test--no-call state)
        (ws-frame-test--successful-client state server)
        (should (= 1 (ws-frame-test--get state :calls)))))))

(ert-deftest ws/frame-disabled-nil-client-has-no-effects ()
  "A nil accept failure never treats nil as a process, even behind containment."
  (let ((now 1000) (ws-request-framing-timeout 30))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let* ((peer (car (ws-frame-test--zero-client server)))
               (callback (process-contact (ws-process server) :log))
               (buffers (buffer-list))
               (count (ws-frame-test--registry-count server))
               (nil-process-calls nil)
               (originals (mapcar (lambda (name) (cons name (symbol-function name)))
                                  '(process-sentinel set-process-sentinel delete-process
                                                     process-get process-put process-live-p process-status
                                                     process-contact))))
          (setq now 1100)
          (should (functionp callback))
          (cl-labels ((instrument (name)
                        (let ((original (cdr (assq name originals))))
                          (lambda (&rest args)
                            (unless (car args) (push name nil-process-calls))
                            (apply original args)))))
            ;; Count only: every call delegates to its unchanged original.
            (cl-letf (((symbol-function 'process-sentinel) (instrument 'process-sentinel))
                      ((symbol-function 'set-process-sentinel) (instrument 'set-process-sentinel))
                      ((symbol-function 'delete-process) (instrument 'delete-process))
                      ((symbol-function 'process-get) (instrument 'process-get))
                      ((symbol-function 'process-put) (instrument 'process-put))
                      ((symbol-function 'process-live-p) (instrument 'process-live-p))
                      ((symbol-function 'process-status) (instrument 'process-status))
                      ((symbol-function 'process-contact) (instrument 'process-contact)))
              (should-not (condition-case condition
                              (progn (funcall callback (ws-process server) nil
                                              "accept failed with code24\n") nil)
                            (ert-test-failed (signal (car condition) (cdr condition)))
                            (error condition)))))
          ;; Contract-instrument assertion: P1 containment masks the failed
          ;; nil operation externally.  This is not an external-effect claim.
          (should-not nil-process-calls)
          (should (process-live-p peer))
          (should (equal count (ws-frame-test--registry-count server)))
          (should (equal buffers (buffer-list)))
          (should-not (plist-member (process-plist (ws-process server)) :log-buffer))
          (should-not (ws-requests server))
          (ws-frame-test--no-call state))))))

(ert-deftest ws/frame-refused-accept-preserves-log-shape ()
  (let ((ws-request-incomplete-limit 1)
        (ws-frame-test--log-buffer (generate-new-buffer-name " *ws-frame-log*")))
    (unwind-protect
        (ws-frame-test--with state server
          (let* ((a (car (ws-frame-test--zero-client server)))
                 (b (ws-frame-test--rejected-client server))
                 (ports (mapcar (lambda (p) (aref (process-contact p :local) 4)) (list a b)))
                 (lines (split-string (ws-test-log--text ws-frame-test--log-buffer) "\n" t)))
            (should (= 2 (length lines)))
            (cl-mapc (lambda (line port)
                       (should (string-match-p
                                (format "\\`[^\t\n]+\t127\\.0\\.0\\.1\t%d\taccept from 127\\.0\\.0\\.1\\'" port)
                                line))) lines ports)
            (should (= 1 (ws-frame-test--registry-count server)))
            (should-not (ws-requests server))
            (ws-frame-test--no-call state)))
      (when (get-buffer ws-frame-test--log-buffer) (kill-buffer ws-frame-test--log-buffer)))))

(ert-deftest ws/frame-stop-fences-reentrant-admission ()
  "Replay an accept callback delayed until after stop: it must close the peer."
  (let ((original-network (symbol-function 'make-network-process))
        (late nil) (accepts 0) (ws-request-incomplete-limit 2))
    (cl-letf (((symbol-function 'make-network-process)
               (lambda (&rest arguments)
                 (let ((callback (plist-get arguments :log)))
                   (when callback
                     (setq arguments
                           (plist-put arguments :log
                                      (lambda (listener client event)
                                        (if (and client (= 2 (cl-incf accepts)))
                                            (setq late (list callback listener client event))
                                          (funcall callback listener client event))))))
                   (apply original-network arguments)))))
      (ws-frame-test--with state server
        (let ((a (car (ws-frame-test--zero-client server)))
              (b (car (ws-frame-test--zero-client server))))
          (should late)
          (ws-stop server)
          (apply (car late) (cdr late))
          (ws-frame-test--closed a)
          (ws-frame-test--closed b)
          (should-not (ws-requests server))
          (should (equal 0 (ws-frame-test--registry-count server)))
          (ws-frame-test--no-call state))))))


;;; admission-reentry-controls.el --- P1-P3 admission contract -*- lexical-binding: t; -*-
(ert-deftest ws/frame-admission-error-is-contained-and-fail-closed ()
  "One failed sentinel install after reservation cannot escape the waiter."
  (ws-frame-test--with state server
    (let ((original (symbol-function 'set-process-sentinel))
          (injected 0) (registered-at-failure nil) (escaped nil) (client nil))
      (cl-letf (((symbol-function 'set-process-sentinel)
                 (lambda (proc sentinel)
                   (when (and (= injected 0)
                              (eq server (process-get proc :server))
                              (not (eq proc (ws-process server))))
                     (cl-incf injected)
                     (setq registered-at-failure
                           (and (slot-exists-p server 'frame-connections)
                                (gethash proc (slot-value server 'frame-connections))))
                     (error "6kzz injected framing-sentinel installation error"))
                   (funcall original proc sentinel))))
        (setq escaped
              (condition-case condition
                  (progn
                    (setq client (ws-test-deferred--client (ws-test-deferred--port server)))
                    (ws-test-deferred--wait (lambda () (> injected 0)) 1)
                    nil)
                (ert-test-failed (signal (car condition) (cdr condition)))
                (error condition)))
        (should (= 1 injected))
        (should registered-at-failure)
        (should-not escaped)
        (ws-frame-test--closed client)
        (should (equal "" (process-get client :output)))
        (should (equal 0 (ws-frame-test--registry-count server)))
        (should-not (ws-requests server))
        (ws-frame-test--no-call state)))))

(ert-deftest ws/frame-reclaim-rechecks-count-after-reentrant-admission ()
  (let ((now 1000) (ws-request-framing-timeout 30) (ws-request-incomplete-limit 2))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let* ((a (ws-frame-test--zero-client server))
               (original (symbol-function 'delete-process))
               (reentered nil) (removed-first nil) (d nil))
          (setq now 1020)
          (let ((b (car (ws-frame-test--zero-client server))))
            (setq now 1031)
            (cl-letf (((symbol-function 'delete-process)
                       (lambda (proc)
                         (when (and (eq proc (cdr a)) (not reentered))
                           (setq reentered t)
                           (setq removed-first
                                 (not (gethash proc (slot-value server 'frame-connections))))
                           ;; Reproduce a nested accept during status_notify.
                           (setq d (car (ws-frame-test--zero-client server))))
                         (funcall original proc))))
              (let ((c (ws-test-deferred--client (ws-test-deferred--port server))))
                (should (ws-test-deferred--wait (lambda () reentered) 1))
                (should removed-first)
                (ws-frame-test--closed c)
                (should (equal "" (process-get c :output)))
                (should (process-live-p b))
                (should (process-live-p d))
                (should (equal 2 (ws-frame-test--registry-count server)))
                (should-not (ws-requests server))
                (ws-frame-test--no-call state)))))))))

(ert-deftest ws/frame-reclaim-rechecks-stop-fence-after-reentry ()
  (let ((now 1000) (ws-request-framing-timeout 30) (ws-request-incomplete-limit 1))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let* ((a (ws-frame-test--zero-client server))
               (original (symbol-function 'delete-process)) (reentered nil))
          (setq now 1031)
          (cl-letf (((symbol-function 'delete-process)
                     (lambda (proc)
                       (when (and (eq proc (cdr a)) (not reentered))
                         (setq reentered t)
                         (ws-stop server))
                       (funcall original proc))))
            (let ((c (ws-test-deferred--client (ws-test-deferred--port server))))
              (should (ws-test-deferred--wait (lambda () reentered) 1))
              (ws-frame-test--closed c)
              (should (equal "" (process-get c :output)))
              (should (equal 0 (ws-frame-test--registry-count server)))
              (should-not (ws-requests server))
              (ws-frame-test--no-call state))))))))

(ert-deftest ws/frame-reclaim-skips-peer-dispatched-during-reentry ()
  (let ((now 1000) (ws-request-framing-timeout 30) (ws-request-incomplete-limit 2))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let* ((a (ws-frame-test--zero-client server))
               (b (ws-frame-test--zero-client server))
               (original (symbol-function 'delete-process)) (reentered nil) (responder nil))
          (setf (ws-handlers server) (lambda (request) (setq responder (ws-defer request))))
          (setq now 1031)
          (cl-letf (((symbol-function 'delete-process)
                     (lambda (proc)
                       (when (and (eq proc (cdr a)) (not reentered))
                         (setq reentered t)
                         ;; B completes on its first input during the close of A.
                         ;; Its old snapshot entry must not be reaped afterward.
                         (ws-filter (cdr b) (ws-frame-test--header ""))
                         ;; Real callbacks can consume time.  Advance beyond
                         ;; B's now-completed first-input deadline too, so a
                         ;; fresh deadline cannot mask stale eligibility.
                         (setq now 1100))
                       (funcall original proc))))
            (let ((c (car (ws-frame-test--zero-client server))))
              (should reentered)
              (should responder)
              (should (eq 'pending (ws-responder-state responder)))
              (should (process-live-p (cdr b)))
              (should (process-live-p (car b)))
              (should (process-live-p c))
              (should (equal 1 (ws-frame-test--registry-count server)))
              (should (ws-respond responder 200 nil "done"))
              (ws-frame-test--status (car b) 200))))))))

(defun ws-frame-test--event (proc event)
  "Normalize the native event, retaining whether the socket is live then."
  (list (cond ((string-prefix-p "open from " event) 'open)
              ((string-prefix-p "deleted" event) 'deleted)
              (t event))
        (and (process-live-p proc) t)))
(ert-deftest ws/frame-admitted-open-event-keeps-reservation ()
  (let* ((events nil) (ws-request-incomplete-limit 1)
         (ws-frame-test--network-args
          (list :sentinel (lambda (proc event)
                            (push (cons proc (ws-frame-test--event proc event)) events)))))
    (ws-frame-test--with state server
      (let* ((a (ws-frame-test--zero-client server))
             (seen (mapcar #'cdr (cl-remove-if-not (lambda (e) (eq (car e) (cdr a))) events))))
        (should (equal '((open t)) seen))
        (ws-frame-test--rejected-client server)
        (should (process-live-p (car a)))
        (should (equal 1 (ws-frame-test--registry-count server)))
        (ws-frame-test--no-call state)))))

(ert-deftest ws/frame-refused-and-reclaimed-caller-sentinel-sequences ()
  (let* ((events nil) (now 1000) (ws-request-framing-timeout 30) (ws-request-incomplete-limit 1)
         (ws-frame-test--network-args
          (list :sentinel (lambda (proc event)
                            (push (cons proc (ws-frame-test--event proc event)) events)))))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let* ((a (ws-frame-test--zero-client server))
               (before (copy-sequence events)))
          (ws-frame-test--rejected-client server)
          (let* ((delta (nreverse (cl-subseq events 0 (- (length events) (length before)))))
                 (refused (caar delta)))
            (should refused)
            (should (cl-every (lambda (entry) (eq refused (car entry))) delta))
            ;; Native order: delete inside :log, then open from on dead proc.
            (should (equal '((deleted nil) (open nil)) (mapcar #'cdr delta))))
          (setq now 1031)
          (let ((c (car (ws-frame-test--zero-client server))))
            (ws-frame-test--closed (car a))
            (should (process-live-p c))
            (let ((seen (nreverse (mapcar #'cdr
                                          (cl-remove-if-not (lambda (e) (eq (car e) (cdr a))) events)))))
              (should (equal '((open t) (deleted nil)) seen)))
            (should (equal 1 (ws-frame-test--registry-count server)))
            (ws-frame-test--no-call state)))))))

(ert-deftest ws/frame-broken-log-does-not-refuse-admitted-peer ()
  "A dead optional log buffer cannot revoke admission or signal to the waiter."
  (let ((ws-frame-test--log-buffer (generate-new-buffer-name " *ws-broken-log*")))
    (ws-frame-test--with state server
      (kill-buffer ws-frame-test--log-buffer)
      (let ((escaped nil))
        (setq escaped
              (condition-case condition
                  (progn (ws-frame-test--successful-client state server) nil)
                (ert-test-failed (signal (car condition) (cdr condition)))
                (error condition)))
        (should-not escaped)
        (should (= 1 (ws-frame-test--get state :calls)))
        (should-not (get-buffer ws-frame-test--log-buffer))
        (should (equal 0 (ws-frame-test--registry-count server)))
        (should-not (ws-requests server))))))
(ert-deftest ws/frame-admission-reclaims-all-expired-snapshot-entries ()
  (let ((now 1000) (ws-request-framing-timeout 30) (ws-request-incomplete-limit 2))
    (cl-letf (((symbol-function 'ws--frame-now) (lambda () now)))
      (ws-frame-test--with state server
        (let ((a (car (ws-frame-test--zero-client server)))
              (b (car (ws-frame-test--zero-client server))))
          (setq now 1031)
          (let ((c (car (ws-frame-test--zero-client server))))
            (ws-frame-test--closed a)
            (ws-frame-test--closed b)
            (should (equal "" (process-get a :output)))
            (should (equal "" (process-get b :output)))
            (should (process-live-p c))
            (should (equal 1 (ws-frame-test--registry-count server)))
            (ws-frame-test--no-call state)))))))

(provide 'web-server-frame-test)
