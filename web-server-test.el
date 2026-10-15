;;; web-server-test.el --- Test the Emacs Web Server  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2021  Free Software Foundation, Inc.

;; Author: Eric Schulte <schulte.eric@gmail.com>

;; This software is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:
(require 'web-server)
(require 'cl-lib)
(require 'ert)

(defvar ws-test-port 8999)

(defun ws-test-curl-to-string (url &optional get-params post-params curl-flags)
  "Curl URL with optional parameters."
  (async-shell-command
   (format "curl -s -m 4 %s %s %s localhost:%s/%s"
           (or curl-flags "")
           (if get-params
               (mapconcat (lambda (p) (format "-d \"%s=%s\"" (car p) (cdr p)))
                          get-params " ")
             "")
           (if post-params
               (mapconcat (lambda (p) (format "-F \"%s=%s\"" (car p) (cdr p)))
                          post-params " ")
             "")
           ws-test-port url))
  (unwind-protect
      (with-current-buffer "*Async Shell Command*"
        (while (get-buffer-process (current-buffer)) (sit-for 0.1))
        (goto-char (point-min))
        (buffer-string))
    (kill-buffer "*Async Shell Command*")))

(defmacro ws-test-with (handler &rest body)
  (declare (indent 1))
  (let ((srv (cl-gensym)))
    `(let* ((,srv (ws-start ,handler ws-test-port)))
       (unwind-protect (progn ,@body) (ws-stop ,srv)))))
(def-edebug-spec ws-test-with (form body))

(ert-deftest ws/keyword-style-handler ()
  "Ensure that a simple keyword-style handler matches correctly."
  (ws-test-with (mapcar (lambda (letter)
                           `((:GET . ,letter) .
                             (lambda (request)
                               (ws-response-header (ws-process request) 200
                                 '("Content-type" . "text/plain"))
                               (process-send-string (ws-process request)
                                 (concat "returned:" ,letter)))))
                         '("a" "b"))
    (should (string= "returned:a" (ws-test-curl-to-string "a")))
    (should (string= "returned:b" (ws-test-curl-to-string "b")))))

(ert-deftest ws/function-style-handler ()
  "Test that a simple hello-world server responds."
  (ws-test-with
      '(((lambda (_) t) .
         (lambda (request)
           (ws-response-header (ws-process request) 200
             '("Content-type" . "text/plain"))
           (process-send-string (ws-process request) "hello world"))))
    (should (string= (ws-test-curl-to-string "") "hello world"))))

(ert-deftest ws/removed-from-ws-servers-after-stop ()
  (let ((start-length (length ws-servers)))
    (let ((server (ws-start nil ws-test-port)))
      (should (= (length ws-servers) (+ 1 start-length)))
      (ws-stop server)
      (should (= (length ws-servers) start-length)))))

(ert-deftest ws/parse-many-headers ()
  "Test that a number of headers parse successfully."
  (let ((server (ws-start nil ws-test-port))
        (request (make-instance 'ws-request)))
    (unwind-protect
        (progn
          (setf (ws-pending request)
                "GET / HTTP/1.1
Host: localhost:7777
User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:26.0) Gecko/20100101 Firefox/26.0
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
DNT: 1
Cookie: __utma=111872281.1462392269.1345929539.1345929539.1345929539.1
Connection: keep-alive

")
          (ws-parse-request request)
          (let ((headers (cdr (ws-headers request))))
            (should (string= (cdr (assoc :ACCEPT-ENCODING headers))
                             "gzip, deflate"))
            (should (string= (cdr (assoc :GET headers)) "/"))
            (should (string= (cdr (assoc :CONNECTION headers)) "keep-alive"))))
      (ws-stop server))))

(ert-deftest ws/parse-post-data ()
  (let ((server (ws-start nil ws-test-port))
        (request (make-instance 'ws-request)))
    (unwind-protect
        (progn
          (setf (ws-pending request)
                "POST / HTTP/1.1
User-Agent: curl/7.33.0
Host: localhost:8080
Accept: */*
Content-Length: 273
Expect: 100-continue
Content-Type: multipart/form-data; boundary=----------------f1270d0deb77af03

------------------f1270d0deb77af03
Content-Disposition: form-data; name=\"date\"

Wed Dec 18 00:55:39 MST 2013

------------------f1270d0deb77af03
Content-Disposition: form-data; name=\"name\"

\"schulte\"
------------------f1270d0deb77af03--
")
          (ws-parse-request request)
          (let ((headers (cdr (ws-headers request))))
            (should (string= (cdr (assoc 'content (cdr (assoc "name" headers))))
                             "\"schulte\""))
            (should (string= (cdr (assoc 'content (cdr (assoc "date" headers))))
                             "Wed Dec 18 00:55:39 MST 2013\n"))))
      (ws-stop server))))

(ert-deftest ws/parse-another-post-data ()
  "This one from an AJAX request."
  (let ((server (ws-start nil ws-test-port))
        (request (make-instance 'ws-request)))
    (unwind-protect
        (progn
          (setf (ws-pending request)
                "POST /complex.org HTTP/1.1
Host: localhost:4444
User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:26.0) Gecko/20100101 Firefox/26.0
Accept: */*
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
DNT: 1
Content-Type: application/x-www-form-urlencoded; charset=UTF-8
X-Requested-With: XMLHttpRequest
Referer: http://localhost:4444/complex.org
Content-Length: 78
Cookie: __utma=111872281.1462392269.1345929539.1345929539.1345929539.1
Connection: keep-alive
Pragma: no-cache
Cache-Control: no-cache

org=-+one%0A-+two%0A-+three%0A-+four%0A%0A&beg=646&end=667&path=%2Fcomplex.org")
          (ws-parse-request request)
          (let ((headers (cdr (ws-headers request))))
            (should (string= (cdr (assoc "path" headers)) "/complex.org"))
            (should (string= (cdr (assoc "beg" headers)) "646"))
            (should (string= (cdr (assoc "end" headers)) "667"))
            (should (string= (cdr (assoc "org" headers))
                             "- one
- two
- three
- four

"))
            (should (string= (cdr (assoc :CONTENT-TYPE headers))
                             "application/x-www-form-urlencoded; charset=UTF-8"))
            (should (string= (oref request body)
                             "org=-+one%0A-+two%0A-+three%0A-+four%0A%0A&beg=646&end=667&path=%2Fcomplex.org"))))
      (ws-stop server))))

(ert-deftest ws/parse-json-data ()
  "Ensure we can send arbitrary data through to the handler

The handler can then parse it itself."
  (let ((server (ws-start nil ws-test-port))
        (request (make-instance 'ws-request)))
    (unwind-protect
        (progn
          (setf (ws-pending request)
                "POST /complex.org HTTP/1.1
Host: localhost:4444
User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:26.0) Gecko/20100101 Firefox/26.0
Accept: */*
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
DNT: 1
Content-Type: application/json
Referer: http://localhost:4444/complex.org
Content-Length: 33
Cookie: __utma=111872281.1462392269.1345929539.1345929539.1345929539.1
Connection: keep-alive
Pragma: no-cache
Cache-Control: no-cache

{\"some example\": \"json data\"}")
          (ws-parse-request request)
          (let ((headers (cdr (ws-headers request))))
            (should (string= (cdr (assoc :CONTENT-TYPE headers))
                             "application/json"))
            (should (string= (oref request body)
                             "{\"some example\": \"json data\"}")))
      (ws-stop server)))))

(ert-deftest ws/simple-post ()
  "Test a simple POST server."
  (ws-test-with
      '(((:POST . ".*") .
         (lambda (request)
           (with-slots (process headers) request
             (let ((message (cdr (assoc "message" headers))))
               (ws-response-header process 200
                 '("Content-type" . "text/plain"))
               (process-send-string process
                 (format "you said %S\n" (cdr (assoc 'content message)))))))))
    (should (string= (ws-test-curl-to-string "" nil '(("message" . "foo")))
                     "you said \"foo\"\n"))))

(ert-deftest ws/in-directory-p ()
  (mapc (lambda (pair)
          (let ((should-or-not (car pair))
                (dir (cdr pair)))
            (message "dir: %S" dir)
            (should
             (funcall (if should-or-not #'identity #'not)
                      (ws-in-directory-p temporary-file-directory dir)))))
        `((nil . "foo/bar/../../../")
          (t   . ,(concat
                   "foo/bar/../../../"
                   (file-name-nondirectory
                    (directory-file-name temporary-file-directory))
                   "/baz"))
          (t   . "./")
          (nil . "/~/pics")
          (nil . "~/pics")
          (nil . "/pics")
          (nil . "../pics")
          (t   . "pics")
          (nil . ".."))))

(ert-deftest ws/parse-basic-authorization ()
  "Test that a number of headers parse successfully."
  (let* ((server (ws-start nil ws-test-port))
         (request (make-instance 'ws-request))
         (username "foo") (password "bar"))
    (unwind-protect
        (progn
          (setf (ws-pending request)
                (format "GET / HTTP/1.1
Authorization: Basic %s
Connection: keep-alive

" (base64-encode-string (concat username ":" password))))
          (ws-parse-request request)
          (with-slots (headers) request
            (cl-tree-equal (cdr (assoc :AUTHORIZATION headers))
                           (cons :BASIC (cons username password)))))
      (ws-stop server))))

(ert-deftest ws/parse-large-file-upload ()
  "Test that `ws-parse-request' can handle at large file upload.
At least when it comes in a single chunk."
  (let* ((long-string (mapconcat #'int-to-string (number-sequence 0 20000) " "))
         (server (ws-start nil ws-test-port))
         (request (make-instance 'ws-request)))
    (unwind-protect
        (progn
          (setf (ws-pending request)
                (format "POST / HTTP/1.1
User-Agent: curl/7.34.0
Host: localhost:9008
Accept: */*
Content-Length: 9086
Expect: 100-continue
Content-Type: multipart/form-data; boundary=----------------e458fb665704290b

------------------e458fb665704290b
Content-Disposition: form-data; name=\"file\"; filename=\"-\"
Content-Type: application/octet-stream

%s
------------------e458fb665704290b--

" long-string))
          (ws-parse-request request)
          (should
           (string= long-string
                    (cdr (assoc 'content
                                (cdr (assoc "file" (ws-headers request))))))))
      (ws-stop server))))

(ert-deftest ws/web-socket-handshake-rfc-example ()
  "Ensure that `ws-web-socket-handshake' conforms to the example in RFC6455."
  (should (string= (ws-web-socket-handshake "dGhlIHNhbXBsZSBub25jZQ==")
                   "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")))

(ert-deftest ws/web-socket-frame ()
  "Test WebSocket frame encoding for the different varint payload lengths:
   0-125, 126-64k, 64k-2^64."
  (should (string= (ws-web-socket-frame "short") "\201short"))
  (should (string= (substring (ws-web-socket-frame (make-string 126 ?a))
                              0 5) "\201~\0~a"))
  (should (string= (substring (ws-web-socket-frame (make-string 65536 ?a))
                              0 11) "\201\0\0\0\0\0\0\0a")))

(ert-deftest ws/simple-chunked ()
  "Test a simple server using chunked transfer encoding."
  (ws-test-with
      (lambda (request)
        (with-slots (process) request
          (ws-response-header process 200
            '("Content-type" . "text/plain")
            '("Transfer-Encoding" . "chunked"))
          (ws-send process "I am chunked")))
    (should (string= (ws-test-curl-to-string "") "I am chunked"))))

(ert-deftest ws/simple-gzip ()
  "Test a simple server using gzip content/transfer encoding."
  (cl-macrolet ((gzipper (header)
                         `(ws-test-with
                              (lambda (request)
                                (with-slots (process) request
                                  (ws-response-header process 200
                                    '("Content-type" . "text/plain")
                                    '(,header . "gzip"))
                                  (ws-send process "I am zipped")))
                            (should (string= (ws-test-curl-to-string
                                              "" nil nil "--compressed")
                                             "I am zipped")))))
    (gzipper "Content-Encoding")
    (gzipper "Transfer-Encoding")))

;;; Deferred buffered responses

;; Contract: a handler may call `ws-defer' and return.  The connection
;; then stays open with one pending `ws-responder' until exactly one
;; terminal event wins: `ws-respond' (completed, or failed when the reply
;; cannot be written), `ws-responder-abort' (aborted), client disconnect
;; (disconnected), input beyond the declared bound (overflow), a handler
;; error after deferral (failed) or `ws-stop' (stopped).  The winner
;; releases the request and the socket once and calls ON-CLOSE once; every
;; later attempt returns nil.  A deferred reply is buffered and the
;; connection closes after it: no keep-alive and no second handler on the
;; same connection.
;;
;; Input boundary: the request is what the filter had received when the
;; handler ran, as `ws-parse-request' already defines it (without
;; Content-Length framing, bytes coalesced into the completing chunk belong
;; to that request).  Every byte received after deferral is counted by
;; `ws-responder-input-bytes', checked against the bound and discarded: it
;; is not appended to the request's pending input and the responder keeps
;; no copy.

(defvar ws-test-deferred--clients nil
  "Client processes opened by the current deferred test, for cleanup.")

(defun ws-test-deferred--port (server)
  "Return the port SERVER actually listens on."
  (process-contact (ws-process server) :service))

(defun ws-test-deferred--wait (pred &optional seconds)
  "Process events until PRED is non-nil or SECONDS (default 5) elapse.
Return PRED's final value; never depends on a fixed sleep."
  (let ((deadline (+ (float-time) (or seconds 5)))
        (value nil))
    (while (and (not (setq value (funcall pred)))
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    value))

(defun ws-test-deferred--client (port &optional text)
  "Open a raw client to PORT, send TEXT, and return the client process.
The process plist :output accumulates every received byte.  The client
is registered for cleanup by `ws-test-deferred--with'."
  (let ((proc (open-network-stream "ws-test-deferred-client" nil
                                   "127.0.0.1" port)))
    (push proc ws-test-deferred--clients)
    (set-process-coding-system proc 'binary 'binary)
    (set-process-query-on-exit-flag proc nil)
    (process-put proc :output "")
    (set-process-filter proc (lambda (p s)
                               (process-put p :output
                                            (concat (process-get p :output) s))))
    (set-process-sentinel proc #'ignore)
    (when text (process-send-string proc text))
    proc))

(defun ws-test-deferred--get (path)
  "Return a minimal HTTP/1.1 GET request for PATH."
  (format "GET %s HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n" path))

(defmacro ws-test-deferred--with (handler var &rest body)
  "Run BODY with server VAR serving HANDLER on an ephemeral port.
Every client opened through `ws-test-deferred--client' and the server
are cleaned up even when an assertion fails."
  (declare (indent 2))
  `(let ((ws-test-deferred--clients nil)
         (,var (ws-start ,handler 0)))
     (unwind-protect (progn ,@body)
       (mapc (lambda (p) (when (process-live-p p) (delete-process p)))
             ws-test-deferred--clients)
       (when (memq ,var ws-servers) (ws-stop ,var)))))

(defun ws-test-deferred--note (state key value)
  "Set KEY to VALUE in the plist held in the car of STATE."
  (setcar state (plist-put (car state) key value)))

(defun ws-test-deferred--on-close (state)
  "Return an ON-CLOSE callback that records reasons into STATE."
  (lambda (_responder reason)
    (ws-test-deferred--note state :closed
                            (cons reason (plist-get (car state) :closed)))))

(defun ws-test-deferred--handlers (state)
  "Return handlers recording into the plist cell STATE.
/defer defers and stores the request, the responder and the pending
length at deferral; /sync replies synchronously; /fail defers and then
signals."
  `(((:GET . "^/defer") .
     ,(lambda (request)
        (ws-test-deferred--note state :calls
                                (1+ (or (plist-get (car state) :calls) 0)))
        (ws-test-deferred--note state :request request)
        (ws-test-deferred--note state :pending-at-defer
                                (length (ws-pending request)))
        (ws-test-deferred--note
         state :responder
         (ws-defer request :input-limit 64
                   :on-close (ws-test-deferred--on-close state)))
        nil))
    ((:GET . "^/sync") .
     ,(lambda (request)
        (ws-response-header (ws-process request) 200
                            '("Content-type" . "text/plain"))
        (process-send-string (ws-process request) "sync")))
    ((:GET . "^/fail") .
     ,(lambda (request)
        (ws-test-deferred--note
         state :responder
         (ws-defer request :on-close (ws-test-deferred--on-close state)))
        (error "Handler failed after deferral")))))

(defun ws-test-deferred--reply-count (text)
  "Return how many HTTP status lines TEXT contains."
  (with-temp-buffer
    (insert text)
    (count-matches "HTTP/1.1 [0-9][0-9][0-9]" (point-min) (point-max))))

(ert-deftest ws/deferred-returns-and-completes-once ()
  "A deferred handler returns to the event loop; completion wins once."
  (let ((state (list nil)))
    (ws-test-deferred--with (ws-test-deferred--handlers state) server
      (let* ((port (ws-test-deferred--port server))
             (a (ws-test-deferred--client port (ws-test-deferred--get "/defer"))))
        (should (ws-test-deferred--wait (lambda () (plist-get (car state) :responder))))
        (let ((responder (plist-get (car state) :responder)))
          (should (eq 'pending (ws-responder-state responder)))
          ;; Another connection is served while A is still pending.
          (let ((b (ws-test-deferred--client port (ws-test-deferred--get "/sync"))))
            (should (ws-test-deferred--wait (lambda () (not (process-live-p b)))))
            (should (string-match-p "\r\n\r\nsync\\'" (process-get b :output))))
          (should (equal "" (process-get a :output)))
          (should (process-live-p a))
          ;; Completion: one buffered reply, then close and release.
          (should (eq t (ws-respond responder 200 '(("Content-type" . "text/plain"))
                                    "done")))
          (should (ws-test-deferred--wait (lambda () (not (process-live-p a)))))
          (should (string-match-p "\\`HTTP/1.1 200 OK\r\n" (process-get a :output)))
          (should (string-match-p "\r\n\r\ndone\\'" (process-get a :output)))
          (should (eq 'completed (ws-responder-state responder)))
          (should (null (ws-requests server)))
          (should (equal '(completed) (plist-get (car state) :closed)))
          ;; Every later terminal attempt loses and changes nothing.
          (should (null (ws-respond responder 200 nil "again")))
          (should (null (ws-responder-abort responder)))
          (should (equal '(completed) (plist-get (car state) :closed)))
          (should (= 1 (plist-get (car state) :calls))))))))

(ert-deftest ws/deferred-later-input-within-bound-is-counted-not-retained ()
  "Input after deferral is witnessed by the filter, counted, not retained,
and never dispatches a second handler."
  (let ((state (list nil))
        (extra "GET /defer HTTP/1.1\r\n\r\n")) ; 24 bytes, under the 64 bound
    (ws-test-deferred--with (ws-test-deferred--handlers state) server
      (let ((a (ws-test-deferred--client (ws-test-deferred--port server)
                                         (ws-test-deferred--get "/defer"))))
        (should (ws-test-deferred--wait (lambda () (plist-get (car state) :responder))))
        (let ((responder (plist-get (car state) :responder))
              (request (plist-get (car state) :request)))
          (should (= 0 (ws-responder-input-bytes responder)))
          (process-send-string a extra)
          ;; Latch on the filter's consumption, not on elapsed time.
          (should (ws-test-deferred--wait
                   (lambda () (= (length extra)
                                 (ws-responder-input-bytes responder)))))
          (should (eq 'pending (ws-responder-state responder)))
          (should (process-live-p a))
          (should (= (plist-get (car state) :pending-at-defer)
                     (length (ws-pending request))))
          (should (= 1 (plist-get (car state) :calls)))
          (should (eq t (ws-respond responder 200 nil "one")))
          (should (ws-test-deferred--wait (lambda () (not (process-live-p a)))))
          (should (= 1 (ws-test-deferred--reply-count (process-get a :output))))
          (should (= 1 (plist-get (car state) :calls))))))))

(defun ws-test-deferred--accepted-connection (server)
  "Return SERVER's accepted connection process, once Emacs has accepted it."
  (ws-test-deferred--wait
   (lambda ()
     (cl-find-if (lambda (p)
                   (and (eq server (plist-get (process-plist p) :server))
                        (not (eq p (ws-process server)))
                        (process-live-p p)))
                 (process-list)))))

(ert-deftest ws/deferred-coalesced-input-belongs-to-the-request ()
  "Boundary control: bytes coalesced into the completing chunk belong to the
request.  One chunk is fed deterministically through the real `ws-filter'
on the owned accepted connection, so no packet split can change what the
filter sees: one handler, no post-deferral input counted, one reply."
  (let ((state (list nil)))
    (ws-test-deferred--with (ws-test-deferred--handlers state) server
      (let* ((a (ws-test-deferred--client (ws-test-deferred--port server)))
             (conn (ws-test-deferred--accepted-connection server)))
        (should conn)
        (ws-filter conn (concat (ws-test-deferred--get "/defer")
                                "GET /defer HTTP/1.1\r\n\r\n"))
        (let ((responder (plist-get (car state) :responder))
              (request (plist-get (car state) :request)))
          (should responder)
          (should (= 0 (ws-responder-input-bytes responder)))
          (should (= (plist-get (car state) :pending-at-defer)
                     (length (ws-pending request))))
          (should (= 1 (plist-get (car state) :calls)))
          (should (eq t (ws-respond responder 200 nil "one")))
          (should (ws-test-deferred--wait (lambda () (not (process-live-p a)))))
          (should (= 1 (ws-test-deferred--reply-count (process-get a :output))))
          (should (= 1 (plist-get (car state) :calls))))))))

(ert-deftest ws/deferred-extra-input-beyond-bound-closes-once ()
  "Input beyond the declared bound closes the connection as overflow."
  (let ((state (list nil)))
    (ws-test-deferred--with (ws-test-deferred--handlers state) server
      (let ((a (ws-test-deferred--client (ws-test-deferred--port server)
                                         (ws-test-deferred--get "/defer"))))
        (should (ws-test-deferred--wait (lambda () (plist-get (car state) :responder))))
        (process-send-string a (make-string 200 ?x))
        (should (ws-test-deferred--wait (lambda () (not (process-live-p a)))))
        (let ((responder (plist-get (car state) :responder))
              (request (plist-get (car state) :request)))
          (should (eq 'overflow (ws-responder-state responder)))
          (should (> (ws-responder-input-bytes responder) 64))
          (should (= (plist-get (car state) :pending-at-defer)
                     (length (ws-pending request))))
          (should (equal '(overflow) (plist-get (car state) :closed)))
          (should (null (ws-requests server)))
          (should (null (ws-respond responder 200 nil "late")))
          (should (equal "" (process-get a :output)))
          (should (= 1 (plist-get (car state) :calls))))))))

(ert-deftest ws/deferred-disconnect-reconciles-once ()
  "A client disconnect releases the request once; late completion loses."
  (let ((state (list nil)))
    (ws-test-deferred--with (ws-test-deferred--handlers state) server
      (let ((a (ws-test-deferred--client (ws-test-deferred--port server)
                                         (ws-test-deferred--get "/defer"))))
        (should (ws-test-deferred--wait (lambda () (plist-get (car state) :responder))))
        (delete-process a)
        (let ((responder (plist-get (car state) :responder)))
          (should (ws-test-deferred--wait
                   (lambda () (eq 'disconnected (ws-responder-state responder)))))
          (should (null (ws-requests server)))
          (should (equal '(disconnected) (plist-get (car state) :closed)))
          (should (null (ws-respond responder 200 nil "late")))
          (should (null (ws-responder-abort responder)))
          (should (equal '(disconnected) (plist-get (car state) :closed))))))))

(ert-deftest ws/deferred-failed-write-ends-once ()
  "A reply that cannot be written ends the responder once as failed.
`ws-respond' returns nil without signalling, ON-CLOSE runs once, the
request and socket are released and later attempts lose."
  (let ((state (list nil)))
    (ws-test-deferred--with (ws-test-deferred--handlers state) server
      (let ((a (ws-test-deferred--client (ws-test-deferred--port server)
                                         (ws-test-deferred--get "/defer"))))
        (should (ws-test-deferred--wait (lambda () (plist-get (car state) :responder))))
        (let ((responder (plist-get (car state) :responder))
              (writes 0))
          ;; Owned write failure, before any sentinel can observe it.
          (cl-letf (((symbol-function 'process-send-string)
                     (lambda (&rest _)
                       (cl-incf writes)
                       (signal 'file-error '("Writing to process" "Broken pipe")))))
            (should (null (ws-respond responder 200 nil "lost"))))
          (should (> writes 0))
          (should (eq 'failed (ws-responder-state responder)))
          (should (equal '(failed) (plist-get (car state) :closed)))
          (should (null (ws-requests server)))
          (should (ws-test-deferred--wait (lambda () (not (process-live-p a)))))
          (should (null (ws-respond responder 200 nil "late")))
          (should (null (ws-responder-abort responder)))
          (should (equal '(failed) (plist-get (car state) :closed))))))))

(ert-deftest ws/deferred-stop-reconciles-once ()
  "`ws-stop' with a pending responder ends it once as stopped."
  (let ((state (list nil)))
    (ws-test-deferred--with (ws-test-deferred--handlers state) server
      (let ((a (ws-test-deferred--client (ws-test-deferred--port server)
                                         (ws-test-deferred--get "/defer"))))
        (should (ws-test-deferred--wait (lambda () (plist-get (car state) :responder))))
        (ws-stop server)
        (let ((responder (plist-get (car state) :responder)))
          (should (ws-test-deferred--wait (lambda () (not (process-live-p a)))))
          (should (eq 'stopped (ws-responder-state responder)))
          (should (equal '(stopped) (plist-get (car state) :closed)))
          (should (null (ws-respond responder 200 nil "late")))
          (should (equal '(stopped) (plist-get (car state) :closed))))))))

(ert-deftest ws/deferred-handler-failure-ends-once ()
  "A handler error after deferral answers 500 and ends the responder once."
  (let ((state (list nil)))
    (ws-test-deferred--with (ws-test-deferred--handlers state) server
      (let ((a (ws-test-deferred--client (ws-test-deferred--port server)
                                         (ws-test-deferred--get "/fail"))))
        (should (ws-test-deferred--wait (lambda () (not (process-live-p a)))))
        (should (string-match-p "\\`HTTP/1.1 500" (process-get a :output)))
        (let ((responder (plist-get (car state) :responder)))
          (should (eq 'failed (ws-responder-state responder)))
          (should (equal '(failed) (plist-get (car state) :closed)))
          (should (null (ws-requests server)))
          (should (null (ws-respond responder 200 nil "late"))))))))

(ert-deftest ws/deferred-abort-closes-without-reply ()
  "`ws-responder-abort' closes with no reply bytes and wins once."
  (let ((state (list nil)))
    (ws-test-deferred--with (ws-test-deferred--handlers state) server
      (let ((a (ws-test-deferred--client (ws-test-deferred--port server)
                                         (ws-test-deferred--get "/defer"))))
        (should (ws-test-deferred--wait (lambda () (plist-get (car state) :responder))))
        (let ((responder (plist-get (car state) :responder)))
          (should (eq t (ws-responder-abort responder)))
          (should (ws-test-deferred--wait (lambda () (not (process-live-p a)))))
          (should (equal "" (process-get a :output)))
          (should (eq 'aborted (ws-responder-state responder)))
          (should (equal '(aborted) (plist-get (car state) :closed)))
          (should (null (ws-requests server)))
          (should (null (ws-respond responder 200 nil "late"))))))))

(ert-deftest ws/deferred-on-close-error-is-contained ()
  "An error in ON-CLOSE is raised once and never escapes completion,
a filter or a sentinel."
  (let ((responder nil)
        (on-close-calls 0))
    (ws-test-deferred--with
        (lambda (request)
          (setq responder (ws-defer request :on-close
                                    (lambda (&rest _)
                                      (cl-incf on-close-calls)
                                      (error "on-close exploded"))))
          nil)
        server
      (let ((a (ws-test-deferred--client (ws-test-deferred--port server)
                                         (ws-test-deferred--get "/x"))))
        (should (ws-test-deferred--wait (lambda () responder)))
        (should (eq t (ws-respond responder 200 nil "ok")))
        (should (= 1 on-close-calls))
        (should (ws-test-deferred--wait (lambda () (not (process-live-p a)))))
        (should (eq 'completed (ws-responder-state responder)))
        (should (null (ws-requests server)))
        (should (= 1 on-close-calls))))))

(ert-deftest ws/synchronous-handler-unchanged-raw ()
  "A synchronous handler still replies and closes with no responder."
  (ws-test-deferred--with
      (lambda (request)
        (ws-response-header (ws-process request) 200
                            '("Content-type" . "text/plain"))
        (process-send-string (ws-process request) "plain"))
      server
    (let ((a (ws-test-deferred--client (ws-test-deferred--port server)
                                       (ws-test-deferred--get "/"))))
      (should (ws-test-deferred--wait (lambda () (not (process-live-p a)))))
      (should (string-match-p "\r\n\r\nplain\\'" (process-get a :output)))
      (should (null (ws-requests server))))))

(defun ws-test-deferred--during-write (state action)
  "Defer /defer, then run ACTION on the responder from the owned client's
filter while an 8 MiB reply is being written.  Return a plist of what the
outer `ws-respond', the reentrant ACTION and the client observed.
The client's ordinary process filter is the reentry point, so no server
primitive is replaced: `process-send-string' lets other process output run
between write bunches."
  (let ((writing nil)
        (reentered nil)
        (inner :unobserved)
        (outer :unobserved))
    (ws-test-deferred--with (ws-test-deferred--handlers state) server
      (let ((client (ws-test-deferred--client
                     (ws-test-deferred--port server)
                     (ws-test-deferred--get "/defer"))))
        (should (ws-test-deferred--wait
                 (lambda () (plist-get (car state) :responder))))
        (let* ((responder (plist-get (car state) :responder))
               (original-filter (process-filter client)))
          (set-process-filter
           client
           (lambda (proc text)
             (funcall original-filter proc text)
             (when (and writing (not reentered))
               (setq reentered t
                     inner (funcall action responder proc)))))
          (setq writing t)
          (setq outer (ws-respond responder 200 nil
                                  (make-string (* 8 1024 1024) ?x)))
          (setq writing nil)
          (should reentered)
          (should (ws-test-deferred--wait
                   (lambda () (not (process-live-p client)))))
          (list :inner inner :outer outer
                :replies (ws-test-deferred--reply-count (process-get client :output))
                :state (ws-responder-state responder)
                :closed (plist-get (car state) :closed)
                :requests (ws-requests server)))))))

(ert-deftest ws/deferred-reentrant-completion-during-write-emits-one-response ()
  "A completion attempted while the reply is being written writes nothing:
one reply, the outer call wins, ON-CLOSE fires once."
  (let* ((state (list nil))
         (r (ws-test-deferred--during-write
             state (lambda (responder _client)
                     (ws-respond responder 200 nil "inner")))))
    (should (null (plist-get r :inner)))
    (should (eq t (plist-get r :outer)))
    (should (= 1 (plist-get r :replies)))
    (should (eq 'completed (plist-get r :state)))
    (should (equal '(completed) (plist-get r :closed)))
    (should (null (plist-get r :requests)))))

(ert-deftest ws/deferred-abort-during-write-ends-once ()
  "An abort while the reply is being written wins: the outer completion
returns nil, cleanup and ON-CLOSE happen once, as aborted."
  (let* ((state (list nil))
         (r (ws-test-deferred--during-write
             state (lambda (responder _client)
                     (ws-responder-abort responder)))))
    (should (eq t (plist-get r :inner)))
    (should (null (plist-get r :outer)))
    (should (eq 'aborted (plist-get r :state)))
    (should (equal '(aborted) (plist-get r :closed)))
    (should (null (plist-get r :requests)))))

(ert-deftest ws/deferred-disconnect-during-write-ends-once ()
  "A client disconnect while the reply is being written ends the responder
exactly once.  Whichever terminal event the server observes first wins
(the disconnect, a failed write, or completion when every byte was already
written); the outer call returns t only when completion won."
  (let* ((state (list nil))
         (r (ws-test-deferred--during-write
             state (lambda (_responder client)
                     (delete-process client)
                     :deleted))))
    (should (eq :deleted (plist-get r :inner)))
    (should (= 1 (length (plist-get r :closed))))
    (should (memq (car (plist-get r :closed)) '(disconnected failed completed)))
    (should (eq (eq t (plist-get r :outer))
                (eq 'completed (car (plist-get r :closed)))))
    (should (eq (car (plist-get r :closed)) (plist-get r :state)))
    (should (null (plist-get r :requests)))))

(provide 'web-server-test)
