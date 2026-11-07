;;; web-server-test.el --- Test the Emacs Web Server  -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2021  Free Software Foundation, Inc.
;; Copyright (C) 2026 OC (cosmicz@github)
;; Modified by OC (cosmicz@github), 2026: Test deferred replies and accept logging; correct fixture Content-Lengths; bound curl completion and check gzip transfer coding; test the raw request-target and raw query accessors.

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

(defvar ws-test-curl-deadline 10
  "Seconds `ws-test-curl-to-string' may spend on one curl call.
The bound covers waiting for curl to exit and reading all of its
output.  It is longer than curl's own -m 4, so a curl timeout is
still reported as curl's exit status.")

(defun ws-test-curl-to-string (url &optional get-params post-params curl-flags)
  "Curl URL with optional parameters and return curl's stdout.
Curl runs on pipes, not a terminal, and asynchronously, so the
in-process server keeps answering.  Completion follows curl's real
process status, not its sentinel.  Unless curl exits 0, the test
fails with curl's exit status, stderr and stdout and the command.
If curl or its output is not finished within `ws-test-curl-deadline'
seconds, the test fails with the phase, curl's status and the output
read so far.
An HTTP error status is part of the response, not a curl failure."
  (let* ((command
          (format "curl -sS -m 4 %s %s %s localhost:%s/%s"
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
         (out (generate-new-buffer " *ws-test-curl*"))
         (err (generate-new-buffer " *ws-test-curl-stderr*"))
         ;; Own sentinels, so no process status text lands in either buffer.
         (stderr (make-pipe-process :name "ws-test-curl-stderr" :buffer err
                                    :coding 'binary :noquery t
                                    :sentinel #'ignore))
         (deadline (+ (float-time) ws-test-curl-deadline))
         proc)
    (unwind-protect
        (let ((check
               (lambda (phase)
                 (when (> (float-time) deadline)
                   (ert-fail
                    (list :curl-timeout ws-test-curl-deadline
                          :phase phase
                          :status (process-status proc)
                          :exit (and (memq (process-status proc) '(exit signal))
                                     (process-exit-status proc))
                          :stderr (with-current-buffer err (buffer-string))
                          :stdout (with-current-buffer out (buffer-string))
                          :command command))))))
          (setq proc (make-process
                      :name "ws-test-curl" :buffer out
                      :command (list shell-file-name shell-command-switch command)
                      :connection-type 'pipe :coding 'binary
                      :stderr stderr :noquery t
                      :sentinel #'ignore))
          ;; Wait on process status, not on a sentinel, so a sentinel that
          ;; never runs cannot stall the wait.  A nil process lets the
          ;; in-process server and every other process callback run.
          (while (process-live-p proc)
            (funcall check 'exit)
            (accept-process-output nil 0.05))
          ;; The stderr pipe closes at EOF, so it is complete when not live.
          (while (process-live-p stderr)
            (funcall check 'stderr)
            (accept-process-output nil 0.05))
          ;; Read whatever stdout was still unread when curl exited.
          (while (progn (funcall check 'stdout)
                        (accept-process-output proc 0)))
          (let ((stdout (with-current-buffer out (buffer-string))))
            (unless (and (eq (process-status proc) 'exit)
                         (zerop (process-exit-status proc)))
              (ert-fail
               (list :curl-exit (if (eq (process-status proc) 'exit)
                                    (process-exit-status proc)
                                  (list (process-status proc)
                                        (process-exit-status proc)))
                     :stderr (with-current-buffer err (buffer-string))
                     :stdout stdout
                     :command command)))
            stdout))
      (when proc (delete-process proc))
      (delete-process stderr)
      (kill-buffer out)
      (kill-buffer err))))

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

(defun ws-test-parse-chunks (&rest chunks)
  "Feed CHUNKS to a fresh request as successive arrivals; return it.
Each chunk is appended to the pending input and parsed, as `ws-filter' does."
  (let ((request (make-instance 'ws-request)))
    (dolist (chunk chunks request)
      (setf (ws-pending request) (concat (ws-pending request) chunk))
      (ws-parse-request request))))

(ert-deftest ws/request-target-raw ()
  "The request-target and its query are kept exactly as received."
  (let ((request (ws-test-parse-chunks
                  "GET /p?a=b%3Dc&x=%26&sp=a+b&u=%C3%A9 HTTP/1.1\r\nHost: h\r\n\r\n")))
    (should (equal "/p?a=b%3Dc&x=%26&sp=a+b&u=%C3%A9" (ws-request-target request)))
    (should (equal "a=b%3Dc&x=%26&sp=a+b&u=%C3%A9" (ws-request-raw-query request)))
    (should-not (multibyte-string-p (ws-request-target request)))))

(ert-deftest ws/request-target-raw-bytes ()
  "Raw non-ASCII bytes in the request-target stay unibyte and unchanged."
  (let ((request (ws-test-parse-chunks
                  "GET /caf\303\251?q=\303\251 HTTP/1.1\r\nHost: h\r\n\r\n")))
    (should (equal "/caf\303\251?q=\303\251" (ws-request-target request)))
    (should (equal "q=\303\251" (ws-request-raw-query request)))
    (should-not (multibyte-string-p (ws-request-raw-query request)))))

(ert-deftest ws/request-raw-query-forms ()
  "The raw query distinguishes no query, an empty query and absolute form."
  (let ((unparsed (make-instance 'ws-request)))
    (should-not (ws-request-target unparsed))
    (should-not (ws-request-raw-query unparsed)))
  (dolist (case '(("/p" . nil) ("/p?" . "") ("/p?a?b" . "a?b")
                  ("http://h/p?q=1" . "q=1")))
    (let ((request (ws-test-parse-chunks
                    (concat "GET " (car case) " HTTP/1.1\r\nHost: h\r\n\r\n"))))
      (should (equal (car case) (ws-request-target request)))
      (should (equal (cdr case) (ws-request-raw-query request))))))

(ert-deftest ws/request-target-legacy-headers-unchanged ()
  "Keeping the raw target leaves the parsed header alist as it was.
The path stays the method entry's value and the query is still decoded
into pairs; this is compatibility, not an endorsement of that decoding."
  (let ((request (ws-test-parse-chunks
                  "GET /p?a=b%3Dc&x=%26&sp=a+b&u=%C3%A9 HTTP/1.1\r\nHost: h\r\n\r\n")))
    (should (equal '((:GET . "/p") ("u" . "\303\251") ("sp" . "a+b") ("" . "")
                     ("x" . "") ("a" . "b=c") (:HOST . "h"))
                   (cdr (ws-headers request))))))

(ert-deftest ws/request-target-fragmented-input ()
  "The target is set once framing has the complete request head.
Arrivals are fed as `ws-filter' does: framed by `ws--frame-append', then
parsed.  Until the head is complete no target is recorded."
  (let ((request (make-instance 'ws-request)))
    (cl-flet ((feed (chunk)
                (ws--frame-append request chunk)
                (ws-parse-request request)))
      (should-not (feed "GET /p?a=%2"))
      (should-not (ws-request-target request))
      (should-not (feed "6b HTTP/1.1\r\nHo"))
      (should-not (ws-request-target request))
      (should (feed "st: h\r\n\r\n"))
      (should (equal "/p?a=%26b" (ws-request-target request)))
      (should (equal "a=%26b" (ws-request-raw-query request))))))

(defun ws-test-refused-head (head)
  "Parse HEAD as a legacy direct caller does; return the refused request.
Fail unless framing refuses it.  Without a process no reply is written."
  (let ((request (make-instance 'ws-request)))
    (setf (ws-pending request) head)
    (should-not (catch 'close-connection (ws-parse-request request) 'parsed))
    request))

(ert-deftest ws/request-target-uncommon-method ()
  "A method outside `ws-http-common-methods' is refused with no target."
  (let ((request (ws-test-refused-head
                  "PATCH /p?x=1 HTTP/1.1\r\nHost: h\r\n\r\n")))
    (should (equal 400 (ws-frame-error request)))
    (should-not (ws-request-target request))
    (should-not (ws-request-raw-query request))))

(ert-deftest ws/request-target-refused-head ()
  "A head refused after a valid request line records no target.
A second request-line-shaped line is not a field line, so framing refuses
the whole head; the first line's target must not survive that."
  (let ((request (ws-test-refused-head
                  "GET /p?a=1 HTTP/1.1\r\nGET /other?x=1 HTTP/1.1\r\n\r\n")))
    (should (equal 400 (ws-frame-error request)))
    (should-not (ws-request-target request))
    (should-not (ws-request-raw-query request))))

(ert-deftest ws/request-target-method-case ()
  "A head framing accepts has a target whatever `case-fold-search' is.
Framing matches the method case-insensitively, so the accessor does too."
  (let ((request (ws-test-parse-chunks
                  "get /p?x=1 HTTP/1.1\r\nHost: h\r\n\r\n"))
        (case-fold-search nil))
    (should (equal "/p?x=1" (ws-request-target request)))
    (should (equal "x=1" (ws-request-raw-query request)))))

(ert-deftest ws/request-target-keeps-match-data ()
  "The accessors leave the caller's match data as it was."
  (let ((request (ws-test-parse-chunks
                  "GET /p?x=1 HTTP/1.1\r\nHost: h\r\n\r\n")))
    (should (string-match "b\\(c\\)" "abcd"))
    (should (equal "x=1" (ws-request-raw-query request)))
    (should (equal "/p?x=1" (ws-request-target request)))
    (should (equal '(1 3 2 3) (match-data)))))

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
Content-Length: 246
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
Content-Length: 29
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
Content-Length: 109074
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
  "Test a simple server using gzip content/transfer encoding.
curl decodes a gzip Content-Encoding under --compressed, but a gzip
Transfer-Encoding only under --tr-encoding, which also sends TE: gzip."
  (cl-macrolet ((gzipper (header flag)
                         `(ws-test-with
                              (lambda (request)
                                (with-slots (process) request
                                  (ws-response-header process 200
                                    '("Content-type" . "text/plain")
                                    '(,header . "gzip"))
                                  (ws-send process "I am zipped")))
                            (should (string= (ws-test-curl-to-string
                                              "" nil nil ,flag)
                                             "I am zipped")))))
    (gzipper "Content-Encoding" "--compressed")
    (gzipper "Transfer-Encoding" "--tr-encoding")))

(ert-deftest ws/curl-helper-fails-on-curl-error ()
  "A curl failure fails the test with curl's own diagnostic, not \"\".
The handler writes nothing, so the server closes the connection
without a reply and curl exits nonzero with empty stdout."
  (ws-test-with (lambda (_request) nil)
    (let* ((err (should-error (ws-test-curl-to-string "")
                              :type 'ert-test-failed))
           (data (cadr err))
           (code (plist-get data :curl-exit)))
      (message "ws/curl-helper-fails-on-curl-error: curl exit %S" code)
      (should (integerp code))
      (should-not (zerop code))
      (should (equal "" (plist-get data :stdout)))
      (should (string-match-p (format "curl: (%d)" code)
                              (plist-get data :stderr)))
      (should (string-match-p "curl -sS" (plist-get data :command))))))

(ert-deftest ws/curl-helper-returns-empty-body ()
  "A successful response with an empty body is \"\", not a failure."
  (ws-test-with
      (lambda (request)
        (with-slots (process) request
          (ws-response-header process 200 '("Content-type" . "text/plain"))))
    (should (string= "" (ws-test-curl-to-string "")))))

(ert-deftest ws/curl-helper-returns-4xx-body ()
  "An HTTP error status is a response for the test, not a curl failure."
  (ws-test-with
      (lambda (request)
        (with-slots (process) request
          (ws-response-header process 404 '("Content-type" . "text/plain"))
          (ws-send process "missing")))
    (should (string= "missing" (ws-test-curl-to-string "")))))

(defmacro ws-test-curl--bounded (seconds &rest body)
  "Run BODY, giving up after SECONDS.
Return (:value VALUE) when BODY returns VALUE, or :outer-bound when
SECONDS pass first.  A timer throws out of whatever BODY is waiting
in, so a helper that never completes fails the test instead of
hanging the suite."
  (declare (indent 1))
  (let ((tag (make-symbol "tag"))
        (timer (make-symbol "timer")))
    `(let* ((,tag (make-symbol "ws-test-curl--bounded"))
            (,timer (run-at-time ,seconds nil
                                 (lambda () (throw ,tag :outer-bound)))))
       (unwind-protect
           (catch ,tag (list :value (progn ,@body)))
         (cancel-timer ,timer)))))

(defmacro ws-test-curl--with-stub (script &rest body)
  "Run BODY with `curl' on PATH replaced by the shell SCRIPT.
The helper's own command line then runs SCRIPT, whose output and
exit status the test controls exactly.  SCRIPT finds its directory
in $WS_TEST_CURL_STUB, and appends the pid of each background job it
starts to the file pids there; those jobs are terminated on exit."
  (declare (indent 1))
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (file-name-as-directory
                  (make-temp-file "ws-test-curl-stub" t))))
       (unwind-protect
           (let ((process-environment
                  (append (list (concat "PATH=" ,dir path-separator
                                        (getenv "PATH"))
                                (concat "WS_TEST_CURL_STUB=" ,dir))
                          process-environment)))
             (with-temp-file (expand-file-name "curl" ,dir)
               (insert "#!/bin/sh\n" ,script))
             (set-file-modes (expand-file-name "curl" ,dir) #o755)
             ,@body)
         (let ((pids (expand-file-name "pids" ,dir)))
           (when (file-exists-p pids)
             (dolist (pid (split-string (with-temp-buffer
                                          (insert-file-contents pids)
                                          (buffer-string))))
               (ignore-errors (signal-process (string-to-number pid) 'TERM)))))
         (delete-directory ,dir t)))))

(defun ws-test-curl--when-exited (file)
  "Create FILE once Emacs sees the helper's curl process exit.
Return the polling timer.  A stand-in curl's background job can wait
for FILE, so what it writes is ordered after the exit."
  (let (proc timer)
    (setq timer
          (run-at-time
           0 0.01
           (lambda ()
             ;; Keep the process once seen: an exited process leaves the
             ;; process list, so `get-process' cannot find it again.
             (setq proc (or proc (get-process "ws-test-curl")))
             (when (and proc (memq (process-status proc) '(exit signal)))
               (write-region "" nil file nil 'silent)
               (cancel-timer timer)))))
    timer))

(defun ws-test-curl--leftovers ()
  "Return the helper's live processes and buffers, which should be none."
  (append (cl-remove-if-not
           (lambda (p) (and (process-live-p p)
                            (string-prefix-p "ws-test-curl" (process-name p))))
           (process-list))
          (cl-remove-if-not
           (lambda (b) (string-prefix-p " *ws-test-curl" (buffer-name b)))
           (buffer-list))))

;; Advising a primitive such as `make-process' normally makes a native
;; build compile a trampoline for it, which needs a working native
;; compiler.  The tests bind this to nil so they never depend on one;
;; the helper's calls from Lisp still see the advice.
(defvar native-comp-enable-subr-trampolines)

(defvar ws-test-curl--replaced nil
  "Curl processes whose sentinel `ws-test-curl--without-sentinel' replaced.")

(defun ws-test-curl--without-sentinel (make-process &rest args)
  "Call MAKE-PROCESS with ARGS, ignoring the helper's curl sentinel."
  (if (equal (plist-get args :name) "ws-test-curl")
      (let ((proc (apply make-process
                         (plist-put (copy-sequence args) :sentinel #'ignore))))
        (push proc ws-test-curl--replaced)
        proc)
    (apply make-process args)))

(ert-deftest ws/curl-helper-completes-without-sentinel ()
  "The helper completes from curl's real exit, not from its sentinel.
The sentinel of the helper's curl process is replaced with `ignore',
so a helper that waits for its sentinel never returns; the outer
bound turns that into a failure instead of a hang."
  (ws-test-with
      (lambda (request)
        (with-slots (process) request
          (ws-response-header process 200 '("Content-type" . "text/plain"))
          (ws-send process "ok")))
    (let ((ws-test-curl--replaced nil)
          result)
      (let ((native-comp-enable-subr-trampolines nil))
        (advice-add 'make-process :around #'ws-test-curl--without-sentinel))
      (unwind-protect
          (setq result (ws-test-curl--bounded 3 (ws-test-curl-to-string "")))
        (advice-remove 'make-process #'ws-test-curl--without-sentinel))
      (should (= 1 (length ws-test-curl--replaced)))
      (let ((proc (car ws-test-curl--replaced)))
        (message "ws/curl-helper-completes-without-sentinel: %S %S %S"
                 (process-sentinel proc) (process-status proc)
                 (process-exit-status proc))
        (should (eq #'ignore (process-sentinel proc)))
        (should (eq 'exit (process-status proc)))
        (should (zerop (process-exit-status proc))))
      (should (equal '(:value "ok") result)))))

(ert-deftest ws/curl-helper-deadline-fails-with-cleanup ()
  "Curl or its stderr still open at the deadline fails the test promptly.
In the first case the handler keeps the connection open without
replying, so curl waits for its own -m 4.  In the second, a stand-in
curl exits 0 while a background child holds its stderr open.  The
same shorter deadline fails first with its phase, curl's status and
the output so far, invents no exit status, and leaves no process or
buffer of the helper behind."
  (ws-test-with (lambda (_request) :keep-alive)
    (let* ((ws-test-curl-deadline 0.5)
           (start (float-time))
           (err (should-error (ws-test-curl--bounded 6
                                (ws-test-curl-to-string ""))
                              :type 'ert-test-failed))
           (data (cadr err)))
      (should (< (- (float-time) start) 3))
      (should (equal 0.5 (plist-get data :curl-timeout)))
      (should (eq 'exit (plist-get data :phase)))
      (should (eq 'run (plist-get data :status)))
      (should (plist-member data :exit))
      (should-not (plist-get data :exit))
      (should-not (plist-member data :curl-exit))
      (should (equal "" (plist-get data :stdout)))
      (should (stringp (plist-get data :stderr)))
      (should (string-match-p "curl -sS" (plist-get data :command)))
      (should-not (ws-test-curl--leftovers))))
  (ws-test-curl--with-stub
      (concat "printf partial; printf 'stderr-open\\n' >&2\n"
              "sleep 2 >/dev/null &\n"
              "echo $! >>\"$WS_TEST_CURL_STUB/pids\"\n"
              "exit 0\n")
    (let* ((ws-test-curl-deadline 0.5)
           (start (float-time))
           (err (should-error (ws-test-curl--bounded 6
                                (ws-test-curl-to-string ""))
                              :type 'ert-test-failed))
           (data (cadr err)))
      (should (< (- (float-time) start) 1.5))
      (should (equal 0.5 (plist-get data :curl-timeout)))
      (should (eq 'stderr (plist-get data :phase)))
      (should (eq 'exit (plist-get data :status)))
      (should (eql 0 (plist-get data :exit)))
      (should-not (plist-member data :curl-exit))
      (should (string-prefix-p (plist-get data :stdout) "partial"))
      (should (string-prefix-p (plist-get data :stderr) "stderr-open\n"))
      (should-not (ws-test-curl--leftovers)))))

(ert-deftest ws/curl-helper-returns-complete-body ()
  "A body larger than a pipe buffer is returned complete, byte for byte.
The outer bound keeps a helper that never completes from hanging."
  (let ((body (mapconcat (lambda (i) (format "%07d\n" i))
                         (number-sequence 0 24999) "")))
    (should (= 200000 (length body)))
    (ws-test-with
        (lambda (request)
          (with-slots (process) request
            (ws-response-header process 200 '("Content-type" . "text/plain"))
            (ws-send process body)))
      (should (equal (list :value body)
                     (ws-test-curl--bounded 6 (ws-test-curl-to-string "")))))))

(ert-deftest ws/curl-helper-reports-complete-stderr ()
  "A failing curl is reported with its complete stdout and stderr.
The stand-in curl exits 7.  Its background job writes the end of
stderr only after Emacs has seen that exit, so the report must keep
reading stderr after curl is gone."
  (ws-test-curl--with-stub
      (concat "printf stdout-part; printf 'stderr-head\\n' >&2\n"
              "(i=0\n"
              " while [ ! -e \"$WS_TEST_CURL_STUB/exited\" ] && [ $i -lt 100 ]; do\n"
              "   sleep 0.05; i=$((i+1))\n"
              " done\n"
              " [ -e \"$WS_TEST_CURL_STUB/exited\" ] && printf 'stderr-tail\\n' >&2\n"
              ") >/dev/null &\n"
              "echo $! >>\"$WS_TEST_CURL_STUB/pids\"\n"
              "exit 7\n")
    (let* ((exited (expand-file-name "exited" (getenv "WS_TEST_CURL_STUB")))
           (timer (ws-test-curl--when-exited exited))
           (err (unwind-protect
                    (should-error (ws-test-curl--bounded 6
                                    (ws-test-curl-to-string ""))
                                  :type 'ert-test-failed)
                  (cancel-timer timer)))
           (data (cadr err)))
      (should (eql 7 (plist-get data :curl-exit)))
      (should (equal "stdout-part" (plist-get data :stdout)))
      (should (equal "stderr-head\nstderr-tail\n" (plist-get data :stderr)))
      (should (file-exists-p exited))
      (should-not (ws-test-curl--leftovers)))))

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
;; handler ran, after complete Content-Length framing.  The coalesced
;; fixture explicitly declares its body; undeclared extras are refused.  Every byte received after deferral is counted by
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
  "Boundary control: declared body bytes coalesced into the completing chunk belong to the
request.  One chunk is fed deterministically through the real `ws-filter'
on the owned accepted connection, so no packet split can change what the
filter sees: one handler, no post-deferral input counted, one reply."
  (let ((state (list nil)))
    (ws-test-deferred--with (ws-test-deferred--handlers state) server
      (let* ((a (ws-test-deferred--client (ws-test-deferred--port server)))
             (conn (ws-test-deferred--accepted-connection server)))
        (should conn)
        (let ((payload "GET /defer HTTP/1.1\r\n\r\n"))
          (ws-filter conn
                     (concat "GET /defer HTTP/1.1\r\n"
                             (format "Content-Length: %d\r\n\r\n" (length payload))
                             payload)))
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

;;; Connection log

;; Emacs calls a server's :log function with the listening process, the
;; accepted client and a message.  When accept itself fails, the client
;; is nil.  See server_accept_connection in Emacs' src/process.c.
;; These tests call the callback that `ws-start' installed, so they cover
;; the exact function Emacs calls.

(defmacro ws-test-log--with (var log &rest body)
  "Run BODY with server VAR on an ephemeral port, logging to buffer LOG.
LOG is a buffer name, or nil for a server started without logging.
The server, the log buffer and every client opened through
`ws-test-deferred--client' are cleaned up even when an assertion fails."
  (declare (indent 2))
  `(let ((ws-test-deferred--clients nil)
         (,var (ws-start #'ignore 0 ,log)))
     (unwind-protect (progn ,@body)
       (mapc (lambda (p) (when (process-live-p p) (delete-process p)))
             ws-test-deferred--clients)
       (when (memq ,var ws-servers) (ws-stop ,var))
       (when (and ,log (get-buffer ,log)) (kill-buffer ,log)))))

(defun ws-test-log--text (log)
  "Return the contents of the log buffer named LOG."
  (with-current-buffer log (buffer-string)))

(ert-deftest ws/log-accept-failure-without-client ()
  "An accept failure, reported with a nil client, logs one line.
The line holds the time, nil for the client host and port, and Emacs'
message; the callback signals nothing."
  (let ((log (generate-new-buffer-name " *ws-test-log*")))
    (ws-test-log--with server log
      (let ((callback (process-contact (ws-process server) :log)))
        (should (functionp callback))
        (funcall callback (ws-process server) nil "accept failed with code24\n")
        (should (string-match-p
                 "\\`[^\t\n]+\tnil\tnil\taccept failed with code24\n\\'"
                 (ws-test-log--text log)))))))

(ert-deftest ws/log-accepted-client ()
  "An accepted client is logged once with its host and port.
The client sends a request so the server handles the connection and
closes it; by then every line for it has been logged."
  (let ((log (generate-new-buffer-name " *ws-test-log*")))
    (ws-test-log--with server log
      (let* ((client (ws-test-deferred--client (ws-test-deferred--port server)
                                               (ws-test-deferred--get "/")))
             (port (aref (process-contact client :local) 4)))
        (should (ws-test-deferred--wait (lambda () (not (process-live-p client)))))
        (should (string-match-p
                 (format "\\`[^\t\n]+\t127\\.0\\.0\\.1\t%d\taccept from 127\\.0\\.0\\.1\n\\'"
                         port)
                 (ws-test-log--text log)))))))

(ert-deftest ws/log-disabled-keeps-admission-without-buffer ()
  "Without a log buffer, admission stays active without creating a log buffer."
  (ws-test-log--with server nil
    (should (functionp (process-contact (ws-process server) :log)))
    (should-not (plist-member (process-plist (ws-process server)) :log-buffer))))

(provide 'web-server-test)
