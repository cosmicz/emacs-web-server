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

(provide 'web-server-test)
