;;; web-server.el --- Emacs Web Server -*- lexical-binding: t -*-

;; Copyright (C) 2013-2014 Free Software Foundation, Inc.
;; Copyright (C) 2026 OC (cosmicz@github)
;; Modified by OC (cosmicz@github), 2026: Add one-shot deferred replies, accept-failure logging, and bounded byte-framed request dispatch; expose the raw request-target, raw query and decoded form params. Also prune dispatched keep-alive and WebSocket requests on peer close and stop.

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Maintainer: Eric Schulte <schulte.eric@gmail.com>
;; Version: 0.1.2
;; Package-Requires: ((emacs "24.1") (cl-lib "0.6"))
;; Keywords: http, server, network
;; URL: https://github.com/eschulte/emacs-web-server

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

;;; Commentary:

;; A web server in Emacs running handlers written in Emacs Lisp.
;;
;; Full support for GET and POST requests including URL-encoded
;; parameters and multipart/form data.  Supports web sockets.
;;
;; See the examples/ directory for examples demonstrating the usage of
;; the Emacs Web Server.  The following launches a simple "hello
;; world" server.
;;
;;     (ws-start
;;      '(((lambda (_) t) .                         ; match every request
;;         (lambda (request)                        ; reply with "hello world"
;;           (with-slots (process) request
;;             (ws-response-header process 200 '("Content-type" . "text/plain"))
;;             (process-send-string process "hello world")))))
;;      9000)

;;; Code:
(require 'web-server-status-codes)
(require 'mail-parse)             ; to parse multipart data in headers
(require 'mm-encode)              ; to look-up mime types for files
(require 'url-util)               ; to decode url-encoded params
(require 'eieio)
(require 'cl-lib)

(defclass ws-server ()
  ((handlers :initarg :handlers :accessor ws-handlers :initform nil)
   (process  :initarg :process  :accessor ws-process  :initform nil)
   (port     :initarg :port     :accessor ws-port     :initform nil)
   (requests :initarg :requests :accessor ws-requests :initform nil)
   (frame-settings :accessor ws-frame-settings :initform nil)
   (frame-connections :accessor ws-frame-connections :initform (make-hash-table :test #'eq))
   (frame-sequence :accessor ws-frame-sequence :initform 0)
   (frame-stopped :accessor ws-frame-stopped :initform nil)))

(defclass ws-request ()
  ((process  :initarg :process  :accessor ws-process  :initform nil)
   (pending  :initarg :pending  :accessor ws-pending  :initform "")
   (context  :initarg :context  :accessor ws-context  :initform nil)
   (boundary :initarg :boundary :accessor ws-boundary :initform nil)
   (index    :initarg :index    :accessor ws-index    :initform 0)
   (active   :initarg :active   :accessor ws-active   :initform nil)
   (headers  :initarg :headers  :accessor ws-headers  :initform (list nil))
   (body     :initarg :body     :accessor ws-body     :initform "")
   (frame-state :accessor ws-frame-state :initform nil)
   (frame-limits :accessor ws-frame-limits :initform nil)
   (frame-error :accessor ws-frame-error :initform nil)
   (frame-header-end :accessor ws-frame-header-end :initform nil)
   (frame-length :accessor ws-frame-length :initform nil)
   (frame-request-line :accessor ws-frame-request-line :initform nil)
   (frame-fields :accessor ws-frame-fields :initform nil)
   (frame-form-params :accessor ws-frame-form-params :initform nil)
   (frame-deadline :accessor ws-frame-deadline :initform nil)
   (responder :initarg :responder :accessor ws-request-responder
              :initform nil)))

(defvar ws-deferred-input-limit 8192
  "Default bound on bytes accepted after a request is deferred.
Bytes received on a deferred connection after `ws-defer' are counted
against this bound and discarded; exceeding it closes the connection.")

(defclass ws-responder ()
  ((request     :initarg :request :accessor ws-responder-request)
   (state       :initform 'pending :accessor ws-responder-state)
   (on-close    :initarg :on-close :initform nil)
   (input-limit :initarg :input-limit :initform nil)
   (input-bytes :initform 0 :accessor ws-responder-input-bytes))
  "One pending buffered reply for a deferred request.
See `ws-defer'.")

(defvar ws-servers nil
  "List holding all web servers.")

(defvar ws-log-time-format "%Y.%m.%d.%H.%M.%S.%N"
  "Logging time format passed to `format-time-string'.")

(defvar ws-guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "This GUID is defined in RFC6455.")

;;;###autoload
(defun ws-start (handlers port &optional log-buffer &rest network-args)
  "Start a server using HANDLERS and return the server object.

HANDLERS may be a single function (which is then called on every
request) or a list of conses of the form (MATCHER . FUNCTION),
where the FUNCTION associated with the first successful MATCHER
is called.  Handler functions are called with two arguments, the
process and the request object.

A MATCHER may be either a function (in which case it is called on
the request object) or a cons cell of the form (KEYWORD . STRING)
in which case STRING is matched against the value of the header
specified by KEYWORD.

Any supplied NETWORK-ARGS are assumed to be keyword arguments for
`make-network-process' to which they are passed directly.

Framing settings are validated and frozen before listener allocation.  The
incomplete cap counts zero-byte peers too.  A full cap silently refuses new
peers until release or lazy reclamation on another accept, against admission
or first-input time plus the interval.  No timer closes quiet peers.  Wider
:host bindings expose this pre-handler path; Clime defaults to 127.0.0.1.
The cap bounds framing state, not deferred/streaming/WebSocket lifetimes.

For example, the following starts a simple hello-world server on
port 8080.

  (ws-start
   (lambda (request)
     (with-slots (process headers) request
       (process-send-string process
        \"HTTP/1.1 200 OK\\r\\nContent-Type: text/plain\\r\\n\\r\\nhello world\")))
   8080)

Equivalently, the following starts an identical server using a
function MATCH and the `ws-response-header' convenience
function.

  (ws-start
   `(((lambda (_) t) .
      (lambda (request)
        (with-slots ((proc process)) request
          (ws-response-header proc 200 '(\"Content-Type\" . \"text/plain\"))
          (process-send-string proc \"hello world\")))))
   8080)

"
  (let* ((settings (ws--frame-settings))
         (server (make-instance 'ws-server :handlers handlers :port port))
         (log (when log-buffer (get-buffer-create log-buffer))))
    (setf (ws-frame-settings server) settings)
    (setf (ws-process server)
          (apply
           #'make-network-process
           :name "ws-server"
           :service (ws-port server)
           :filter 'ws-filter
           :server t
           :nowait (< emacs-major-version 26)
           :family 'ipv4
           :coding 'no-conversion
           :plist (append (list :server server)
                          (when log (list :log-buffer log)))
           :log (lambda (proc client message)
                  (ws--frame-log server proc client message))
           network-args))
    (push server ws-servers)
    server))

(defun ws-stop (server)
  "Stop SERVER.
Pending deferred responders end as `stopped' before their connections
are deleted.  Detach all requests before closing callbacks run."
  (setf (ws-frame-stopped server) t)
  (let ((requests (copy-sequence (ws-requests server)))
        (snapshot nil))
    ;; Reentrant callbacks must see the stopped state, including dead entries.
    (setf (ws-requests server) nil)
    (setq ws-servers (delq server ws-servers))
    (maphash (lambda (proc entry) (push (cons proc entry) snapshot))
             (ws-frame-connections server))
    (dolist (pair snapshot)
      (when (eq (cdr pair) (gethash (car pair) (ws-frame-connections server)))
        (ws--frame-close server (car pair) (ws--frame-entry-prior (cdr pair)))))
    (dolist (request requests)
      (let ((responder (ws-request-responder request)))
        (when responder
          (ws--responder-finish responder 'stopped))))
    (dolist (proc (append (mapcar #'ws-process requests) (list (ws-process server))))
      (when (process-live-p proc) (delete-process proc)))))

(defun ws-stop-all ()
  "Stop all servers in `ws-servers'."
  (interactive)
  (mapc #'ws-stop ws-servers))

(defvar ws-http-common-methods '(GET HEAD POST PUT DELETE TRACE)
  "HTTP methods from http://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html.")

(defvar ws-http-method-rx
  (format "^\\(%s\\) \\([^[:space:]]+\\) \\([^[:space:]]+\\)$"
          (mapconcat #'symbol-name ws-http-common-methods "\\|")))

(defun ws-parse-query-string (string)
  "Thin wrapper around `url-parse-query-string'."
  (mapcar (lambda (pair) (cons (cl-first pair) (cl-second pair)))
          (url-parse-query-string string nil 'allow-newlines)))

(defun ws-parse (proc string)
  "Parse HTTP headers in STRING reporting errors to PROC."
  (cl-flet ((to-keyword (s) (intern (concat ":" (upcase s)))))
    (cond
     ;; Method
     ((string-match ws-http-method-rx string)
      (let ((method (to-keyword (match-string 1 string)))
            (url (match-string 2 string)))
        (if (string-match "?" url)
            (cons (cons method (substring url 0 (match-beginning 0)))
                  (ws-parse-query-string
                   (url-unhex-string (substring url (match-end 0)))))
          (list (cons method url)))))
     ;; Authorization
     ((string-match "^AUTHORIZATION: \\([^[:space:]]+\\) \\(.*\\)$" string)
      (let ((protocol (to-keyword (match-string 1 string)))
            (credentials (match-string 2 string)))
        (list (cons :AUTHORIZATION
                    (cons protocol
                          (cl-case protocol
                            (:BASIC
                             (let ((cred (base64-decode-string credentials)))
                               (if (string-match ":" cred)
                                   (cons (substring cred 0 (match-beginning 0))
                                         (substring cred (match-end 0)))
                                 (ws-error proc "bad credentials: %S" cred))))
                            (t (ws-error proc "un-support protocol: %s"
                                         protocol))))))))
     ;; All other headers
     ((string-match "^\\([^[:space:]]+\\): \\(.*\\)$" string)
      (list (cons (to-keyword (match-string 1 string))
                  (match-string 2 string))))
     (:otherwise (ws-error proc "bad header: %S" string) nil))))

(defun ws-trim (string)
  (while (and (> (length string) 0)
              (or (and (string-match "[\r\n]" (substring string -1))
                       (setq string (substring string 0 -1)))
                  (and (string-match "[\r\n]" (substring string 0 1))
                       (setq string (substring string 1))))))
  string)

(defun ws-parse-multipart/form (proc string)
  ;; ignore empty and non-content blocks
  (when (string-match "Content-Disposition:[[:space:]]*\\(.*\\)\r\n" string)
    (let ((dp (cdr (mail-header-parse-content-disposition
                    (match-string 1 string))))
          (last-index (match-end 0))
          index)
      ;; every line up until the double \r\n is a header
      (while (and (setq index (string-match "\r\n" string last-index))
                  (not (= index last-index)))
        (setcdr (last dp) (ws-parse proc (substring string last-index index)))
        (setq last-index (+ 2 index)))
      ;; after double \r\n is all content
      (cons (cdr (assoc 'name dp))
            (cons (cons 'content (substring string (+ 2 last-index)))
                  dp)))))

(defun ws-filter (proc string)
  (let ((server (process-get proc :server)))
    (when (and (process-live-p proc) (not (ws-frame-stopped server)))
      (with-slots (handlers requests) (plist-get (process-plist proc) :server)
        (unless (cl-find-if (lambda (c) (equal proc (ws-process c))) requests)
          (push (make-instance 'ws-request :process proc) requests))
        (let* ((request (cl-find-if (lambda (c) (equal proc (ws-process c))) requests))
               (responder (ws-request-responder request)))
          (if (and responder (ws--responder-open-p responder))
              ;; Deferred (pending or writing its reply): later input is
              ;; counted against the bound and discarded; it never reaches
              ;; pending or a second handler.
              (ws--deferred-input responder string)
            (ws--frame-append request string)
            (unless (ws-active request) ; don't re-start if request is being parsed
              (setf (ws-active request) t)
              (let* ((thrown t)
                     (result (catch 'close-connection
                               (prog1 (if (ws-parse-request request)
                                          (ws-call-handler request handlers)
                                        :keep-alive)
                                 (setq thrown nil))))
                     (responder (ws-request-responder request)))
                (cond
                 ;; A handler error after deferral: the 500 has been sent.
                 ((and thrown responder)
                  (ws--responder-finish responder 'failed))
                 ;; The handler deferred: keep the connection for `ws-respond'.
                 (responder nil)
                 ((eq result :keep-alive)
                  (when (eq (ws-frame-state request) 'complete)
                    (ws--watch-request server request)))
                 (t
                  ;; Properly shut down processes requiring an ending (e.g., chunked)
                  (let ((ender (plist-get (process-plist proc) :ender)))
                    (when ender (process-send-string proc ender)))
                  (setq requests (cl-remove-if (lambda (r) (eql proc (ws-process r))) requests))
                  (delete-process proc)))))))))))

(defun ws--prune-request (server request)
  "Detach exactly REQUEST from SERVER without invalidating the object."
  (setf (ws-requests server) (delq request (ws-requests server))))

(defun ws--watch-request (server request)
  "Prune dispatched REQUEST from SERVER when its connection closes.
Capture ownership after the handler, independently of the process plist.
Caller sentinels receive their actual events after terminal detachment."
  (let* ((proc (ws-process request))
         (prior (process-sentinel proc))
         (wrapper nil)
         (finished nil)
         (success nil))
    (if (not (process-live-p proc))
        ;; A yielding handler may already have delivered the terminal event.
        (ws--prune-request server request)
      (setq wrapper
            (lambda (process event)
              (cond
               ((process-live-p process)
                (when prior (funcall prior process event)))
               ((not finished)
                (setq finished t)
                (ws--prune-request server request)
                (when (eq wrapper (process-sentinel process))
                  (set-process-sentinel process prior))
                (when prior (funcall prior process event))))))
      (unwind-protect
          (progn
            (set-process-sentinel proc wrapper)
            ;; EOF can land between the first liveness check and installation.
            (unless (process-live-p proc)
              (setq finished t)
              (ws--prune-request server request)
              (when (eq wrapper (process-sentinel proc))
                (set-process-sentinel proc prior)))
            (setq success t))
        (unless success
          (ws--prune-request server request)
          ;; Cleanup cannot replace the original setup error or quit.
          (condition-case nil
              (when (eq wrapper (process-sentinel proc))
                (set-process-sentinel proc prior))
            ((error quit) nil))
          (condition-case nil
              (when (process-live-p proc) (delete-process proc))
            ((error quit) nil)))))))

;;; Deferred buffered responses

(cl-defun ws-defer (request &key on-close (input-limit ws-deferred-input-limit))
  "Defer the reply to REQUEST and return its `ws-responder'.
Call this from a handler, then return.  The connection stays open until
exactly one terminal event wins: `ws-respond' (completed, or failed when
the reply cannot be written), `ws-responder-abort' (aborted), a client
disconnect (disconnected), more than INPUT-LIMIT bytes received after
deferral (overflow), a handler error after deferral (failed) or
`ws-stop' (stopped).  The winner releases the request and the connection
once and calls ON-CLOSE, a function of (RESPONDER REASON), exactly once;
errors in ON-CLOSE are caught and logged.  The reply is buffered and the
connection closes after it: there is no keep-alive and no second handler
on a deferred connection.  While `ws-respond' writes the reply the
responder's state is `responding'."
  (when (ws-request-responder request)
    (error "ws-defer: request is already deferred"))
  (let ((responder (make-instance 'ws-responder
                                  :request request
                                  :on-close on-close
                                  :input-limit input-limit)))
    (setf (ws-request-responder request) responder)
    (set-process-sentinel (ws-process request) #'ws--deferred-sentinel)
    responder))

(defun ws--responder-open-p (responder)
  "Return non-nil while RESPONDER has not reached a terminal state.
It is open while `pending' and while `responding', that is while
`ws-respond' holds the reply and is writing it."
  (memq (ws-responder-state responder) '(pending responding)))

(defun ws--responder-finish (responder reason)
  "End open RESPONDER with REASON once.
Release its request and connection and call its ON-CLOSE.  Return t when
this call made the transition, nil when RESPONDER had already ended.
An abort, disconnect, overflow or stop during an in-flight reply still
ends it here, exactly once."
  (when (ws--responder-open-p responder)
    (setf (ws-responder-state responder) reason)
    (let* ((request (ws-responder-request responder))
           (proc (ws-process request))
           (server (plist-get (process-plist proc) :server)))
      (when server
        (setf (ws-requests server) (delq request (ws-requests server))))
      (when (process-live-p proc)
        (set-process-sentinel proc #'ignore)
        (delete-process proc)))
    (let ((on-close (slot-value responder 'on-close)))
      (when on-close
        (condition-case err
            (funcall on-close responder reason)
          (error (message "ws-responder: on-close error: %S" err)))))
    t))

(defun ws--deferred-input (responder string)
  "Count STRING against pending RESPONDER's input bound and discard it."
  (let ((total (+ (ws-responder-input-bytes responder) (length string))))
    (setf (ws-responder-input-bytes responder) total)
    (when (> total (slot-value responder 'input-limit))
      (ws--responder-finish responder 'overflow))))

(defun ws--deferred-sentinel (proc _event)
  "End a pending deferred responder on PROC when its connection closes."
  (unless (process-live-p proc)
    (let* ((server (plist-get (process-plist proc) :server))
           (request (and server
                         (cl-find-if (lambda (r) (eq proc (ws-process r)))
                                     (ws-requests server))))
           (responder (and request (ws-request-responder request))))
      (when responder
        (ws--responder-finish responder 'disconnected)))))

(defun ws-respond (responder code headers body)
  "Send one buffered reply for pending RESPONDER, then close.
Send status CODE with HEADERS, an alist, and BODY through
`ws-response-header' and `ws-send'.  Return t when the reply was written
and RESPONDER completed.  Return nil, without signalling, when RESPONDER
had already ended or was already replying, or when the reply could not be
written, which ends it as `failed'.

The reply is claimed before the first write: RESPONDER moves to
`responding', so a completion attempted while bytes are being written
(other process output may run between write bunches) writes nothing.
An abort, disconnect, overflow or stop during the write still ends the
responder once, and this call then returns nil."
  (when (eq (ws-responder-state responder) 'pending)
    (setf (ws-responder-state responder) 'responding)
    (let ((proc (ws-process (ws-responder-request responder))))
      (if (condition-case nil
              (progn
                (apply #'ws-response-header proc code headers)
                (when body (ws-send proc body))
                (let ((ender (plist-get (process-plist proc) :ender)))
                  (when ender (process-send-string proc ender)))
                t)
            (error nil))
          (ws--responder-finish responder 'completed)
        (ws--responder-finish responder 'failed)
        nil))))

(defun ws-responder-abort (responder)
  "End pending RESPONDER without a reply and close its connection.
Return t when this call ended it, nil when it had already ended."
  (ws--responder-finish responder 'aborted))

(defvar ws-request-body-limit 8388608
  "Absolute request body ceiling in octets, captured by `ws-start'.
Must be a nonnegative integer.  Transport refusals precede app handlers.")
(defvar ws-request-header-limit 65536
  "Request header ceiling in octets, captured by `ws-start'.
Includes the request line and all of the terminating CRLFCRLF.")
(defvar ws-request-framing-timeout 30
  "Positive finite seconds from first input until a complete request frame.
Captured by `ws-start', never renewed by input.  Expiry is lazy: later input
gets 408, later admission may reclaim a quiet peer.  No timer closes peers.")
(defvar ws-request-incomplete-limit 16
  "Positive finite count of admitted incomplete sockets, including zero-byte peers.
Captured before listener allocation.  A full cap silently refuses new peers
until completion/EOF/stop or lazy expiry frees capacity.  This bounds framing
state only, not deferred, streaming, WebSocket or kernel-backlog lifetimes.")
(put 'ws-request-setting-error 'error-conditions '(ws-request-setting-error error))
(put 'ws-request-setting-error 'error-message "Invalid request framing setting")

(defun ws--frame-settings ()
  "Validate and return the current body/header/time configuration."
  (unless (and (integerp ws-request-body-limit) (>= ws-request-body-limit 0))
    (signal 'ws-request-setting-error '(ws-request-body-limit)))
  (unless (and (integerp ws-request-header-limit) (> ws-request-header-limit 0))
    (signal 'ws-request-setting-error '(ws-request-header-limit)))
  (unless (and (numberp ws-request-framing-timeout)
               (> ws-request-framing-timeout 0)
               (= (- (float ws-request-framing-timeout)
                     (float ws-request-framing-timeout)) 0))
    (signal 'ws-request-setting-error '(ws-request-framing-timeout)))
  (unless (and (integerp ws-request-incomplete-limit) (> ws-request-incomplete-limit 0))
    (signal 'ws-request-setting-error '(ws-request-incomplete-limit)))
  (list ws-request-body-limit ws-request-header-limit ws-request-framing-timeout
        ws-request-incomplete-limit))


;;; Bounded admission and request framing lifetime.

(cl-defstruct (ws--frame-entry (:constructor ws--frame-entry-create))
  admitted deadline request prior sentinel order)

(defun ws--frame-now ()
  "Return the framing clock in seconds; isolated for deterministic tests."
  (float-time))

(defun ws--frame-release (server proc &optional discard)
  "Release PROC's incomplete entry on SERVER once, before any socket close.
When DISCARD is non-nil also detach its incomplete request.  A stale release
cannot release another process's entry.  Return the removed entry or nil."
  (let* ((table (ws-frame-connections server)) (entry (gethash proc table)))
    (when entry
      ;; delete-process can synchronously dispatch other filters and sentinels.
      (remhash proc table)
      (let ((request (ws--frame-entry-request entry)))
        (when (and discard request)
          (setf (ws-frame-state request) 'retired
                (ws-requests server) (delq request (ws-requests server)))))
      (when (eq (process-sentinel proc) (ws--frame-entry-sentinel entry))
        (set-process-sentinel proc (ws--frame-entry-prior entry)))
      entry)))

(defun ws--frame-close (server proc prior)
  "Silently release and close PROC on SERVER, retaining its PRIOR sentinel."
  (ws--frame-release server proc t)
  (set-process-sentinel proc prior)
  (when (process-live-p proc) (delete-process proc)))

(defun ws--frame-reclaim (server)
  "Reclaim expired incomplete sockets from a stable admission-order snapshot.
Recheck membership and dispatch state after each potentially reentrant close."
  (let ((snapshot nil) (table (ws-frame-connections server)))
    (maphash (lambda (proc entry) (push (cons proc entry) snapshot)) table)
    (setq snapshot
          (sort snapshot (lambda (a b) (< (ws--frame-entry-order (cdr a))
                                          (ws--frame-entry-order (cdr b))))))
    (dolist (pair snapshot)
      (let* ((proc (car pair)) (entry (cdr pair))
             (request (ws--frame-entry-request entry)))
        (when (and (not (ws-frame-stopped server)) (eq entry (gethash proc table))
                   (or (null request) (ws--frame-waiting-p request))
                   (>= (ws--frame-now)
                       (or (ws--frame-entry-deadline entry)
                           (+ (ws--frame-entry-admitted entry)
                              (nth 2 (ws-frame-settings server))))))
          (ws--frame-close server proc (ws--frame-entry-prior entry)))))))

(defun ws--frame-admit (server proc)
  "Admit PROC to SERVER's bounded incomplete registry, or silently close it.
Reserve before watcher installation.  Any unsuccessful nonlocal exit releases
and closes the peer; errors are contained by the enclosing log callback."
  (let ((prior nil) (committed nil))
    (unwind-protect
        (progn
          (setq prior (process-sentinel proc))
          (unless (ws-frame-stopped server) (ws--frame-reclaim server))
          ;; Reclamation may have stopped the server or admitted another peer.
          (when (and (not (ws-frame-stopped server)) (process-live-p proc)
                     (< (hash-table-count (ws-frame-connections server))
                        (nth 3 (ws-frame-settings server))))
            (let* ((entry (ws--frame-entry-create
                           :admitted (ws--frame-now) :prior prior
                           :order (cl-incf (ws-frame-sequence server))))
                   (sentinel
                    (lambda (p event)
                      (unless (process-live-p p) (ws--frame-release server p t))
                      ;; In particular, live "open from" does not release.
                      (when prior (funcall prior p event)))))
              (setf (ws--frame-entry-sentinel entry) sentinel)
              (puthash proc entry (ws-frame-connections server))
              (set-process-sentinel proc sentinel)
              (setq committed t)))
          committed)
      (unless committed
        ;; Protect cleanup steps separately so one failure cannot skip close.
        (ignore-errors (ws--frame-release server proc t))
        (ignore-errors (set-process-sentinel proc prior))
        (ignore-errors (when (process-live-p proc) (delete-process proc)))))))

(defun ws--frame-log (server listener client message)
  "Admit CLIENT to SERVER; log MESSAGE through LISTENER without waiter errors.
Nil CLIENT denotes an accept failure and has no framing registry effects.
Optional log write failure never revokes an otherwise committed admission."
  (condition-case nil
      (progn
        (when client (ws--frame-admit server client))
        (condition-case nil
            (let ((buffer (process-get listener :log-buffer)))
              (when buffer
                (let ((contact (and client (process-contact client))))
                  (with-current-buffer buffer
                    (goto-char (point-max))
                    (insert (format "%s\t%s\t%s\t%s"
                                    (format-time-string ws-log-time-format)
                                    (cl-first contact) (cl-second contact) message))))))
          (error nil)))
    (error nil)))

(defun ws--frame-waiting-p (request)
  "Return non-nil while REQUEST still owns an incomplete frame."
  (memq (ws-frame-state request) '(headers body)))

(defun ws--frame-retire (request)
  "Release REQUEST's framing admission before dispatch or normal refusal."
  (let* ((proc (ws-process request)) (server (and proc (process-get proc :server))))
    (when server (ws--frame-release server proc))))

(defun ws--frame-send (request code)
  "Attempt zero-body refusal CODE from the ordinary input callback.
A successful write is not proof of delivery.  Callers retire even on failure."
  (let ((proc (ws-process request)))
    (when (and proc (process-live-p proc) (eq (process-status proc) 'open))
      (condition-case nil
          (progn (ws-response-header proc code '("Content-Length" . "0")
                                     '("Connection" . "close")) t)
        (error nil)))))

(defun ws--frame-begin (request)
  "Capture REQUEST's framing settings and fixed deadline at its first input."
  (let* ((proc (ws-process request)) (server (and proc (process-get proc :server)))
         (settings (if server (ws-frame-settings server) (ws--frame-settings)))
         (entry (and server (gethash proc (ws-frame-connections server)))))
    (setf (ws-frame-state request) 'headers (ws-frame-limits request) settings)
    (when proc
      (setf (ws-frame-deadline request) (+ (ws--frame-now) (nth 2 settings)))
      (when entry
        (setf (ws--frame-entry-request entry) request
              (ws--frame-entry-deadline entry) (ws-frame-deadline request))))))

(defconst ws--frame-token "[!#$%&'*+.^_`|~0-9A-Za-z-]+"
  "HTTP field-name and transfer-coding token pattern.")

(defun ws--frame-transfer-p (value)
  "Return non-nil for a nonempty, syntactically valid coding list VALUE.
Only syntax is recognized: every request transfer coding is unsupported."
  (let* ((quoted (concat "\"\\(?:[^\"\\\\\0-\10\12-\37\177]\\|"
                         "\\\\[\t -~\200-\377]\\)*\""))
         (parameter (concat ws--frame-token "[ \t]*=[ \t]*\\(?:"
                            ws--frame-token "\\|" quoted "\\)"))
         (coding (concat ws--frame-token "\\(?:[ \t]*;[ \t]*" parameter "\\)*")))
    (string-match-p (concat "\\`[ \t]*" coding "\\(?:[ \t]*,[ \t]*" coding
                            "\\)*[ \t]*\\'") value)))

(defun ws--frame-length (value limit)
  "Return unsigned decimal VALUE bounded by LIMIT, or a refusal code.
Return a cons whose car is the length on success.  Compare canonical decimal
strings before integer conversion, including arbitrarily many leading zeroes."
  (if (not (string-match-p "\\`[0-9]+\\'" value)) 400
    (let* ((canonical (replace-regexp-in-string "\\`0+" "" value))
           (ceiling (number-to-string limit)))
      (if (or (> (length canonical) (length ceiling))
              (and (= (length canonical) (length ceiling))
                   (string-lessp ceiling canonical)))
          413
        (list (if (equal canonical "") 0 (string-to-number canonical)))))))

(defun ws--frame-read-head (request head)
  "Validate bounded HEAD and store framing metadata on REQUEST.
Defer request/query/auth and content decoding until the complete frame.
Return a refusal code or nil."
  (catch 'refusal
    (let* ((lines (split-string (substring head 0 (- (length head) 4)) "\r\n"))
           (request-line (pop lines)) (fields nil) (case-fold-search t))
      (unless (and request-line (string-match ws-http-method-rx request-line))
        (throw 'refusal 400))
      (dolist (line lines)
        (unless (string-match (concat "\\`\\(" ws--frame-token "\\):[ \t]*\\(.*\\)\\'") line)
          (throw 'refusal 400))
        (let ((name (intern (concat ":" (upcase (match-string 1 line)))))
              (value (replace-regexp-in-string "[ \t]+\\'" "" (match-string 2 line))))
          (when (string-match-p "[\0-\10\12-\37\177]" value)
            (throw 'refusal 400))
          (push (cons name value) fields)))
      (setq fields (nreverse fields))
      (let* ((lengths (cl-remove-if-not (lambda (h) (eq (car h) :CONTENT-LENGTH)) fields))
             (codings (cl-remove-if-not (lambda (h) (eq (car h) :TRANSFER-ENCODING)) fields)))
        (when (or (> (length lengths) 1) (> (length codings) 1) (and lengths codings))
          (throw 'refusal 400))
        (when codings
          (throw 'refusal (if (ws--frame-transfer-p (cdar codings)) 501 400)))
        (let ((size (if lengths
                        (ws--frame-length (cdar lengths) (car (ws-frame-limits request)))
                      '(0))))
          (when (numberp size) (throw 'refusal size))
          (setf (ws-frame-length request) (car size))))
      (setf (ws-frame-request-line request) request-line
            (ws-frame-fields request) fields)
      nil)))

(defun ws--frame-header-end (old input)
  "Find the header terminator across OLD and INPUT without copying INPUT.
Return its end offset in their conceptual concatenation, or nil."
  (let* ((tail (min 3 (length old)))
         (bridge (concat (substring old (- (length old) tail))
                         (substring input 0 (min 3 (length input)))))
         (cross (string-match "\r\n\r\n" bridge))
         (inside (string-match "\r\n\r\n" input)))
    (cond (cross (+ (- (length old) tail) cross 4))
          (inside (+ (length old) inside 4)))))

(defun ws--frame-append (request input)
  "Append only budget-checked octets of INPUT to incomplete REQUEST.
A filter argument has already been allocated by Emacs; never make an
unbounded copy of that argument merely to discover it must be refused."
  (unless (ws-frame-state request) (ws--frame-begin request))
  (when (and (ws--frame-waiting-p request) (multibyte-string-p input))
    (setf (ws-frame-error request) 400))
  (when (and (ws--frame-waiting-p request) (not (ws-frame-error request)))
    (if (and (ws-frame-deadline request)
             (>= (ws--frame-now) (ws-frame-deadline request)))
        (setf (ws-frame-error request) 408)
      (let ((pending (ws-pending request)) (body-input input))
        (when (eq (ws-frame-state request) 'headers)
          (let ((end (ws--frame-header-end pending input))
                (limit (nth 1 (ws-frame-limits request))))
            (cond
             ((or (and end (> end limit))
                  (and (not end) (> (+ (length pending) (length input)) limit)))
              (setf (ws-frame-error request) 431))
             (end
              (let* ((used (- end (length pending)))
                     (head (concat pending (substring input 0 used))))
                (setf (ws-frame-error request) (ws--frame-read-head request head)
                      (ws-pending request) head
                      (ws-frame-header-end request) end
                      (ws-frame-state request) 'body)
                ;; Check the rest BEFORE making its substring or concatenation.
                (if (> (- (length input) used) (or (ws-frame-length request) 0))
                    (unless (ws-frame-error request) (setf (ws-frame-error request) 400))
                  (unless (ws-frame-error request)
                    (setq body-input (substring input used))))))
             (t (setf (ws-pending request) (concat pending input))))))
        (when (and (eq (ws-frame-state request) 'body) (not (ws-frame-error request)))
          (if (> (+ (- (length (ws-pending request)) (ws-frame-header-end request))
                    (length body-input)) (ws-frame-length request))
              (setf (ws-frame-error request) 400)
            (setf (ws-pending request) (concat (ws-pending request) body-input))))))))

(defun ws--frame-decode (request)
  "Decode the already complete REQUEST once, preserving its exact raw body."
  (let* ((proc (ws-process request)) (fields (ws-frame-fields request))
         (type (cdr (assq :CONTENT-TYPE fields))) (case-fold-search t))
    (setf (ws-headers request)
          (cons nil (append (ws-parse proc (ws-frame-request-line request))
                            (cl-mapcan (lambda (field)
                                         (if (eq (car field) :AUTHORIZATION)
                                             (ws-parse proc (concat "Authorization: " (cdr field)))
                                           (list field))) fields))))
    (when type
      (cl-destructuring-bind (name &rest data) (mail-header-parse-content-type type)
        (setf (ws-context request) (intern (downcase name))
              (ws-boundary request) (cdr (assq 'boundary data)))))
    (let ((forms
           (cl-case (ws-context request)
             (application/x-www-form-urlencoded
              (ws-parse-query-string (replace-regexp-in-string (regexp-quote "+") " " (ws-body request))))
             (multipart/form-data
              (let ((boundary (ws-boundary request)) (start 0) (parts nil) (next nil))
                (when boundary
                  (let ((delimiter (concat "\r\n--" (regexp-quote boundary))) (body (ws-body request)))
                    (while (setq next (string-match delimiter body start))
                      (let ((part (ws-parse-multipart/form proc (substring body start next))))
                        (when part (push part parts)))
                      (setq start (+ next 4 (length boundary))))))
                (nreverse parts))))))
      (setf (ws-frame-form-params request) forms)
      (setcdr (last (ws-headers request)) forms))))

(defun ws-parse-request (request)
  "Parse REQUEST only after one complete, bounded Content-Length frame.
Wait without decoding partial forms.  Raw body bytes are never trimmed."
  ;; Legacy direct parser callers supply pending, without a socket/filter.
  (unless (ws-frame-state request)
    (let ((input (ws-pending request)))
      (setf (ws-pending request) "")
      (ws--frame-append request input)))
  (when (ws-frame-error request)
    (ws--frame-retire request)
    (setf (ws-frame-state request) 'retired)
    (ws--frame-send request (ws-frame-error request))
    (throw 'close-connection nil))
  (if (and (eq (ws-frame-state request) 'body)
           (= (- (length (ws-pending request)) (ws-frame-header-end request))
              (ws-frame-length request)))
      (progn
        (setf (ws-body request) (substring (ws-pending request) (ws-frame-header-end request))
              (ws-frame-state request) 'complete)
        (ws--frame-retire request)
        (ws--frame-decode request)
        t)
    (setf (ws-active request) nil)
    nil))

(defun ws-request-target (request)
  "Return REQUEST's request-target exactly as received, or nil.
It is read from the request line that framing accepted, so it is nil
until the complete head has been accepted and for a refused head.  No
percent-decoding and no \"+\" mapping is applied.  The caller's match
data is preserved."
  (let ((line (ws-frame-request-line request)))
    (when line
      (save-match-data
        ;; Match as framing does, whatever the caller's `case-fold-search'.
        (let ((case-fold-search t))
          (when (string-match ws-http-method-rx line)
            (match-string 2 line)))))))

(defun ws-request-raw-query (request)
  "Return the raw query of REQUEST's request-target, or nil.
The query is the text after the first \"?\", exactly as received: no
percent-decoding and no \"+\" mapping.  It is \"\" for a target ending
in \"?\", and nil when the target has no \"?\" or was not parsed.
The caller's match data is preserved."
  (let ((target (ws-request-target request)))
    (when target
      (save-match-data
        (when (string-match "\\?" target)
          (substring target (match-end 0)))))))

(defun ws-request-form-params (request)
  "Return the form params decoded from REQUEST's body, or nil.
For an urlencoded body they are the decoded (NAME . VALUE) pairs; for a
multipart body the parsed parts.  It is the same list that is appended
to the header alist, not a copy, so nothing is parsed here.  It is nil
until the complete request has been decoded and when the media type is
not a form type.  The request owns the list; callers must not modify it."
  (ws-frame-form-params request))

(defun ws-call-handler (request handlers)
  (catch 'matched-handler
    (when (functionp handlers)
      (throw 'matched-handler
             (condition-case e (funcall handlers request)
               (error (ws-error (ws-process request) "Caught Error: %S" e)))))
    (mapc (lambda (handler)
            (let ((match (car handler))
                  (function (cdr handler)))
              (when (or (and (consp match)
                             (assoc (car match) (ws-headers request))
                             (string-match (cdr match)
                                           (cdr (assoc (car match)
                                                       (ws-headers request)))))
                        (and (functionp match) (funcall match request)))
                (throw 'matched-handler
                       (condition-case e (funcall function request)
                         (error (ws-error (ws-process request)
                                          "Caught Error: %S" e)))))))
          handlers)
    (ws-error (ws-process request) "no handler matched request: %S"
              (ws-headers request))))

(defun ws-error (proc msg &rest args)
  (let ((buf (plist-get (process-plist proc) :log-buffer))
        (c (process-contact proc)))
    (when buf
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format "%s\t%s\t%s\tWS-ERROR: %s"
                        (format-time-string ws-log-time-format)
                        (cl-first c) (cl-second c)
                        (apply #'format msg args)))))
    (apply #'ws-send-500 proc msg args)))


;;; Web Socket
;; Implement to conform to http://tools.ietf.org/html/rfc6455.

;; The `ws-message' object is used to hold state across multiple calls
;; of the process filter on the websocket network process.  The fields
;; play the following roles.
;; process ------ holds the process itself, used for communication
;; pending ------ holds text received from the client but not yet parsed
;; active ------- indicates that parsing is active to avoid re-entry
;;                of the `ws-web-socket-parse-messages' function
;; new ---------- indicates that new text was received during parsing
;;                and causes `ws-web-socket-parse-messages' to be
;;                called again after it terminates
;; data --------- holds the data of parsed messages
;; handler ------ holds the user-supplied function of two arguments
;;                called on the process and the data of parsed
;;                messages
(defclass ws-message ()                 ; web socket message object
  ((process  :initarg :process  :accessor ws-process  :initform "")
   (pending  :initarg :pending  :accessor ws-pending  :initform "")
   (active   :initarg :active   :accessor ws-active   :initform nil)
   (new      :initarg :new      :accessor ws-new      :initform nil)
   (data     :initarg :data     :accessor ws-data     :initform "")
   (handler  :initarg :handler  :accessor ws-handler  :initform "")))

(defun ws-web-socket-connect (request handler)
  "Establish a web socket connection with REQUEST.
On success return its process; the handler must return `:keep-alive'
or throw it to `close-connection' to retain the connection.
If no web-socket connection is
established (e.g., because REQUEST is not attempting to establish
a connection) then no actions are taken and nil is returned.

Second argument HANDLER should be a function of two arguments,
the process and a string, which will be called on all complete
messages as they are received and parsed from the network."
  (with-slots (process headers) request
    (when (assoc :SEC-WEBSOCKET-KEY headers)
      ;; Accept the connection
      (ws-response-header process 101
        (cons "Upgrade" "websocket")
        (cons "Connection" "upgrade")
        (cons "Sec-WebSocket-Accept"
              (ws-web-socket-handshake
               (cdr (assoc :SEC-WEBSOCKET-KEY headers)))))
      ;; Setup the process filter
      (set-process-coding-system process 'binary)
      (set-process-plist
       process (list :message (make-instance 'ws-message
                                :handler handler :process process)))
      (set-process-filter process 'ws-web-socket-filter)
      process)))

(defun ws-web-socket-filter (process string)
  (let ((message (plist-get (process-plist process) :message)))
    (if (ws-active message) ; don't re-start if message is being parsed
        (setf (ws-new message) string)
      (setf (ws-pending message) (concat (ws-pending message) string))
      (setf (ws-active message) t)
      (ws-web-socket-parse-messages message))
    (setf (ws-active message) nil)))

(defun ws-web-socket-mask (masking-key data)
  (let ((masking-data (apply #'concat (make-list (+ 1 (/ (length data) 4))
                                                 masking-key))))
    (apply #'string (cl-mapcar #'logxor masking-data data))))

;; Binary framing protocol
;; from http://tools.ietf.org/html/rfc6455#section-5.2
;;
;;  0                   1                   2                   3
;;  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
;; +-+-+-+-+-------+-+-------------+-------------------------------+
;; |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
;; |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
;; |N|V|V|V|       |S|             |   (if payload len==126/127)   |
;; | |1|2|3|       |K|             |                               |
;; +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
;; |     Extended payload length continued, if payload len == 127  |
;; + - - - - - - - - - - - - - - - +-------------------------------+
;; |                               |Masking-key, if MASK set to 1  |
;; +-------------------------------+-------------------------------+
;; | Masking-key (continued)       |          Payload Data         |
;; +-------------------------------- - - - - - - - - - - - - - - - +
;; :                     Payload Data continued ...                :
;; + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
;; |                     Payload Data continued ...                |
;; +---------------------------------------------------------------+
;;
(defun ws-web-socket-parse-messages (message)
  "Web socket filter to pass whole frames to the client.
See RFC6455."
  (with-slots (process active pending data handler new) message
    (let ((index 0))
      (cl-labels ((int-to-bits (int size)
                    (let ((result (make-bool-vector size nil)))
                      (mapc (lambda (place)
                              (let ((val (expt 2 place)))
                                (when (>= int val)
                                  (setq int (- int val))
                                  (aset result place t))))
                            (reverse (number-sequence 0 (- size 1))))
                      (reverse (append result nil))))
                  (bits-to-int (bits)
                    (let ((place 0))
                      (apply #'+
                       (mapcar (lambda (bit)
                                 (prog1 (if bit (expt 2 place) 0) (cl-incf place)))
                               (reverse bits)))))
                  (bits (length)
                    (apply #'append
                           (mapcar (lambda (int) (int-to-bits int 8))
                                   (cl-subseq
                                    pending index (cl-incf index length))))))
        (let (fin rsvs opcode mask pl mask-key)
          ;; Parse fin bit, rsvs bits and opcode
          (let ((byte (bits 1)))
            (setq fin (car byte)
                  rsvs (cl-subseq byte 1 4)
                  opcode
                  (let ((it (bits-to-int (cl-subseq byte 4))))
                    (cl-case it
                      (0 :CONTINUATION)
                      (1 :TEXT)
                      (2 :BINARY)
                      ((3 4 5 6 7) :NON-CONTROL)
                      (8 :CLOSE)
                      (9 :PING)
                      (10 :PONG)
                      ((11 12 13 14 15) :CONTROL)
                      ;; If an unknown opcode is received, the receiving
                      ;; endpoint MUST _Fail the WebSocket Connection_.
                      (t (ws-error process
                                   "Web Socket Fail: bad opcode %d" it))))))
          (unless (cl-every #'null rsvs)
            ;; MUST be 0 unless an extension is negotiated that defines
            ;; meanings for non-zero values.
            (ws-error process "Web Socket Fail: non-zero RSV 1 2 or 3"))
          ;; Parse mask and payload length
          (let ((byte (bits 1)))
            (setq mask (car byte)
                  pl (bits-to-int (cl-subseq byte 1))))
          (unless (eq mask t)
            ;; All frames sent from client to server have this bit set to 1.
            (ws-error process "Web Socket Fail: client must mask data"))
          (cond
           ((= pl 126) (setq pl (bits-to-int (bits 2))))
           ((= pl 127) (setq pl (bits-to-int (bits 8)))))
          ;; unmask data
          (when mask (setq mask-key (cl-subseq pending index (cl-incf index 4))))
          (setq data (concat data
                             (ws-web-socket-mask
                              mask-key (cl-subseq pending index (+ index pl)))))
          (if fin
              ;; wipe the message state and call the handler
              (let ((it data))
                (setq data "" active nil pending "" new nil)
                ;; close on a close frame, otherwise call the handler
                (if (not (eql opcode :CLOSE))
                    (funcall handler process it)
                  (process-send-string process
                    (unibyte-string (logior (lsh 1 7) 8) 0))))
            ;; add any remaining un-parsed network data to pending
            (when (< (+ index pl) (length pending))
              (setq pending (substring pending (+ index pl)))))))
      ;; possibly re-parse any pending input
      (when (ws-new message) (ws-web-socket-parse-messages message)))))

(defun ws-web-socket-frame (string &optional opcode)
  "Frame STRING for web socket communication."
  (let* ((fin 1) ;; set to 0 if not final frame
         (len (length string))
         (opcode (cl-ecase (or opcode :TEXT) (:TEXT 1) (:BINARY 2))))
    ;; Does not do any masking which is only required of client communication
    (concat
     (cond
      ((< len 126) (unibyte-string (logior (lsh fin 7) opcode) len))
      ((< len 65536) (unibyte-string (logior (lsh fin 7) opcode) 126
                                     ;; extended 16-bit length
                                     (logand (lsh len -8) 255)
                                     (logand      len     255)))
      (t (unibyte-string (logior (lsh fin 7) opcode) 127
                         ;; more extended 64-bit length
                         (logand (lsh len -56) 255)
                         (logand (lsh len -48) 255)
                         (logand (lsh len -40) 255)
                         (logand (lsh len -32) 255)
                         (logand (lsh len -24) 255)
                         (logand (lsh len -16) 255)
                         (logand (lsh len -8)  255)
                         (logand      len      255))))
     string)))


;;; Content and Transfer encoding support
(defvar ws-compress-cmd "compress"
  "Command used for the \"compress\" Content or Transfer coding.")

(defvar ws-deflate-cmd "zlib-flate -compress"
  "Command used for the \"deflate\" Content or Transfer coding.")

(defvar ws-gzip-cmd "gzip"
  "Command used for the \"gzip\" Content or Transfer coding.")

(defmacro ws-encoding-cmd-to-fn (cmd)
  "Return a function which applies CMD to strings."
  `(lambda (s)
     (with-temp-buffer
       (insert s)
       (shell-command-on-region (point-min) (point-max) ,cmd nil 'replace)
       (buffer-string))))

(defun ws-chunk (string)
  "Convert STRING to a valid chunk for HTTP chunked Transfer-encoding."
  (format "%x\r\n%s\r\n" (string-bytes string) string))


;;; Convenience functions to write responses
(defun ws-response-header (proc code &rest headers)
  "Send the headers for an HTTP response to PROC.
CODE should be an HTTP status code, see `ws-status-codes' for a
list of known codes.

When \"Content-Encoding\" or \"Transfer-Encoding\" headers are
supplied any subsequent data written to PROC using `ws-send' will
be encoded appropriately including sending the appropriate data
upon the end of transmission for chunked transfer encoding.

For example with the header `(\"Content-Encoding\" . \"gzip\")',
any data subsequently written to PROC using `ws-send' will be
compressed using the command specified in `ws-gzip-cmd'."
  ;; update process to reflect any Content or Transfer encodings
  (let ((content  (cdr (assoc "Content-Encoding" headers)))
        (transfer (cdr (assoc "Transfer-Encoding" headers))))
    (when content
      (set-process-plist proc
        (append
         (list :content-encoding
               (cl-ecase (intern content)
                 ((compress x-compress) (ws-encoding-cmd-to-fn ws-compress-cmd))
                 ((deflate x-deflate)   (ws-encoding-cmd-to-fn ws-deflate-cmd))
                 ((gzip x-gzip)         (ws-encoding-cmd-to-fn ws-gzip-cmd))
                 (identity #'identity)
                 ((exi pack200-zip)
                  (ws-error proc "`%s' Content-encoding not supported."
                            content))))
         (process-plist proc))))
    (when transfer
      (set-process-plist proc
        (append
         (when (string= transfer "chunked") (list :ender "0\r\n\r\n"))
         (list :transfer-encoding
               (cl-ecase (intern transfer)
                 (chunked  #'ws-chunk)
                 ((compress x-compress) (ws-encoding-cmd-to-fn ws-compress-cmd))
                 ((deflate x-deflate)   (ws-encoding-cmd-to-fn ws-deflate-cmd))
                 ((gzip x-gzip)         (ws-encoding-cmd-to-fn ws-gzip-cmd))))
         (process-plist proc)))))
  (let ((headers
         (cons
          (format "HTTP/1.1 %d %s" code (cdr (assoc code ws-status-codes)))
          (mapcar (lambda (h) (format "%s: %s" (car h) (cdr h))) headers))))
    (setcdr (last headers) (list "" ""))
    (process-send-string proc (mapconcat #'identity headers "\r\n"))))

(defun ws-send (proc string)
  "Send STRING to process PROC.
If any Content or Transfer encodings are in use, apply them to
STRING before sending."
  (let
      ((cc (or (plist-get (process-plist proc) :content-encoding) #'identity))
       (tc (or (plist-get (process-plist proc) :transfer-encoding) #'identity)))
    (process-send-string proc (funcall tc (funcall cc string)))))

(defun ws-send-500 (proc &rest msg-and-args)
  "Send 500 \"Internal Server Error\" to PROC with an optional message."
  (ws-response-header proc 500
    '("Content-type" . "text/plain"))
  (process-send-string proc (if msg-and-args
                                (apply #'format msg-and-args)
                              "500 Internal Server Error"))
  (throw 'close-connection nil))

(defun ws-send-404 (proc &rest msg-and-args)
  "Send 404 \"Not Found\" to PROC with an optional message."
  (ws-response-header proc 404
    '("Content-type" . "text/plain"))
  (process-send-string proc (if msg-and-args
                                (apply #'format msg-and-args)
                              "404 Not Found"))
  (throw 'close-connection nil))

(defun ws-send-file (proc path &optional mime-type)
  "Send PATH to PROC.
Optionally explicitly set MIME-TYPE, otherwise it is guessed by
`mm-default-file-encoding'."
  (let ((mime (or mime-type
                  (mm-default-file-encoding path)
                  "application/octet-stream")))
    (process-send-string proc
      (with-temp-buffer
        (insert-file-contents-literally path)
        (ws-response-header proc 200
          (cons "Content-type" mime)
          (cons "Content-length" (- (point-max) (point-min))))
        (buffer-string)))))

(defun ws-send-directory-list (proc directory &optional match)
  "Send a listing of files in DIRECTORY to PROC.
Optional argument MATCH is passed to `directory-files' and may be
used to limit the files sent."
  (ws-response-header proc 200 (cons "Content-type" "text/html"))
  (process-send-string proc
    (concat "<ul>"
            (mapconcat (lambda (f)
                         (let* ((full (expand-file-name f directory))
                                (end (if (file-directory-p full) "/" ""))
                                (url (url-encode-url (concat f end))))
                           (format "<li><a href=%s>%s</li>" url f)))
                       (directory-files directory nil match)
                       "\n")
            "</ul>")))

(defun ws-in-directory-p (parent path)
  "Check if PATH is under the PARENT directory.
If so return PATH, if not return nil.  Note: the PARENT directory
must be full expanded as with `expand-file-name' and should not
contain e.g., \"~\" for a user home directory."
  (if (zerop (length path))
      parent
    (let ((expanded (expand-file-name path parent)))
      (and (>= (length expanded) (length parent))
           (string= parent (substring expanded 0 (length parent)))
           expanded))))

(defun ws-with-authentication (handler credentials
                                       &optional realm unauth invalid)
  "Return a version of HANDLER protected by CREDENTIALS.
HANDLER should be a function as passed to `ws-start', and
CREDENTIALS should be an alist of elements of the form (USERNAME
. PASSWORD).

Optional argument REALM sets the realm in the authentication
challenge.  Optional arguments UNAUTH and INVALID should be
functions which are called on the request when no authentication
information, or invalid authentication information are provided
respectively."
  (let ((handler handler)
        (credentials credentials)
        (realm realm)
        (unauth unauth)
        (invalid invalid))
    (lambda (request)
      (with-slots (process headers) request
        (let ((auth (cddr (assoc :AUTHORIZATION headers))))
          (cond
           ;; no authentication information provided
           ((not auth)
            (if unauth
                (funcall unauth request)
              (ws-response-header process 401
                (cons "WWW-Authenticate"
                      (format "Basic realm=%S" (or realm "restricted")))
                '("Content-type" . "text/plain"))
              (process-send-string process "authentication required")))
           ;; valid authentication information
           ((string= (cdr auth) (cdr (assoc (car auth) credentials)))
            (funcall handler request))
           ;; invalid authentication information
           (t
            (if invalid
                (funcall invalid request)
              (ws-response-header process 403 '("Content-type" . "text/plain"))
              (process-send-string process "invalid credentials")))))))))

(defun ws-web-socket-handshake (key)
  "Perform the handshake defined in RFC6455."
  (base64-encode-string (sha1 (concat (ws-trim key) ws-guid) nil nil 'binary)))

;;; Enable the old accessors without the `ws-' namespace as obsolete.
;;; Lets plan to remove these within a year of the date they were
;;; marked obsolete, so that would be roughly 2021-03-12.
(define-obsolete-function-alias 'active 'ws-active "2020-03-12")
(define-obsolete-function-alias 'body 'ws-body "2020-03-12")
(define-obsolete-function-alias 'boundary 'ws-boundary "2020-03-12")
(define-obsolete-function-alias 'context 'ws-context "2020-03-12")
(define-obsolete-function-alias 'data 'ws-data "2020-03-12")
(define-obsolete-function-alias 'handler 'ws-handler "2020-03-12")
(define-obsolete-function-alias 'handlers 'ws-handlers "2020-03-12")
(define-obsolete-function-alias 'headers 'ws-headers "2020-03-12")
(define-obsolete-function-alias 'index 'ws-index "2020-03-12")
(define-obsolete-function-alias 'new 'ws-new "2020-03-12")
(define-obsolete-function-alias 'pending 'ws-pending "2020-03-12")
(define-obsolete-function-alias 'port 'ws-port "2020-03-12")
(define-obsolete-function-alias 'process 'ws-process "2020-03-12")
(define-obsolete-function-alias 'requests 'ws-requests "2020-03-12")

(provide 'web-server)
;;; web-server.el ends here
