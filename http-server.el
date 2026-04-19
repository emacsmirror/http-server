;;; http-server.el --- Speaks HTTP for you -*- lexical-binding: t -*-

;; Author: Marten Lienen <ml@martenlienen.com>
;; URL: https://codeberg.org/martenlienen/http-server.el
;; Keywords: comm
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;;; License

;; This file is part of http-server.

;; http-server is free software: you can redistribute it and/or modify it under the terms
;; of the GNU General Public License as published by the Free Software Foundation, either
;; version 3 of the License, or (at your option) any later version.

;; http-server is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
;; PARTICULAR PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License along with
;; http-server.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; http-server speaks HTTP for you.  It is a building block for other packages that want
;; to offer an HTTP server inside of Emacs to communicate with the user or external
;; programs.  http-server's minimal design makes no assumptions on what that package will
;; do with the server.  It gets out of your way and grants you full control and
;; responsibility over any aspect of the server that is not directly tied to the HTTP
;; protocol itself.

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 'seq)
(require 'url-util)

(define-error 'http-server-error "http-server")
(define-error 'http-server-client-disconnected "Client disconnected" 'http-server-error)

;;; HTTP constants

(defconst http-server-methods '(GET HEAD POST PUT DELETE CONNECT OPTIONS TRACE PATCH)
  "Known HTTP methods.")

(defconst http-server-status-codes-and-phrases
  '((100 . "Continue")
    (101 . "Switching Protocols")
    (200 . "OK")
    (201 . "Created")
    (202 . "Accepted")
    (203 . "Non-Authoritative Information")
    (204 . "No Content")
    (205 . "Reset Content")
    (206 . "Partial Content")
    (300 . "Multiple Choices")
    (301 . "Moved Permanently")
    (302 . "Found")
    (303 . "See Other")
    (304 . "Not Modified")
    (305 . "Use Proxy")
    (307 . "Temporary Redirect")
    (308 . "Permanent Redirect")
    (400 . "Bad Request")
    (401 . "Unauthorized")
    (402 . "Payment Required")
    (403 . "Forbidden")
    (404 . "Not Found")
    (405 . "Method Not Allowed")
    (406 . "Not Acceptable")
    (407 . "Proxy Authentication Required")
    (408 . "Request Timeout")
    (409 . "Conflict")
    (410 . "Gone")
    (411 . "Length Required")
    (412 . "Precondition Failed")
    (413 . "Content Too Large")
    (414 . "URI Too Long")
    (415 . "Unsupported Media Type")
    (416 . "Range Not Satisfiable")
    (417 . "Expectation Failed")
    (421 . "Misdirected Request")
    (422 . "Unprocessable Content")
    (426 . "Upgrade Required")
    (500 . "Internal Server Error")
    (501 . "Not Implemented")
    (502 . "Bad Gateway")
    (503 . "Service Unavailable")
    (504 . "Gateway Timeout")
    (505 . "HTTP Version Not Supported"))
  "Known HTTP status codes and reason phrases.")

(defconst http-server-headers
  '( A-IM Accept Accept-Additions Accept-CH Accept-Charset Accept-Datetime Accept-Encoding
     Accept-Features Accept-Language Accept-Patch Accept-Post Accept-Query Accept-Ranges
     Accept-Signature Access-Control Access-Control-Allow-Credentials
     Access-Control-Allow-Headers Access-Control-Allow-Methods Access-Control-Allow-Origin
     Access-Control-Expose-Headers Access-Control-Max-Age Access-Control-Request-Headers
     Access-Control-Request-Method Activate-Storage-Access Age Allow ALPN Alt-Svc Alt-Used
     Alternates AMP-Cache-Transform Apply-To-Redirect-Ref Authentication-Control
     Authentication-Info Authorization Available-Dictionary C-Ext C-Man C-Opt C-PEP
     C-PEP-Info Cache-Control Cache-Group-Invalidation Cache-Groups Cache-Status
     Cal-Managed-ID CalDAV-Timezones Capsule-Protocol CDN-Cache-Control CDN-Loop
     Cert-Not-After Cert-Not-Before Clear-Site-Data Client-Cert Client-Cert-Chain Close
     CMCD-Object CMCD-Request CMCD-Session CMCD-Status CMSD-Dynamic CMSD-Static
     Concealed-Auth-Export Configuration-Context Connection Content-Base Content-Digest
     Content-Disposition Content-Encoding Content-ID Content-Language Content-Length
     Content-Location Content-MD5 Content-Range Content-Script-Type
     Content-Security-Policy Content-Security-Policy-Report-Only Content-Style-Type
     Content-Type Content-Version Cookie Cookie2 Cross-Origin-Embedder-Policy
     Cross-Origin-Embedder-Policy-Report-Only Cross-Origin-Opener-Policy
     Cross-Origin-Opener-Policy-Report-Only Cross-Origin-Resource-Policy
     CTA-Common-Access-Token DASL Date DAV Default-Style Delta-Base Deprecation Depth
     Derived-From Destination Detached-JWS Differential-ID Dictionary-ID Digest DPoP
     DPoP-Nonce Early-Data EDIINT-Features ETag Expect Expect-CT Expires Ext Forwarded
     From GetProfile Hobareg Host HTTP2-Settings If If-Match If-Modified-Since
     If-None-Match If-Range If-Schedule-Tag-Match If-Unmodified-Since IM
     Include-Referred-Token-Binding-ID Incremental Isolation Keep-Alive Label
     Last-Event-ID Last-Modified Link Link-Template Location Lock-Token Man Max-Forwards
     Memento-Datetime Meter Method-Check Method-Check-Expires MIME-Version Negotiate NEL
     OData-EntityId OData-Isolation OData-MaxVersion OData-Version Opt
     Optional-WWW-Authenticate Ordering-Type Origin Origin-Agent-Cluster OSCORE
     OSLC-Core-Version Overwrite P3P PEP PEP-Info Permissions-Policy PICS-Label Ping-From
     Ping-To Position Pragma Prefer Preference-Applied Priority ProfileObject Protocol
     Protocol-Info Protocol-Query Protocol-Request Proxy-Authenticate
     Proxy-Authentication-Info Proxy-Authorization Proxy-Features Proxy-Instruction
     Proxy-Status Public Public-Key-Pins Public-Key-Pins-Report-Only Range Redirect-Ref
     Referer Referer-Root Referrer-Policy Refresh Repeatability-Client-ID
     Repeatability-First-Sent Repeatability-Request-ID Repeatability-Result Replay-Nonce
     Reporting-Endpoints Repr-Digest Retry-After Safe Schedule-Reply Schedule-Tag
     Sec-Fetch-Dest Sec-Fetch-Mode Sec-Fetch-Site Sec-Fetch-Storage-Access Sec-Fetch-User
     Sec-GPC Sec-Purpose Sec-Token-Binding Sec-WebSocket-Accept Sec-WebSocket-Extensions
     Sec-WebSocket-Key Sec-WebSocket-Protocol Sec-WebSocket-Version Security-Scheme Server
     Server-Timing Set-Cookie Set-Cookie2 Set-Txn SetProfile Signature Signature-Input
     SLUG SoapAction Status-URI Strict-Transport-Security Sunset Surrogate-Capability
     Surrogate-Control TCN TE Timeout Timing-Allow-Origin Topic Traceparent Tracestate
     Trailer Transfer-Encoding TTL Upgrade Urgency URI Use-As-Dictionary User-Agent
     Variant-Vary Vary Via Want-Content-Digest Want-Digest Want-Repr-Digest Warning
     WWW-Authenticate X-Content-Type-Options X-Frame-Options)
  "Known HTTP header field names in canonical capitalization.

See URL `https://www.iana.org/assignments/http-fields/http-fields.xhtml'
for the official registry.")

;;; Logging

(eval-and-compile
  ;; Has to be available at compile time for http-server--log
  (defconst http-server-log-levels '(trace debug info warning error)
    "Possible log levels from most to least chatty."))

(defvar http-server-log-level 'info
  "Fallback log level if not set explicitly on a server.")

(defmacro http-server--log (proc &rest clauses)
  "Log to the log-buffer of PROC.

CLAUSES are evaluated in turn, so one call can log at multiple log
levels.

Each clause is (LEVEL BODY...).  If the log-level of PROC is below LEVEL,
evaluate BODY for a message.  If the message is nil, nothing is logged."
  (declare (indent 1) (debug (form &rest (symbolp body))))
  (let ((log-buffer (gensym "log-buffer"))
        (point-at-max (gensym "point-at-max"))
        (proc-level (gensym "proc-level"))
        (proc-level-pos (gensym "proc-level-pos"))
        (host (gensym "host"))
        (port (gensym "port"))
        (message (gensym "message")))
    `(when-let* ((,log-buffer (process-get ,proc :log-buffer))
                 ((buffer-live-p ,log-buffer)))
       (with-current-buffer ,log-buffer
         (let* ((,point-at-max (equal (point) (point-max)))
                (,proc-level (or (process-get ,proc :log-level) http-server-log-level))
                (,proc-level-pos (seq-position http-server-log-levels ,proc-level))
                (,host (process-contact ,proc :host))
                (,port (process-contact ,proc :service)))
           (save-excursion
             (goto-char (point-max))
             ,@(mapcar
                (lambda (clause)
                  (let* ((level (car clause))
                         (level-name (format "%-5s" (upcase (symbol-name level))))
                         (body (cdr clause)))
                    `(when-let* (((<= ,proc-level-pos ,(seq-position http-server-log-levels level)))
                                 (,message (progn ,@body)))
                       (let ((prefix (format "%s %s %s:%d: "
                                             (format-time-string "%Y/%m/%d %T.%3N")
                                             ,level-name ,host ,port)))
                         (insert (http-server--prefix-lines prefix ,message) "\n")))))
                clauses))
           (when ,point-at-max
             (goto-char (point-max))))))))

(defvar http-server--log-escape-placeholder "×"
  "Replacement for non-ASCII characters in logs.")

(defconst http-server--log-escape-regexp
  (rx (not (any ?\n ?\r ?\t (?\x20 . ?\x7E))))
  "Matches any character that should not be printed in the log.")

(defun http-server--log-escape (message)
  "Escape MESSAGE for logging."
  (replace-regexp-in-string http-server--log-escape-regexp
                            http-server--log-escape-placeholder
                            message t t))

(cl-defun http-server--log-fill (message &key (width 60))
  "Fill MESSAGE as a text block of width WIDTH.

Useful for logging of long messages and binary blobs."
  (replace-regexp-in-string (rx-to-string `(repeat ,width not-newline))
                            "\\&\n" message t))

(defun http-server--prefix-lines (prefix str)
  "Prepend PREFIX to the lines of STR."
  (concat prefix (replace-regexp-in-string "\n" (concat "\n" prefix) str)))

;;; Server life cycle

(cl-defun http-server-start
    (&key (name "http-server")
          (host "0.0.0.0")
          (port t)
          on-request
          on-upgrade
          (log-buffer nil log-buffer-given)
          log-level
          (kill-log-buffer t)
          (kill-connection-buffers t)
          (default-status 'Not-Found)
          extra-methods
          extra-headers
          (unknown-headers #'ignore))
  "Start and return an HTTP server on HOST and PORT.

NAME determines the process name for the server and client connections.
HOST and PORT specify the network address to bind to.  The default of t
for PORT chooses any free port.  `http-server-url' returns the URL of the
server with the chosen port.

ON-REQUEST is a synchronous or asynchronous request handler.  If
ON-REQUEST takes a single argument, it is called as (funcall ON-REQUEST
REQUEST) and must return a RESPONSE synchronously.  If ON-REQUEST takes
two arguments, it is called as (funcall ON-REQUEST REQUEST
SEND-RESPONSE) and must (funcall SEND-RESPONSE RESPONSE) exactly once to
send RESPONSE asynchronously.

In both cases, REQUEST is a plist (:method METHOD :path PATH :query
QUERY :headers HEADERS :body BODY :connection CONNECTION).  METHOD is
the request method as a symbol such as \\='GET or \\='POST.  PATH and
QUERY are the path and query components of the request target as decoded
multibyte strings.  HEADERS is an alist of (NAME . VALUE) header fields
where NAME is a symbol such as \\='Content-Type and VALUE is a string.
BODY is a unibyte string without any decoding applied.  CONNECTION is
the network connection to the client, see info node `(elisp)Network'.

Similarly in both cases, RESPONSE is a plist (:status STATUS :headers
HEADERS :body BODY) where all entries are optional.  STATUS is the
response status either as an integer code like 404, a symbol such as
\\='OK or \\='Not-Found or a full status line string like \"200 OK\" for
non-standard status codes.  If STATUS is not given, respond with
DEFAULT-STATUS.  HEADERS is again an alist of (NAME . VALUE) header
fields where NAME is a symbol and VALUE is a string.  BODY is either a
unibyte string that will be sent as-is or a function that will be called
as (funcall BODY SEND-CHUNK). SEND-CHUNK lets BODY send the message body
asynchronously in multiple chunks. Call as (funcall SEND-CHUNK CHUNK
:keep-open KEEP-OPEN) where CHUNK is a unibyte string to send and
KEEP-OPEN, if t, will keep the connection open for more chunks.
Otherwise, the response will be finalized and the connection closed.

Both SEND-RESPONSE and SEND-CHUNK signal
`http-server-client-disconnected' if the client has closed the
connection.  Callers holding long-lived references to SEND-RESPONSE or
SEND-CHUNK should always be prepared to handle this case.

Whenever a request contains an Upgrade header, ON-UPGRADE is called
as (funcall ON-UPGRADE CONNECTION REQUEST SEND-RESPONSE) if present.
REQUEST is the request plist and SEND-RESPONSE is a function that sends
a response to CONNECTION. If the response has status
101 (Switching-Protocols), the connection stays open after sending the
response and ON-UPGRADE is expected to assume control of the connection.
CONNECTION is the network connection to the client, see info
node `(elisp)Network'.  Use `http-server-ws-on-upgrade' from
http-server-ws.el to create a WebSocket upgrade handler.

If no LOG-BUFFER is given, a buffer named *NAME* is created
automatically for logging. Set LOG-BUFFER to nil explicitly to disable
logging. LOG-LEVEL sets the server log level and must be one of
\\='trace, \\='debug, \\='info, \\='warning, \\='error or nil. If it is
nil, logging happens according to `http-server-log-level'.

If KILL-LOG-BUFFER is t, kill the log buffer when stopping the server.
If KILL-CONNECTION-BUFFERS is t, kill connection buffers when
connections are closed.

By default, the server accepts only the standard methods and headers
listed in `http-server-methods' and `http-server-headers'. To accept
others, pass list of method and header symbols as EXTRA-METHODS and
EXTRA-HEADERS, respectively.

UNKNOWN-HEADERS is called as (funcall UNKNOWN-HEADERS NAME) for each
header not in `http-server-headers' or EXTRA-HEADERS, where NAME is the
raw header name string.  It must return nil to drop the header or a
symbol to use as the header name.  The default is #\\='ignore, which
drops all unknown headers.  Use #\\='intern to accept all headers using
their original name as a symbol or #\\='identity to keep them as
strings."
  (let* ((known-methods
          (let ((h (make-hash-table :test 'equal
                                    :size (+ (length http-server-methods)
                                             (length extra-methods)))))
            (dolist (name http-server-methods)
              (puthash (symbol-name name) name h))
            (dolist (name extra-methods h)
              (puthash (symbol-name name) name h))))
         (known-headers
          (let ((h (make-hash-table :test 'equal
                                    :size (+ (length http-server-headers)
                                             (length extra-headers)))))
            (dolist (name http-server-headers)
              (puthash (downcase (symbol-name name)) name h))
            (dolist (name extra-headers h)
              (puthash (downcase (symbol-name name)) name h))))
         (known-statuses
          (let ((h (make-hash-table :test 'eq :size (* 2 (length http-server-status-codes-and-phrases)))))
            (dolist (info http-server-status-codes-and-phrases h)
              (let* ((code (car info))
                     (phrase (cdr info))
                     (sym (intern (string-replace " " "-" phrase)))
                     (status-line (format "%s %s" code phrase)))
                (puthash sym status-line h)
                (puthash code status-line h)))))
         (server (make-network-process
                  :name name
                  :buffer nil
                  :server t
                  :host host
                  :service port
                  :coding 'binary
                  :filter #'http-server--request-filter
                  :log #'http-server--accept-connection
                  :plist `( :known-methods ,known-methods
                            :known-headers ,known-headers
                            :unknown-headers ,unknown-headers
                            :known-statuses ,known-statuses
                            :on-request ,on-request
                            :on-upgrade ,on-upgrade
                            :log-level ,log-level
                            :default-status ,default-status
                            :kill-log-buffer ,kill-log-buffer
                            :kill-connection-buffers ,kill-connection-buffers))))
    (when (and (not log-buffer) (not log-buffer-given))
      ;; Generate a log-buffer if it was not explicitly set to nil
      (setq log-buffer (generate-new-buffer (format "*%s*" name))))
    (when log-buffer
      ;; Set as process buffer, so that killing the buffer stops the server
      (set-process-buffer server log-buffer)
      ;; Store in :log-buffer property for logging
      (process-put server :log-buffer log-buffer)
      (http-server--log server (info "Server started")))
    server))

(cl-defun http-server-stop (server)
  "Stop SERVER: Close the network socket and delete buffers."
  (delete-process server)
  (when-let* (((process-get server :kill-log-buffer))
              (log-buffer (process-get server :log-buffer))
              (buffer-live-p log-buffer))
    (kill-buffer log-buffer)))

;;; Connection management

(defun http-server--accept-connection (server proc message)
  "Set up a new connection PROC to SERVER.

MESSAGE contains additional information."
  (http-server--log server
    (info (format "New connection: %s - %s" (process-contact proc) message)))
  (dolist (keyword '(:known-methods
                     :known-headers
                     :unknown-headers
                     :known-statuses
                     :on-request
                     :on-upgrade
                     :log-buffer
                     :log-level
                     :default-status
                     :kill-connection-buffers))
    (process-put proc keyword (process-get server keyword)))
  (set-process-sentinel proc #'http-server--request-sentinel)
  (set-process-buffer proc
                      (generate-new-buffer
                       (format " *%s-connection* <%s:%s>"
                               (process-name server)
                               (process-contact proc :host)
                               (process-contact proc :service)))))

(defun http-server--close-connection (proc)
  "Close connection PROC: delete the process and its buffers."
  (when (process-live-p proc)
    (delete-process proc)
    (http-server--log proc (trace "Connection closed")))
  (when-let* (((process-get proc :kill-connection-buffers))
              (buffer (process-buffer proc))
              (buffer-live-p buffer))
    (kill-buffer buffer)))

(defun http-server--request-filter (proc chunk)
  "Handle client connection PROC receiving CHUNK."
  (handler-bind
      ;; Last resort response in case an error interrupts request handling
      ((error
        (lambda (err)
          (http-server--log proc
            (error (format "Error during request handling:\n%s"
                           (http-server--prefix-lines
                            "| "
                            (http-server--log-fill (error-message-string err))))))
          (ignore-errors
            (http-server--send-response proc '(:status Internal-Server-Error))))))

    (http-server--log proc
      (debug (format "Chunk received: %d bytes" (length chunk))))
    (when-let* ((buffer (process-buffer proc))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (goto-char (process-mark proc))
        (insert chunk)
        (set-marker (process-mark proc) (point))))
    (pcase (with-current-buffer (process-buffer proc)
             (http-server--parse-http-request))
      ('incomplete
       (http-server--log proc (trace "Request incomplete")))

      ;; Parsing failed
      (`(,(and status (pred (not keywordp))) . ,context)
       (http-server--log proc
         (debug (format "Request parsing failed: %s" context))
         (trace (with-current-buffer (process-buffer proc)
                  (http-server--log-fill (http-server--log-escape (buffer-string))))))
       (http-server--send-response proc `(:status ,status)))

      ;; Parsing succeeded
      (raw-request
       (http-server--log proc
         (info
          (format "> %s %s"
                  (plist-get raw-request :method)
                  (http-server--log-escape (plist-get raw-request :target))))
         (debug
          (http-server--prefix-lines
           "> "
           (mapconcat (lambda (header) (format "%s: %s" (car header) (cdr header)))
                      (plist-get raw-request :headers) "\n")))
         (trace
          (if-let* ((body (plist-get raw-request :body)))
              (http-server--prefix-lines
               "> "
               (concat "\n"
                       (http-server--log-fill (http-server--log-escape body))))
            "No message body")))
       ;; Delete request bytes from buffer to prepare parsing pipelined requests or
       ;; upgraded connection data
       (with-current-buffer (process-buffer proc)
         (delete-region (point-min) (point))
         (set-marker (process-mark proc) (point-max)))
       (pcase (http-server--structure-request proc raw-request)
         ;; Structuring failed
         (`(,(and status (pred (not keywordp))) . ,context)
          (http-server--log proc
            (debug (format "Could not decode request: %s" context)))
          ;; Reject request targets other than origin-form
          (http-server--send-response proc `(:status ,status)))

         ;; Structuring succeeded
         (request
          ;; Any request could be an Upgrade request, so check that first
          (if-let* ((on-upgrade (process-get proc :on-upgrade))
                    ((alist-get 'Upgrade (plist-get request :headers))))
              (funcall on-upgrade proc request
                       (lambda (response)
                         (http-server--send-response-async
                          proc response
                          :keep-open (memq (plist-get response :status)
                                           '(101 Switching-Protocols)))))
            (if (memq (plist-get request :method) '(TRACE OPTIONS CONNECT))
                ;; Reject methods for proxies and exotic use-cases
                (http-server--send-response proc '(:status Not-Implemented))
              (if-let* ((on-request (process-get proc :on-request)))
                  (pcase (func-arity on-request)
                    (`(1 . 1)
                     (http-server--send-response proc (funcall on-request request)))
                    (`(2 . 2)
                     (funcall on-request
                              request
                              (apply-partially #'http-server--send-response-async proc)))
                    (arity
                     (signal 'http-server-error
                             `("ON-REQUEST must accept exactly 1 or exactly 2 arguments, "
                               "but accepts " ,arity))))
                (http-server--send-response proc '(:status Not-Found)))))))))))

(defun http-server--request-sentinel (proc event)
  "Handle PROC receiving an EVENT."
  (unless (string-prefix-p "open" event)
    ;; Either connection closed or timeout
    (http-server--log proc (trace (format "Process event: %s" (string-trim event))))
    (http-server--close-connection proc)))

;;; HTTP parsing

(defconst http-server--request-line-regexp
  (rx-let ((token (one-or-more (or ?! ?# ?$ ?% ?& ?' ?*
                                   ?+ ?- ?. ?^ ?_ ?` ?| ?~
                                   digit (any "a-z" "A-Z")))))
    (rx (group-n 1 token)
        ?\s
        (group-n 2 (one-or-more (not ?\s)))
        ?\s
        "HTTP/1.1" ?\r?\n))
  "Regexp matching valid request lines.")

(defconst http-server--header-line-regexp
  (rx-let ((field-vchar (any (?\x21 . ?\x7E)))
           (field-content (seq field-vchar
                               (opt (+ (or (any ?\s ?\t) field-vchar))
                                    field-vchar)))
           (token (one-or-more (or ?! ?# ?$ ?% ?& ?' ?*
                                   ?+ ?- ?. ?^ ?_ ?` ?| ?~
                                   digit (any "a-z" "A-Z")))))
    (rx (group-n 1 token)
        ":" (* ?\s)
        (group-n 2 (* field-content))
        (* ?\s) ?\r?\n))
  "Regexp matching valid header lines.")

(defconst http-server--chunk-regexp
  (rx (group-n 1 (one-or-more hex-digit))
      ;; Chunk extensions, which we ignore
      (zero-or-more (not (or ?\r ?\n)))
      ?\r?\n)
  "Regexp matching a Transfer-Encoding chunk.")

(defun http-server--parse-http-request ()
  "Parse the current buffer as an HTTP request.

Returns:
  \\='incomplete        -- more data needed
  (STATUS . CONTEXT) -- parse error; STATUS is a symbol denoting an HTTP
                        status code, CONTEXT a string
  plist              -- (:method METHOD :target TARGET :headers HEADERS
                        :body BODY); point is after the last consumed byte

METHOD is the request method as a string.  TARGET is the request target
as a unibyte string.  HEADERS is an alist of (NAME . VALUE) headers,
where both NAME and VALUE consist of visible ASCII characters.  BODY is
an optional unibyte string, only present if the request has a message
body."
  (goto-char (point-min))

  ;; Parse request line
  (catch 'result
    (let ((buffer-end (point-max))
          method target headers)
      ;; Parse request line
      (when (not (looking-at http-server--request-line-regexp))
        ;; We did not find a request line, so if we have already received the end of the
        ;; header section, the request is invalid, otherwise incomplete
        (throw 'result (if (search-forward "\r\n\r\n" nil t)
                           '(Bad-Request . "Invalid request line")
                         'incomplete)))
      (setq method (match-string 1)
            target (match-string 2))
      (goto-char (match-end 0))

      ;; Parse headers
      (while (looking-at http-server--header-line-regexp)
        (push `(,(match-string 1) . ,(match-string 2)) headers)
        (goto-char (match-end 0)))
      (when (not (looking-at "\r\n"))
        ;; Whatever we are looking at is neither a valid header line nor the end of the
        ;; header section
        (throw 'result (if (search-forward "\r\n\r\n" nil t)
                           '(Bad-Request . "Invalid header")
                         'incomplete)))
      (setq headers (nreverse headers))

      ;; Parse request body
      (goto-char (match-end 0))
      (let ((content-length (cdr (assoc "Content-Length" headers
                                        #'string-equal-ignore-case)))
            (transfer-encoding (cdr (assoc "Transfer-Encoding" headers
                                           #'string-equal-ignore-case))))
        (cond
         ;; These are incompatible
         ((and content-length transfer-encoding) '(Bad-Request . "Content-Length and Transfer-Encoding are incompatible"))
         ;; No body announced, so we are done
         ((not (or content-length transfer-encoding))
          `( :method ,method
             :target ,target
             :headers ,headers))
         ;; Fixed-length body
         (content-length
          (if (not (string-match-p "\\`[[:digit:]]+\\'" content-length))
              '(Bad-Request . "Invalid Content-Length")
            (let ((wanted (string-to-number content-length))
                  (remaining (- buffer-end (point))))
              (if (< remaining wanted)
                  'incomplete
                (let ((body-end (+ (point) wanted)))
                  (prog1 `( :method ,method
                            :target ,target
                            :headers ,headers
                            :body ,(buffer-substring (point) body-end))
                    (goto-char body-end)))))))
         ;; Transfer-Encoding
         (t
          (when (not (equal transfer-encoding "chunked"))
            (throw 'result '(Not-Implemented . "Unsupported Transfer-Encoding")))
          (let (body)
            (while (looking-at http-server--chunk-regexp)
              (goto-char (match-end 0))
              (let* ((chunk-len-match (match-string 1))
                     (chunk-len (string-to-number chunk-len-match 16))
                     (chunk-end (+ (point) chunk-len)))
                (when (zerop chunk-len)
                  ;; Chunked trailer sections seem like an exotic HTTP feature
                  (when (not (looking-at "\r\n"))
                    (throw 'result '(Not-Implemented . "Chunked trailer sections not supported")))
                  (goto-char (match-end 0))
                  (throw 'result
                         `( :method ,method
                            :target ,target
                            :headers ,(assoc-delete-all "Transfer-Encoding" headers
                                                        #'string-equal-ignore-case)
                            :body ,(apply #'concat (reverse body)))))
                (when (> chunk-end buffer-end)
                  (throw 'result 'incomplete))
                (push (buffer-substring (point) chunk-end) body)
                (goto-char chunk-end)
                (when (not (looking-at "\r\n"))
                  (throw 'result '(Bad-Request . "Chunk data exceeds declared length")))
                (goto-char (match-end 0)))))))))))

(defconst http-server--origin-form-regexp
  (rx-let ((unreserved (any (?a . ?z) (?A . ?Z) digit ?- ?. ?_ ?~))
           (pct-encoded (seq ?% hex-digit hex-digit))
           (sub-delims (any ?! ?$ ?& ?' ?\( ?\) ?* ?+ ?, ?\; ?=))
           (pchar (or unreserved pct-encoded sub-delims ?: ?@))
           (segment (zero-or-more pchar))
           (absolute-path (one-or-more ?/ segment))
           (query (zero-or-more (or pchar ?/ ??))))
    (rx string-start (group-n 1 absolute-path) (optional ?? (group-n 2 query)) string-end))
  "Regexp matching HTTP request targets in origin-form.")

(defun http-server--structure-request (proc request)
  "Convert a parsed REQUEST into the structure for request handlers.

Verify methods and headers against SERVER.

Add the connection PROC to the request as an escape hatch."
  (catch 'result
    (let* ((method (or (gethash (plist-get request :method) (process-get proc :known-methods))
                       (throw 'result '(Bad-Request . "Unknown method"))))
           (decoded-target (or (http-server--decode-origin-form-target (plist-get request :target))
                               (throw 'result '(Bad-Request . "Request target not in origin-form"))))
           (known-headers (process-get proc :known-headers))
           (unknown-headers (process-get proc :unknown-headers))
           (headers
            (let (headers)
              (dolist (header (plist-get request :headers) (nreverse headers))
                (if-let* ((symbol (gethash (downcase (car header)) known-headers)))
                    (push (cons symbol (cdr header)) headers)
                  (when-let* ((symbol (funcall unknown-headers (car header))))
                    (push (cons symbol (cdr header)) headers)))))))
      `( :method ,method
         :path ,(car decoded-target)
         :query ,(cdr decoded-target)
         :headers ,headers
         :body ,(plist-get request :body)
         :connection ,proc))))

(defun http-server--decode-origin-form-target (target)
  "Decode an HTTP request TARGET in origin-form.

This is the usual \"/path?query\" form for any non-proxy HTTP server as
described in Section 3.2.1 of RFC 9112.

Return (PATH . QUERY) or nil if parsing fails. PATH is a string and
QUERY is an optional string. Both PATH and QUERY are decoded from UTF-8
%-encoding to multibyte strings."
  ;; If I understand this spec [1] and the URI RFC [2] correctly, we should assume that
  ;; the request target is UTF-8 encoded.
  ;;
  ;; [1] https://url.spec.whatwg.org
  ;; [2] https://www.rfc-editor.org/rfc/rfc3986.html
  (when (string-match http-server--origin-form-regexp target)
    (let ((path-match (match-string 1 target))
          (query-match (match-string 2 target)))
      (cons (decode-coding-string (url-unhex-string path-match) 'utf-8 t)
            (decode-coding-string (url-unhex-string query-match) 'utf-8 t)))))

;;; HTTP responses

(defun http-server--process-send-string (proc string)
  "Send STRING to network process PROC.

Signal `http-server-client-disconnected' if sending fails because the
connection has been closed."
  (condition-case err
      (process-send-string proc string)
    (error
     (if (and (eq (process-type proc) 'network)
              (eq (process-status proc) 'closed))
         (signal 'http-server-client-disconnected
                 (list (format "Client %s:%d disconnected"
                               (process-contact proc :host)
                               (process-contact proc :service))
                       err))
       (signal (car err) (cdr err))))))

(cl-defun http-server--send-response-async (proc response &key keep-open)
  "Send an HTTP RESPONSE to PROC from an asynchronous request handler.

RESPONSE is a plist (:status STATUS :headers HEADERS :body BODY) where
STATUS is either a full status line like \"200 OK\", a symbol denoting
an HTTP status or a numerical HTTP status code, HEADERS is an ALIST
of (NAME . VALUE) pairs with symbol NAME and string VALUE and BODY is an
optional unibyte string.

If KEEP-OPEN is non-nil, the connection is kept open after the response
is sent."
  (handler-bind
      ;; Because the response is asynchronous, we are potentially outside of the error
      ;; handler in `http-server--request-filter', so establish a new one.
      ((error
        (lambda (err)
          (http-server--log proc
            (error (format "Error during request handling:\n%s"
                           (http-server--prefix-lines
                            "| "
                            (http-server--log-fill (error-message-string err))))))
          (ignore-errors
            (http-server--send-response proc '(:status Internal-Server-Error))))))
    (when (process-get proc :response-sent)
      (signal 'http-server-error '("Cannot send-response twice.")))
    (process-put proc :response-sent t)
    (http-server--send-response proc response :keep-open keep-open)))

(cl-defun http-server--send-response (proc response &key keep-open)
  "Send an HTTP RESPONSE to PROC.

RESPONSE is a plist (:status STATUS :headers HEADERS :body BODY) where
STATUS is either a full status line like \"200 OK\", a symbol denoting
an HTTP status or a numerical HTTP status code, HEADERS is an ALIST
of (NAME . VALUE) pairs with symbol NAME and string VALUE and BODY is an
optional unibyte string.

If KEEP-OPEN is non-nil, the connection is kept open after the response
is sent."
  (let* ((status (or (plist-get response :status) (process-get proc :default-status)))
         (headers (plist-get response :headers))
         (body (plist-get response :body)))

    ;; Validate body and add implied headers
    (cond
     ((stringp body)
      (when (multibyte-string-p body)
        (signal 'http-server-error '("Cannot send multibyte body. Encode first.")))
      (setq headers (cons `(Content-Length . ,(number-to-string (length body))) headers)))
     ((functionp body)
      (setq headers (cons `(Transfer-Encoding . "chunked") headers))))

    ;; Resolve status to a status line string
    (let ((status-line (if (stringp status)
                           status
                         (gethash status (process-get proc :known-statuses)))))
      (unless status-line
        (signal 'http-server-error (list (format "Unknown status %s" status))))

      ;; RFC9110: An origin server with a clock MUST generate a Date header field.
      (unless (assoc 'Date headers)
        (let ((system-time-locale "C"))
          (setq headers (cons (cons 'Date (format-time-string "%a, %d %b %Y %T GMT" nil t))
                              headers))))

      ;; Build and send the response header
      (let* ((header-lines (apply #'append
                                  (mapcar (lambda (h)
                                            (list (symbol-name (car h))
                                                  ": "
                                                  (cdr h)
                                                  "\r\n"))
                                          headers)))
             (response-header (apply #'concat "HTTP/1.1 " status-line "\r\n"
                                     (append header-lines '("\r\n")))))
        (http-server--process-send-string proc response-header)
        (http-server--log proc
          (info (format "< %s" status-line))
          (debug
           (http-server--prefix-lines
            "< "
            (mapconcat (lambda (h) (http-server--log-escape
                                    (format "%s: %s" (car h) (cdr h))))
                       headers "\n")))
          (trace
           (cond
            ((stringp body)
             (http-server--prefix-lines
              "< "
              (concat "\n" (http-server--log-fill (http-server--log-escape body)))))
            ((functionp body) "Asynchronous message body")
            (t "No message body")))))

      (cond
       ((stringp body)
        (http-server--process-send-string proc body)
        (unless keep-open (http-server--close-connection proc)))
       ((functionp body)
        (funcall body (apply-partially #'http-server--send-chunk proc)))
       (t
        (unless keep-open (http-server--close-connection proc)))))))

(cl-defun http-server--send-chunk (proc chunk &key keep-open)
  "Send CHUNK to connection PROC with chunked transfer encoding.

If KEEP-OPEN is non-nil, keep the connection open for further chunks.
Otherwise, close the connection.

Sending an empty string closes the connection."
  (unless (process-live-p proc)
    (let ((message "Cannot send on a closed connection. Pass :keep-open t for the previous chunk to keep the connection open."))
      (http-server--log proc
        (error message))
      (signal 'http-server-client-disconnected (list message))))
  (if (length= chunk 0)
      (progn
        ;; An empty chunk is equal to the last-chunk marker, so close the connection
        (http-server--process-send-string proc "0\r\n\r\n")
        (http-server--close-connection proc))
    (when (multibyte-string-p chunk)
      (http-server--log proc
        (error "Tried to send multibyte chunk"))
      (signal 'http-server-error '("Cannot send multibyte chunk. Encode first.")))
    (http-server--log proc
      (trace
       (http-server--prefix-lines
        "< "
        (http-server--log-fill (http-server--log-escape chunk)))))
    (let ((message (format "%x\r\n%s\r\n%s"
                           (length chunk)
                           chunk
                           ;; Send the last-chunk marker and final CRLF
                           (if keep-open "" "0\r\n\r\n"))))
      (http-server--process-send-string proc message)
      (unless keep-open
        (http-server--close-connection proc)))))

;;; Utilities

(cl-defun http-server-url (server &optional (path "/") &key (protocol "http"))
  "Generate a URL for PATH on SERVER using PROTOCOL."
  (let ((host (process-contact server :host))
        (port (process-contact server :service)))
    (format "%s://%s:%s%s" protocol host port path)))

(defun http-server-log-buffer (server)
  "Return the log buffer of SERVER."
  (process-get server :log-buffer))

(provide 'http-server)
;;; http-server.el ends here
