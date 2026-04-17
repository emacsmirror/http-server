;;; http-server-ws.el --- WebSocket support for http-server.el -*- lexical-binding: t -*-

;; Author: Marten Lienen <ml@martenlienen.com>
;; URL: https://codeberg.org/martenlienen/http-server.el

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

;; WebSocket (RFC 6455) support for http-server.el.
;;
;; This module implements the WebSocket protocol upgrade and data framing on top of
;; http-server.el's TCP connection infrastructure.  After a successful HTTP upgrade
;; handshake, the connection's process filter is switched to `http-server-ws--filter',
;; which parses WebSocket frames, handles control frames (ping/pong/close), and
;; reassembles fragmented messages.
;;
;; Public API:
;;   `http-server-ws-send'   -- Send a text or binary message to a WebSocket client
;;   `http-server-ws-ping'   -- Send a ping frame
;;   `http-server-ws-close'  -- Initiate a clean close handshake
;;
;; See RFC 6455 for the complete WebSocket specification.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'http-server)

(define-error 'http-server-ws-error "http-server-ws" 'http-server-error)

(defconst http-server-ws--guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "WebSocket GUID used in the opening handshake per RFC 6455 §1.3.")

;;; §4: Opening Handshake

(defun http-server-ws--accept-key (client-key)
  "Compute Sec-WebSocket-Accept from CLIENT-KEY per RFC 6455 §4.2.2."
  (let ((hash (sha1 (concat client-key http-server-ws--guid) nil nil t)))
    (base64-encode-string hash t)))

(defun http-server-ws-on-upgrade (on-websocket)
  "Return an upgrade handler for use as :on-upgrade in `http-server-start'.

The returned handler is a function of three arguments (PROC REQUEST
SEND-RESPONSE) as required by the :on-upgrade calling convention.  It
validates the WebSocket handshake and, if valid, calls ON-WEBSOCKET.

ON-WEBSOCKET is called when a valid WebSocket upgrade request arrives.
If it accepts one argument, it is called synchronously as (funcall
ON-WEBSOCKET REQUEST).  If it accepts two arguments, it is called
as (funcall ON-WEBSOCKET REQUEST RESPOND) and must call (funcall RESPOND
RESULT) exactly once to return a result asynchronously.

REQUEST is the HTTP request plist with an additional :protocols key with
a list of subprotocol strings from Sec-WebSocket-Protocol headers.

RESULT must be a plist with an :on-message key to accept the upgrade.
If RESULT does not contain :on-message, it is interpreted as a normal
HTTP response plist and the upgrade is rejected.

Accepted RESULT keys:

:on-message ON-MESSAGE -- Required.  Called as (funcall ON-MESSAGE CONN
TYPE MESSAGE) when a complete message arrives.  TYPE is `text' or
`binary'.  For text frames MESSAGE is a decoded multibyte string; for
binary frames it is a unibyte string.  Fragmented messages are
reassembled before delivery.

:on-open ON-OPEN -- Called as (funcall ON-OPEN CONN) immediately after
the protocol has been switched and the connection is ready for WebSocket
traffic.

:on-close ON-CLOSE -- Called as (funcall ON-CLOSE CONN CODE REASON) when
the connection closes, either cleanly or abnormally.  CODE is an integer
WebSocket close code (1000 for normal closure, 1006 for abnormal
closure without a Close frame).  REASON is a string.

:on-ping ON-PING -- Called as (funcall ON-PING CONN PAYLOAD) after the
library automatically has sent a Pong in response to a client Ping.

:on-pong ON-PONG -- Called as (funcall ON-PONG CONN PAYLOAD) when a Pong
frame arrives, for example in response to `http-server-ws-ping'.

:protocol STRING -- The selected subprotocol.  If non-nil, sent in the
Sec-WebSocket-Protocol response header.  Must be one of the values in
the :protocols list on REQUEST or nil to decline subprotocol
negotiation."
  (lambda (proc request send-response)
    (http-server-ws--handle-upgrade proc request on-websocket send-response)))

(defun http-server-ws--handle-upgrade (proc request on-websocket send-response)
  "Validate the WebSocket handshake in REQUEST on PROC, then upgrade.

ON-WEBSOCKET is the user-supplied callback.  To accept the upgrade it
must return a plist with at least :on-message.  To reject, it returns a
normal HTTP response plist.  SEND-RESPONSE is a callback to send an
asynchronous HTTP response, such as 101 Switching Protocols to accept
the WebSocket connection."
  (catch 'http-server-ws--upgrade-done
    (let* ((headers (plist-get request :headers))
           (version (alist-get 'Sec-WebSocket-Version headers))
           (key (alist-get 'Sec-WebSocket-Key headers)))

      ;; RFC 6455 §4.2.1: opening handshake MUST be a GET request
      (when (not (eq (plist-get request :method) 'GET))
        (funcall send-response '(:status Bad-Request))
        (throw 'http-server-ws--upgrade-done nil))

      ;; RFC 6455 §4.2.1: version MUST be 13
      (when (not (equal version "13"))
        (funcall send-response
                 '( :status Upgrade-Required
                    :headers ((Sec-WebSocket-Version . "13"))))
        (throw 'http-server-ws--upgrade-done nil))

      ;; Sec-WebSocket-Key must be present
      (when (not key)
        (funcall send-response '(:status Bad-Request))
        (throw 'http-server-ws--upgrade-done nil))

      ;; Parse Sec-WebSocket-Protocol headers (comma-separated values allowed)
      (let* ((protocol-headers
              (cl-loop for (name . value) in headers
                       when (eq name 'Sec-WebSocket-Protocol)
                       append (mapcar #'string-trim (split-string value ","))))
             (augmented-request (plist-put (copy-sequence request)
                                           :protocols protocol-headers))
             (accept-key (http-server-ws--accept-key key))
             (respond
              (lambda (result)
                (if (plist-get result :on-message)
                    (let* ((chosen-protocol (plist-get result :protocol))
                           (response-headers
                            `((Upgrade . "websocket")
                              (Connection . "Upgrade")
                              (Sec-WebSocket-Accept . ,accept-key)
                              ,@(when chosen-protocol
                                  `((Sec-WebSocket-Protocol . ,chosen-protocol))))))
                      (funcall send-response `( :status Switching-Protocols
                                                :headers ,response-headers))
                      (http-server-ws--install proc result))
                  (funcall send-response result)))))
        (pcase (func-arity on-websocket)
          (`(1 . 1)
           (funcall respond (funcall on-websocket augmented-request)))
          (`(2 . 2)
           (funcall on-websocket augmented-request respond))
          (arity
           (signal 'http-server-ws-error
                   `("ON-WEBSOCKET must accept exactly 1 or exactly 2 arguments, "
                     "but accepts " ,arity))))))))

(defun http-server-ws--install (proc properties)
  "Switch PROC to WebSocket mode using callbacks from PROPERTIES."
  (process-put proc :websocket
               (list :on-message    (plist-get properties :on-message)
                     :on-open       (plist-get properties :on-open)
                     :on-close      (plist-get properties :on-close)
                     :on-ping       (plist-get properties :on-ping)
                     :on-pong       (plist-get properties :on-pong)
                     :state         'open
                     :fragments     nil
                     :fragment-type nil))

  (set-process-filter proc #'http-server-ws--filter)
  (set-process-sentinel proc #'http-server-ws--sentinel)

  (when-let* ((on-open (plist-get properties :on-open)))
    (funcall on-open proc))

  ;; If the connection has already received messages, process them immediately
  (when (> (buffer-size (process-buffer proc)) 0)
    (http-server-ws--process-frames proc)))

;;; §5: Data Framing

(defun http-server-ws--unmask (payload masking-key)
  "XOR-unmask PAYLOAD using 4-byte MASKING-KEY per RFC 6455 §5.3."
  (let* ((len (length payload))
         (result (make-string len 0)))
    (dotimes (idx len)
      (aset result idx (logxor (aref payload idx) (aref masking-key (% idx 4)))))
    result))

(cl-defun http-server-ws--parse-frame (&key (require-masked t))
  "Parse and validate a WebSocket frame from the current buffer.

Returns:
  `incomplete'    -- more data needed
  (CODE . REASON) -- protocol violation; CODE is a WebSocket close code
                     and REASON a string for logging
  plist           -- (:fin FIN :opcode OPCODE :payload PAYLOAD)

FIN is t or nil.  OPCODE is an integer.  The PAYLOAD is always returned
unmasked.

When REQUIRE-MASKED is non-nil (the default), unmasked frames are
rejected as a protocol violation per RFC 6455 §5.1.

On success, point is after the last consumed byte."
  (goto-char (point-min))
  (let ((buf-end (point-max)))
    (if (< (- buf-end (point-min)) 2)
        'incomplete
      (let* ((pos (point-min))
             (byte0 (char-after pos))
             (fin (not (zerop (logand byte0 #x80))))
             (rsv (logand byte0 #x70))
             (opcode (logand byte0 #x0f))
             (byte1 (char-after (1+ pos)))
             (masked (not (zerop (logand byte1 #x80))))
             (payload-len-byte (logand byte1 #x7f)))
        ;; Resolve extended payload length, advancing cursor past length bytes.
        (let* ((cursor (+ pos 2))
               (payload-len
                (cond
                 ((= payload-len-byte 126)
                  (if (< (- buf-end cursor) 2)
                      'incomplete
                    (prog1 (logior (ash (char-after cursor) 8)
                                   (char-after (1+ cursor)))
                      (setq cursor (+ cursor 2)))))
                 ((= payload-len-byte 127)
                  (if (< (- buf-end cursor) 8)
                      'incomplete
                    (let ((n 0))
                      (dotimes (i 8)
                        (setq n (logior (ash n 8) (char-after (+ cursor i)))))
                      (setq cursor (+ cursor 8))
                      n)))
                 (t payload-len-byte))))
          (if (eq payload-len 'incomplete)
              'incomplete
            ;; Check we have enough bytes for the optional masking key and payload
            (if (> (+ cursor (if masked 4 0) payload-len) buf-end)
                'incomplete
              (let* ((payload
                      (if masked
                          (let* ((masking-key (buffer-substring cursor (+ cursor 4)))
                                 (cursor      (+ cursor 4)))
                            (prog1 (http-server-ws--unmask
                                    (buffer-substring cursor (+ cursor payload-len))
                                    masking-key)
                              (goto-char (+ cursor payload-len))))
                        (prog1 (buffer-substring cursor (+ cursor payload-len))
                          (goto-char (+ cursor payload-len))))))
                ;; Validate frame constraints per the RFC
                (cond
                 ;; RFC 6455 §5.2: RSV bits MUST be 0 unless an extension is negotiated
                 ((not (zerop rsv))
                  '(1002 . "RSV bits set"))
                 ;; RFC 6455 §5.2: Known opcodes
                 ((not (memq opcode '(#x0 #x1 #x2 #x8 #x9 #xA)))
                  '(1002 . "Unknown opcode"))
                 ;; RFC 6455 §5.1: clients MUST mask all frames
                 ((and require-masked (not masked))
                  '(1002 . "Client frame not masked"))
                 ;; RFC 6455 §5.5: Control frame constraints
                 ((and (>= opcode #x8) (> (length payload) 125))
                  '(1002 . "Control frame payload too large"))
                 ((and (>= opcode #x8) (not fin))
                  '(1002 . "Fragmented control frame"))
                 (t
                  (list :fin fin :opcode opcode :payload payload)))))))))))

(defun http-server-ws--build-frame (opcode payload &optional fragment)
  "Build a server-to-client WebSocket frame.

OPCODE is the frame opcode integer.  PAYLOAD is a unibyte string.
When FRAGMENT is non-nil, the FIN bit is cleared (non-final fragment
frame).  Otherwise the FIN bit is set.  Returns an unibyte string.

Signals `http-server-ws-error' if PAYLOAD exceeds 125 bytes for a
control frame (RFC 6455 §5.5).

Server frames are NOT masked per RFC 6455 §5.1."
  (when (and (>= opcode #x8) (> (length payload) 125))
    (signal 'http-server-ws-error
            (list (format "Control frame payload too large (%d > 125)"
                          (length payload)))))
  (let* ((fin-bit (not fragment))
         (payload-len (length payload))
         ;; Byte 0: FIN bit + opcode
         (byte0 (logior (if fin-bit #x80 #x00) opcode))
         (len-bytes
          (cond
           ((<= payload-len 125)
            (list payload-len))
           ((<= payload-len 65535)
            (list #x7E
                  (ash payload-len -8)
                  (logand payload-len #xff)))
           (t
            (list #x7F
                  ;; 8 bytes big-endian
                  (logand (ash payload-len -56) #xff)
                  (logand (ash payload-len -48) #xff)
                  (logand (ash payload-len -40) #xff)
                  (logand (ash payload-len -32) #xff)
                  (logand (ash payload-len -24) #xff)
                  (logand (ash payload-len -16) #xff)
                  (logand (ash payload-len -8)  #xff)
                  (logand payload-len           #xff)))))
         (header (apply #'unibyte-string byte0 len-bytes)))
    (concat header payload)))

;;; §6: Sending and Receiving Data

(defun http-server-ws--filter (proc chunk)
  "Process filter for WebSocket connections on PROC, receiving CHUNK."
  (handler-bind
      ((error
        (lambda (err)
          (http-server--log proc
            (error (format "WebSocket filter error: %s"
                           (http-server--log-escape (error-message-string err)))))
          (ignore-errors
            (http-server-ws--fail proc 1011 "Internal error")))))

    (http-server--log proc
      (debug (format "WS chunk received: %d bytes" (length chunk))))

    ;; Append chunk to the connection buffer at the process mark
    (when-let* ((buffer (process-buffer proc))
                ((buffer-live-p buffer)))
      (with-current-buffer buffer
        (goto-char (process-mark proc))
        (insert chunk)
        (set-marker (process-mark proc) (point))))

    (http-server-ws--process-frames proc)))

(defun http-server-ws--process-frames (proc)
  "Parse and dispatch all WebSocket frames from the buffer of PROC."
  (let ((continue t))
    (while continue
      (pcase (with-current-buffer (process-buffer proc)
               (http-server-ws--parse-frame))
        ('incomplete
         (setq continue nil))
        (`(,(and code (pred numberp)) . ,reason)
         (http-server-ws--fail proc code reason)
         (setq continue nil))
        (frame
         (let ((opcode  (plist-get frame :opcode))
               (payload (plist-get frame :payload)))
           (http-server--log proc
             (debug (format "WS frame: opcode=#x%x payload=%d fin=%s"
                            opcode (length payload) (plist-get frame :fin)))
             (trace (http-server--prefix-lines
                     "> "
                     (concat "\n" (http-server--log-fill
                                   (http-server--log-escape payload))))))
           ;; Clear frame from request buffer to prepare for the next one.
           (with-current-buffer (process-buffer proc)
             (delete-region (point-min) (point)))
           (pcase opcode
             ((or #x0 #x1 #x2)
              (http-server-ws--handle-data-frame proc frame))
             (#x8
              (http-server-ws--handle-close-frame proc frame)
              (setq continue nil))
             (#x9 (http-server-ws--handle-ping proc frame))
             (#xA (http-server-ws--handle-pong proc frame)))))))))

(defun http-server-ws--handle-data-frame (proc frame)
  "Handle a data or continuation FRAME on PROC."
  (catch 'done
    (let* ((ws (process-get proc :websocket))
           (opcode (plist-get frame :opcode))
           (fin (plist-get frame :fin))
           (payload (plist-get frame :payload)))

      (pcase opcode
        ;; New text or binary message
        ((or #x1 #x2)
         (when (plist-get ws :fragments)
           (http-server-ws--fail proc 1002 "Interleaved data frame")
           (throw 'done nil))
         (setq ws (plist-put ws :fragment-type (if (= opcode #x1) 'text 'binary)))
         (setq ws (plist-put ws :fragments (list payload))))

        ;; Continuation frame
        (#x0
         (when (not (plist-get ws :fragments))
           (http-server-ws--fail proc 1002 "Continuation without start")
           (throw 'done nil))
         (setq ws (plist-put ws :fragments
                             (cons payload (plist-get ws :fragments))))))

      (if fin
          ;; Message complete: reassemble and deliver.
          (let* ((complete-payload
                  (apply #'concat (nreverse (plist-get ws :fragments))))
                 (fragment-type (plist-get ws :fragment-type))
                 (on-message (plist-get ws :on-message))
                 (decoded
                  (if (eq fragment-type 'text)
                      (condition-case _err
                          (decode-coding-string complete-payload 'utf-8 t)
                        (error
                         (http-server-ws--fail proc 1007 "Invalid UTF-8")
                         (throw 'done nil)))
                    complete-payload)))
            ;; Clear fragment state.
            (setq ws (plist-put ws :fragments nil))
            (setq ws (plist-put ws :fragment-type nil))
            (process-put proc :websocket ws)
            (funcall on-message proc fragment-type decoded))

        ;; Not final fragment yet: save state and wait for more.
        (process-put proc :websocket ws)))))

(defun http-server-ws--handle-close-frame (proc frame)
  "Handle a close FRAME received on PROC."
  (catch 'http-server-ws--close-frame-done
    (let* ((payload (plist-get frame :payload))
           (ws (process-get proc :websocket))
           (on-close (plist-get ws :on-close))
           code reason)

      ;; Parse optional close code and reason per RFC 6455 §5.5.1.
      (cond
       ((= (length payload) 0)
        (setq code 1005 reason ""))
       ((= (length payload) 1)
        ;; Malformed: single byte is not a valid close code.
        (http-server-ws--fail proc 1002 "Malformed close frame")
        (throw 'http-server-ws--close-frame-done nil))
       (t
        (setq code (logior (ash (aref payload 0) 8) (aref payload 1)))
        (setq reason
              (condition-case _err
                  (decode-coding-string (substring payload 2) 'utf-8 t)
                (error "")))))

      ;; Echo close frame if we are still open. RFC 6455 §5.5.1.
      (when (eq (plist-get ws :state) 'open)
        (let* ((close-payload (unibyte-string (ash code -8) (logand code #xff)))
               (frame (http-server-ws--build-frame #x8 close-payload)))
          (ignore-error http-server-client-disconnected
            (http-server--process-send-string proc frame))))

      ;; Update state and notify.
      (setq ws (plist-put ws :state 'closing))
      (process-put proc :websocket ws)

      (when on-close
        (funcall on-close proc code reason))

      (http-server--close-connection proc))))

(defun http-server-ws--handle-ping (proc frame)
  "Handle a ping FRAME on PROC by sending a pong and calling :on-ping."
  (let* ((payload (plist-get frame :payload))
         (ws (process-get proc :websocket))
         (on-ping (plist-get ws :on-ping)))
    ;; Respond with pong using same payload. RFC 6455 §5.5.3.
    (http-server--log proc
      (debug (format "Pong (%d bytes)" (length payload)))
      (trace (when (length> payload 0)
               (http-server--prefix-lines
                "< "
                (concat "\n" (http-server--log-fill (http-server--log-escape payload)))))))
    (ignore-error http-server-client-disconnected
      (http-server--process-send-string proc (http-server-ws--build-frame #xA payload)))
    (when on-ping
      (funcall on-ping proc payload))))

(defun http-server-ws--handle-pong (proc frame)
  "Handle a pong FRAME on PROC by calling :on-pong."
  (let* ((ws (process-get proc :websocket))
         (on-pong (plist-get ws :on-pong)))
    (when on-pong
      (funcall on-pong proc (plist-get frame :payload)))))

;;; §7: Closing the Connection

(defun http-server-ws--fail (proc code &optional reason)
  "Fail the WebSocket connection on PROC with status CODE and REASON.

Sends a close frame, calls :on-close, and closes the connection."
  (http-server--log proc
    (error (format "WebSocket failure: %d %s" code
                   (http-server--log-escape (or reason "")))))
  (let* ((reason-bytes (encode-coding-string (or reason "") 'utf-8))
         (payload (concat (unibyte-string (ash code -8) (logand code #xff))
                          reason-bytes)))
    (ignore-error http-server-client-disconnected
      (http-server--process-send-string proc (http-server-ws--build-frame #x8 payload))))
  (when-let* ((ws (process-get proc :websocket))
              (on-close (plist-get ws :on-close)))
    (funcall on-close proc code (or reason "")))
  (http-server--close-connection proc))

(defun http-server-ws--sentinel (proc event)
  "Sentinel for WebSocket connections on PROC receiving EVENT.

Called when the underlying TCP connection closes or errors.  Calls
:on-close with code 1006 (abnormal closure) if the connection was still
open."
  (unless (string-prefix-p "open" event)
    (http-server--log proc
      (trace (format "Process event: %s" (string-trim event))))
    (when-let* ((ws (process-get proc :websocket))
                (on-close (plist-get ws :on-close))
                (state (plist-get ws :state)))
      (pcase state
        ;; TCP dropped without a close handshake — abnormal per RFC 6455 §7.1.5.
        ('open
         (http-server--log proc (info "Abnormal closure (1006)"))
         (funcall on-close proc 1006 ""))
        ;; Close handshake completed, TCP teardown is expected.
        ('closing
         (http-server--log proc (trace "TCP closed after close handshake")))))
    (http-server--close-connection proc)))

;;; Public API

(defun http-server-ws-send (conn type message)
  "Send MESSAGE over WebSocket connection CONN.

TYPE is `text' or `binary'.  For text frames, MESSAGE is a multibyte
string that will be UTF-8 encoded.  For binary frames, MESSAGE is a
unibyte string sent as-is."
  (let ((ws (process-get conn :websocket)))
    (unless (eq (plist-get ws :state) 'open)
      (signal 'http-ws-server-error '("Cannot send on closed WebSocket")))
    (let* ((text-p (eq type 'text))
           (opcode (if text-p #x1 #x2))
           (payload (if text-p
                        (encode-coding-string message 'utf-8)
                      message)))
      (http-server--log conn
        (debug (format "WS %s (%d bytes)" type (length payload)))
        (trace (http-server--prefix-lines
                "< "
                (concat "\n" (http-server--log-fill (http-server--log-escape payload))))))
      (http-server--process-send-string conn (http-server-ws--build-frame opcode payload)))))

(cl-defun http-server-ws-ping (conn &optional (payload ""))
  "Send a ping frame over WebSocket connection CONN with optional PAYLOAD.

PAYLOAD must be a unibyte string."
  (let ((ws (process-get conn :websocket)))
    (when (eq (plist-get ws :state) 'open)
      (http-server--log conn
        (debug (format "Ping (%d bytes)" (length payload)))
        (trace (when (length> payload 0)
                 (http-server--prefix-lines
                  "< "
                  (concat "\n" (http-server--log-fill (http-server--log-escape payload)))))))
      (http-server--process-send-string conn (http-server-ws--build-frame #x9 payload)))))

(cl-defun http-server-ws-close (conn &optional (code 1000) (reason ""))
  "Initiate the closing handshake on WebSocket connection CONN.

CODE defaults to 1000 (normal closure).  REASON is an optional human-
readable string.  After sending the close frame the connection waits for
the client's echoed close before the TCP connection is closed."
  (let ((ws (process-get conn :websocket)))
    (unless (eq (plist-get ws :state) 'closing)
      (let* ((reason-bytes (encode-coding-string reason 'utf-8))
             (payload (concat (unibyte-string (logand (ash code -8) #xff)
                                              (logand code #xff))
                              reason-bytes)))
        (http-server--log conn
          (info (format "Close: %d %s" code (http-server--log-escape reason))))
        (process-put conn :websocket (plist-put ws :state 'closing))
        ;; We want to close anyway, so it doesn't matter if the client has closed the
        ;; connection already
        (ignore-error http-server-client-disconnected
          (http-server--process-send-string conn (http-server-ws--build-frame #x8 payload)))))))

(provide 'http-server-ws)
;;; http-server-ws.el ends here
