;;; http-server-ws-tests.el --- Test suite for http-server-ws.el -*- lexical-binding: t -*-

;; Author: Marten Lienen <ml@martenlienen.com>

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

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'http-server)
(require 'http-server-ws)
(require 'websocket)

;;; Helpers

(defmacro with-server (server form &rest body)
  "Bind SERVER to FORM, execute BODY, then stop the server."
  (declare (indent 2) (debug (symbolp sexp body)))
  `(let* ((,server ,form))
     (unwind-protect
         (progn ,@body)
       (http-server-stop ,server))))

(defmacro with-raw-connection (conn server &rest body)
  "Bind CONN to a new raw TCP connection to SERVER, execute BODY, then clean up."
  (declare (indent 2) (debug (symbolp sexp body)))
  `(let ((,conn (ws-raw-connect ,server)))
     (unwind-protect
         (progn ,@body)
       (when (process-live-p ,conn)
         (delete-process ,conn))
       (when-let* ((buf (process-buffer ,conn)))
         (kill-buffer buf)))))

(defun ws-raw-connect (server)
  "Make a raw TCP connection to SERVER, returning the process."
  (let ((host (process-contact server :host))
        (port (process-contact server :service)))
    (make-network-process
     :name "ws-raw-test"
     :host host
     :service port
     :coding 'binary
     :buffer (generate-new-buffer " *ws-raw-test*")
     :nowait nil)))

(defun ws-raw-send (proc string)
  "Send STRING over raw connection PROC."
  (process-send-string proc string))

(defconst ws-upgrade-key "dGhlIHNhbXBsZSBub25jZQ=="
  "RFC 6455 example Sec-WebSocket-Key value.")

(cl-defun ws-make-upgrade-request (&key (method "GET") (path "/") (host "localhost")
                                        (version "13") (key ws-upgrade-key))
  "Build a raw WebSocket upgrade request string.
METHOD, PATH, HOST, VERSION, and KEY are keyword arguments with sensible defaults.
Set KEY to nil to omit the Sec-WebSocket-Key header."
  (concat method " " path " HTTP/1.1\r\n"
          "Host: " host "\r\n"
          "Upgrade: websocket\r\n"
          "Connection: Upgrade\r\n"
          (when key (concat "Sec-WebSocket-Key: " key "\r\n"))
          "Sec-WebSocket-Version: " version "\r\n"
          "\r\n"))

(defmacro with-websocket (ws form &rest body)
  "Bind WS to FORM, execute BODY, then close."
  (declare (indent 2) (debug (symbolp sexp body)))
  `(let ((,ws ,form))
     (unwind-protect
         (progn ,@body)
       (when (websocket-openp ,ws)
         (websocket-close ,ws)))))

(cl-defun ws-wait-for-data (proc &optional (timeout 1))
  "Wait for data on raw connection PROC for at most TIMEOUT seconds."
  (accept-process-output proc timeout)
  (with-current-buffer (process-buffer proc)
    (buffer-string)))

(cl-defmacro ws-await (condition &key (timeout 1))
  "Process output until CONDITION becomes non-nil, erroring after TIMEOUT seconds."
  (declare (debug (sexp)))
  `(with-timeout (,timeout (error "Timeout waiting for: %s" ',condition))
     (while (not ,condition)
       (accept-process-output nil))))

(ert-deftest http-server-ws-accept-key ()
  "RFC 6455 §1.3 example for Sec-WebSocket-Accept computation."
  (should (equal (http-server-ws--accept-key "dGhlIHNhbXBsZSBub25jZQ==")
                 "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")))

;;; Frame Parsing (RFC Examples)

(ert-deftest http-server-ws-parse-unmasked-text ()
  "RFC §5.7 example 1: single-frame unmasked text \"Hello\" (server-to-client)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    ;; FIN=1, opcode=1 (text), MASK=0, len=5, literal "Hello"
    (insert #x81 #x05 #x48 #x65 #x6c #x6c #x6f)
    (let ((frame (http-server-ws--parse-frame :require-masked nil)))
      (should (listp frame))
      (should (eq (plist-get frame :fin) t))
      (should (= (plist-get frame :opcode) #x1))
      (should (equal (plist-get frame :payload) "Hello"))))
  ;; With default require-masked, this is a protocol violation.
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert #x81 #x05 #x48 #x65 #x6c #x6c #x6f)
    (should (equal (http-server-ws--parse-frame) '(1002 . "Client frame not masked")))))

(ert-deftest http-server-ws-parse-masked-text ()
  "RFC §5.7 example 2: single-frame masked text \"Hello\"."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    ;; FIN=1, opcode=1 (text), MASK=1, len=5, key=#x37fa213d, masked "Hello"
    (insert #x81 #x85 #x37 #xfa #x21 #x3d #x7f #x9f #x4d #x51 #x58)
    (let ((frame (http-server-ws--parse-frame)))
      (should (listp frame))
      (should (eq (plist-get frame :fin) t))
      (should (= (plist-get frame :opcode) #x1))
      (should (equal (plist-get frame :payload) "Hello")))))

(ert-deftest http-server-ws-parse-fragmented-frames ()
  "RFC §5.7 example 3: fragmented unmasked text \"Hel\" + \"lo\" (server-to-client)."
  ;; First frame: FIN=0, opcode=1 (text), MASK=0, len=3, literal "Hel"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert #x01 #x03 #x48 #x65 #x6c)
    (let ((frame (http-server-ws--parse-frame :require-masked nil)))
      (should (eq (plist-get frame :fin) nil))
      (should (= (plist-get frame :opcode) #x1))
      (should (equal (plist-get frame :payload) "Hel"))))
  ;; Second frame: FIN=1, opcode=0 (continuation), MASK=0, len=2, literal "lo"
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert #x80 #x02 #x6c #x6f)
    (let ((frame (http-server-ws--parse-frame :require-masked nil)))
      (should (eq (plist-get frame :fin) t))
      (should (= (plist-get frame :opcode) #x0))
      (should (equal (plist-get frame :payload) "lo")))))

(ert-deftest http-server-ws-parse-masked-pong ()
  "RFC §5.7 example 4: masked pong with \"Hello\" (client-to-server frame)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    ;; FIN=1, opcode=10 (pong), MASK=1, len=5, key=#x37fa213d, masked "Hello"
    (insert #x8a #x85 #x37 #xfa #x21 #x3d #x7f #x9f #x4d #x51 #x58)
    (let ((frame (http-server-ws--parse-frame)))
      (should (eq (plist-get frame :fin) t))
      (should (= (plist-get frame :opcode) #xa))
      (should (equal (plist-get frame :payload) "Hello")))))

(ert-deftest http-server-ws-parse-256-byte-binary ()
  "RFC §5.7 example 5: 256-byte binary with 16-bit extended length (server-to-client)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    ;; FIN=1, opcode=2 (binary), MASK=0, 16-bit length=256
    (let* ((len 256)
           (header (unibyte-string #x82 #x7e (ash len -8) (logand len #xff))))
      (insert header)
      (insert (make-string len 0)))
    (let ((frame (http-server-ws--parse-frame :require-masked nil)))
      (should (= (plist-get frame :opcode) #x2))
      (should (= (length (plist-get frame :payload)) 256)))))

(ert-deftest http-server-ws-parse-64k-binary ()
  "RFC §5.7 example 6: 64KiB binary with 64-bit extended length (server-to-client)."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    ;; FIN=1, opcode=2 (binary), MASK=0, 64-bit length=65536
    (let* ((len 65536)
           (header (unibyte-string #x82 #x7f
                                   0 0 0 0
                                   (ash len -24) (logand (ash len -16) #xff)
                                   (logand (ash len -8) #xff) (logand len #xff))))
      (insert header)
      (insert (make-string len 0)))
    (let ((frame (http-server-ws--parse-frame :require-masked nil)))
      (should (= (plist-get frame :opcode) #x2))
      (should (= (length (plist-get frame :payload)) 65536)))))

;;; More Frame Parsing

(ert-deftest http-server-ws-parse-unmasks-payload ()
  "Parser XORs payload bytes with the masking key, cycling every 4 bytes.

Uses key #x11223344.  The fifth payload byte is XORed with key byte 0
again, verifying the cycle."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    ;; FIN=1, opcode=1 (text), MASK=1, len=5, key=#x11223344
    ;; H=0x48^0x11=0x59, e=0x65^0x22=0x47, l=0x6c^0x33=0x5f,
    ;; l=0x6c^0x44=0x28, o=0x6f^0x11=0x7e  (key cycles: byte 5 uses key[0])
    (insert #x81 #x85 #x11 #x22 #x33 #x44 #x59 #x47 #x5f #x28 #x7e)
    (let ((frame (http-server-ws--parse-frame)))
      (should (equal (plist-get frame :payload) "Hello")))))

(ert-deftest http-server-ws-parse-incomplete ()
  "Parser returns `incomplete' for all truncated frame positions."
  ;; Missing byte 1 (length/mask byte).
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert #x81)
    (should (eq (http-server-ws--parse-frame) 'incomplete)))
  ;; Header complete (unmasked, len=5) but payload missing entirely.
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert #x81 #x05)
    (should (eq (http-server-ws--parse-frame) 'incomplete)))
  ;; Header complete (unmasked, len=5) but payload truncated (2 of 5 bytes).
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert #x81 #x05 #x48 #x65)
    (should (eq (http-server-ws--parse-frame) 'incomplete)))
  ;; 16-bit extended length: only 1 of 2 length bytes present.
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert #x82 #x7e #x01)
    (should (eq (http-server-ws--parse-frame) 'incomplete)))
  ;; 64-bit extended length: only 4 of 8 length bytes present.
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert #x82 #x7f #x00 #x00 #x00 #x00)
    (should (eq (http-server-ws--parse-frame) 'incomplete)))
  ;; Masked frame: masking key truncated (2 of 4 key bytes present, no payload).
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert #x81 #x85 #x37 #xfa)
    (should (eq (http-server-ws--parse-frame) 'incomplete)))
  ;; Masked frame: key complete but payload truncated (3 of 5 bytes).
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert #x81 #x85 #x37 #xfa #x21 #x3d #x7f #x9f #x4d)
    (should (eq (http-server-ws--parse-frame) 'incomplete))))

(ert-deftest http-server-ws-parse-rsv-bits ()
  "RSV bits set without negotiated extensions results in 1002."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    ;; RSV1 set: 0x81 | 0x40 = 0xC1; masked "Hello" with key #x37fa213d
    (insert #xC1 #x85 #x37 #xfa #x21 #x3d #x7f #x9f #x4d #x51 #x58)
    (should (equal (http-server-ws--parse-frame) '(1002 . "RSV bits set")))))

(ert-deftest http-server-ws-parse-multiple-frames ()
  "Two complete frames concatenated in the buffer are both parsed correctly.

This simulates two frames arriving in quick succession."
  ;; Masked "Hi" (text, opcode=1) then masked "!" (text, opcode=1).
  ;; Masking key #x00000000 is the identity XOR so the payload is literal.
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert #x81 #x82 #x00 #x00 #x00 #x00 ?H ?i)  ; frame 1: "Hi"
    (insert #x81 #x81 #x00 #x00 #x00 #x00 ?!)     ; frame 2: "!"
    ;; Parse first frame.
    (let ((frame1 (http-server-ws--parse-frame)))
      (should (equal (plist-get frame1 :payload) "Hi"))
      ;; Simulate what the filter does: delete consumed bytes.
      (delete-region (point-min) (point)))
    ;; Parse second frame from what remains.
    (let ((frame2 (http-server-ws--parse-frame)))
      (should (equal (plist-get frame2 :payload) "!")))))

;;; Frame Building & Unmask

(ert-deftest http-server-ws-build-frame-text ()
  "Build a simple text frame and verify the header."
  (let* ((payload "Hello")
         (frame (http-server-ws--build-frame #x1 payload)))
    ;; Byte 0: FIN=1 opcode=1 → #x81
    (should (= (aref frame 0) #x81))
    ;; Byte 1: no mask, len=5 → #x05
    (should (= (aref frame 1) #x05))
    ;; Remaining is the payload.
    (should (equal (substring frame 2) payload))))

(ert-deftest http-server-ws-build-frame-no-fin ()
  "Build a non-final fragment frame."
  ;; Pass a non-nil FRAGMENT arg to suppress the FIN bit.
  (let* ((frame (http-server-ws--build-frame #x1 "Hel" 'fragment)))
    ;; Byte 0: FIN=0 opcode=1 → #x01
    (should (= (aref frame 0) #x01))))

(ert-deftest http-server-ws-build-frame-16bit-length ()
  "Build a frame with 16-bit length encoding."
  (let* ((payload (make-string 256 ?x))
         (frame (http-server-ws--build-frame #x2 payload)))
    ;; Byte 1 should be 0x7E (126) for 16-bit extended length.
    (should (= (aref frame 1) #x7E))
    ;; Bytes 2-3: 256 = 0x01 0x00
    (should (= (aref frame 2) #x01))
    (should (= (aref frame 3) #x00))))

(ert-deftest http-server-ws-build-frame-control-too-large ()
  "Building a control frame with payload > 125 bytes signals http-server-ws-error."
  ;; All control opcodes (close=8, ping=9, pong=10) must be checked.
  (dolist (opcode '(#x8 #x9 #xA))
    (should-error
     (http-server-ws--build-frame opcode (make-string 126 0))
     :type 'http-server-ws-error))
  ;; 125 bytes is the boundary: must succeed.
  (dolist (opcode '(#x8 #x9 #xA))
    (should (http-server-ws--build-frame opcode (make-string 125 0)))))

(ert-deftest http-server-ws-unmask-identity ()
  "Unmasking with all-zero key returns payload unchanged."
  (let* ((payload "Hello World")
         (key (unibyte-string 0 0 0 0))
         (result (http-server-ws--unmask payload key)))
    (should (equal result payload))))

(ert-deftest http-server-ws-unmask-roundtrip ()
  "Masking twice with the same key recovers the original."
  (let* ((payload (unibyte-string 1 2 3 4 5 6 7 8))
         (key (unibyte-string #xAB #xCD #xEF #x01))
         (masked (http-server-ws--unmask payload key))
         (unmasked (http-server-ws--unmask masked key)))
    (should (equal unmasked payload))))

;;; Integration Tests (raw TCP)

(ert-deftest http-server-ws-upgrade-success ()
  "Successful WebSocket upgrade returns 101 with correct headers."
  (with-server server
      (http-server-start
       :on-upgrade (http-server-ws-on-upgrade
                    (lambda (_req) (list :on-message #'ignore)))
       :log-level 'trace)
    (with-raw-connection conn server
      (ws-raw-send conn (ws-make-upgrade-request))
      (let ((response (ws-wait-for-data conn)))
        (should response)
        (should (string-match-p "101 Switching Protocols" response))
        (should (string-match-p "Upgrade: websocket" response))
        (should (string-match-p "Connection: Upgrade" response))
        (should (string-match-p
                 (regexp-quote
                  (concat "Sec-WebSocket-Accept: "
                          (http-server-ws--accept-key ws-upgrade-key)))
                 response))))))

(ert-deftest http-server-ws-upgrade-without-handler ()
  "Without :on-websocket, upgrade requests are treated as normal HTTP."
  (with-server server
      (http-server-start
       :on-request (lambda (_req) '(:status OK))
       :log-level 'trace)
    (with-raw-connection conn server
      (ws-raw-send conn (ws-make-upgrade-request))
      (let ((response (ws-wait-for-data conn)))
        (should (string-match-p "200" response))))))

(ert-deftest http-server-ws-upgrade-handler-returns-nil ()
  "Handler returning nil results in default response."
  (with-server server
      (http-server-start
       :on-upgrade (http-server-ws-on-upgrade (lambda (_req) nil))
       :default-status 'Not-Found
       :log-level 'trace)
    (with-raw-connection conn server
      (ws-raw-send conn (ws-make-upgrade-request))
      (let ((response (ws-wait-for-data conn)))
        (should (string-match-p "404" response))))))

(ert-deftest http-server-ws-upgrade-wrong-version ()
  "Wrong Sec-WebSocket-Version results in 426."
  (with-server server
      (http-server-start
       :on-upgrade (http-server-ws-on-upgrade
                    (lambda (_req) (list :on-message #'ignore)))
       :log-level 'trace)
    (with-raw-connection conn server
      (ws-raw-send conn (ws-make-upgrade-request :version "8"))
      (let ((response (ws-wait-for-data conn)))
        (should (string-match-p "426" response))
        (should (string-match-p "Sec-WebSocket-Version: 13" response))))))

(ert-deftest http-server-ws-upgrade-missing-key ()
  "Missing Sec-WebSocket-Key results in 400."
  (with-server server
      (http-server-start
       :on-upgrade (http-server-ws-on-upgrade
                    (lambda (_req) (list :on-message #'ignore)))
       :log-level 'trace)
    (with-raw-connection conn server
      (ws-raw-send conn (ws-make-upgrade-request :key nil))
      (let ((response (ws-wait-for-data conn)))
        (should (string-match-p "400" response))))))

(ert-deftest http-server-ws-upgrade-non-get-method ()
  "Non-GET request with Upgrade header results in 400 per RFC 6455 §4.2.1."
  (with-server server
      (http-server-start
       :on-upgrade (http-server-ws-on-upgrade
                    (lambda (_req) (list :on-message #'ignore)))
       :log-level 'trace)
    (with-raw-connection conn server
      (ws-raw-send conn (ws-make-upgrade-request :method "POST"))
      (let ((response (ws-wait-for-data conn)))
        (should (string-match-p "400" response))))))

(ert-deftest http-server-ws-upgrade-on-open-called ()
  ":on-open callback is invoked after upgrade completes."
  (let (open-proc)
    (with-server server
        (http-server-start
         :on-upgrade (http-server-ws-on-upgrade
                      (lambda (_req)
                        (list :on-message #'ignore
                              :on-open (lambda (conn) (setq open-proc conn)))))
         :log-level 'trace)
      (with-raw-connection conn server
        (ws-raw-send conn (ws-make-upgrade-request))
        (ws-wait-for-data conn)
        (should (processp open-proc))))))

;;; Integration Tests (using websocket client)

(ert-deftest http-server-ws-echo-roundtrip ()
  "Full text message round-trip through an echo server."
  (let (received)
    (with-server server
        (http-server-start
         :on-upgrade (http-server-ws-on-upgrade
                      (lambda (_req)
                        (list :on-message #'http-server-ws-send)))
         :log-level 'trace)
      (with-websocket ws
          (websocket-open (http-server-url server "/" :protocol "ws")
                          :on-message (lambda (_ws frame)
                                        (push (websocket-frame-text frame) received)))
        (websocket-send-text ws "hello")
        (ws-await received)
        (should (equal (car received) "hello"))))))

(ert-deftest http-server-ws-binary-message ()
  "Binary message round-trip."
  (let (received)
    (with-server server
        (http-server-start
         :on-upgrade (http-server-ws-on-upgrade
                      (lambda (_req)
                        (list :on-message #'http-server-ws-send)))
         :log-level 'trace)
      (with-websocket ws
          (websocket-open (http-server-url server "/" :protocol "ws")
                          :on-message (lambda (_ws frame)
                                        (push (websocket-frame-payload frame) received)))
        (websocket-send ws (make-websocket-frame
                            :opcode 'binary
                            :payload (unibyte-string 1 2 3 4 5)
                            :completep t))
        (ws-await received)
        (should (equal (car received) (unibyte-string 1 2 3 4 5)))))))

(ert-deftest http-server-ws-server-initiated-ping ()
  "Server-initiated ping via `http-server-ws-ping'."
  (let (server-conn pong-received)
    (with-server server
        (http-server-start
         :on-upgrade (http-server-ws-on-upgrade
                      (lambda (_req)
                        (list :on-message #'ignore
                              :on-open (lambda (conn) (setq server-conn conn))
                              :on-pong (lambda (_conn _payload)
                                         (setq pong-received t)))))
         :log-level 'trace)
      (with-websocket ws
          (websocket-open (http-server-url server "/" :protocol "ws"))
        (ws-await server-conn)
        (http-server-ws-ping server-conn "ping-data")
        (ws-await pong-received)
        (should pong-received)))))

(ert-deftest http-server-ws-server-initiated-close ()
  "Server can initiate a clean close."
  (let (server-conn client-closed)
    (with-server server
        (http-server-start
         :on-upgrade (http-server-ws-on-upgrade
                      (lambda (_req)
                        (list :on-message #'ignore
                              :on-open (lambda (conn) (setq server-conn conn)))))
         :log-level 'trace)
      (with-websocket ws
          (websocket-open (http-server-url server "/" :protocol "ws")
                          :on-close (lambda (_ws) (setq client-closed t)))
        (ws-await server-conn)
        (http-server-ws-close server-conn 1000 "bye")
        (ws-await client-closed)
        (should client-closed)))))

(ert-deftest http-server-ws-on-close-called-on-disconnect ()
  ":on-close is called when the client disconnects."
  (let (close-called close-code)
    (with-server server
        (http-server-start
         :on-upgrade (http-server-ws-on-upgrade
                      (lambda (_req)
                        (list :on-message #'ignore
                              :on-close (lambda (_conn code _reason)
                                          (setq close-called t close-code code)))))
         :log-level 'trace)
      (with-websocket ws
          (websocket-open (http-server-url server "/" :protocol "ws"))
        ;; Work around an Emacs bug, where it writes to a closed socket and crashes with
        ;; SIGPIPE [1]
        ;;
        ;; [1] https://lists.gnu.org/archive/html/bug-gnu-emacs/2026-04/msg00506.html
        (ws-await (eq (websocket-ready-state ws) 'open))
        (websocket-close ws)
        (ws-await close-called)
        (should close-called)
        ;; 1000 = normal closure, 1006 = abnormal (depends on timing)
        (should (memq close-code '(1000 1006)))))))

(ert-deftest http-server-ws-unmasked-frame-protocol-error ()
  "Receiving an unmasked frame from a client causes a 1002 close."
  (with-server server
      (http-server-start
       :on-upgrade (http-server-ws-on-upgrade
                    (lambda (_req)
                      (list :on-message #'ignore
                            :on-close #'ignore)))
       :log-level 'trace)
    (with-raw-connection conn server
      ;; Complete the upgrade first.
      (ws-raw-send conn (ws-make-upgrade-request))
      (ws-wait-for-data conn)
      ;; Clear the buffer.
      (with-current-buffer (process-buffer conn)
        (erase-buffer))
      ;; Send an unmasked text frame (mask bit = 0).
      ;; FIN=1, opcode=1, mask=0, len=5, "Hello"
      (ws-raw-send conn (unibyte-string #x81 #x05 ?H ?e ?l ?l ?o))
      ;; Server should respond with a close frame (opcode=8).
      (let ((response (ws-wait-for-data conn)))
        (should (= (logand (aref response 0) #x0f) #x8))))))

(ert-deftest http-server-ws-http-and-ws-coexistence ()
  "HTTP and WebSocket endpoints work on the same server."
  (let (ws-message-received)
    (with-server server
        (http-server-start
         :on-request (lambda (req)
                       (if (equal (plist-get req :path) "/http")
                           '(:status OK :body "http ok")
                         '(:status Not-Found)))
         :on-upgrade (http-server-ws-on-upgrade
                      (lambda (_req)
                        (list :on-message (lambda (conn _type msg)
                                            (setq ws-message-received msg)
                                            (http-server-ws-send conn 'text "ws ok")))))
         :log-level 'trace)

      ;; Test HTTP endpoint via raw TCP.
      (with-raw-connection http-conn server
        (ws-raw-send http-conn "GET /http HTTP/1.1\r\nHost: localhost\r\n\r\n")
        (let ((http-response (ws-wait-for-data http-conn)))
          (should (and http-response (string-match-p "200 OK" http-response)))))

      ;; Test WebSocket endpoint.
      (with-websocket ws
          (websocket-open (http-server-url server "/" :protocol "ws")
                          :on-message #'ignore)
        (websocket-send-text ws "hello ws")
        (ws-await ws-message-received)
        (should (equal ws-message-received "hello ws"))))))

(ert-deftest http-server-ws-pipelined-upgrade-and-message ()
  "WebSocket frame pipelined with the Upgrade request is processed correctly."
  (let (received)
    (with-server server
        (http-server-start
         :on-upgrade (http-server-ws-on-upgrade
                      (lambda (_req)
                        (list :on-message (lambda (conn _type msg)
                                            (setq received msg)))))
         :log-level 'trace)
      (with-raw-connection conn server
        ;; Send upgrade request and a masked "Hi" frame in one write.
        ;; Masking key #x00000000 is the identity so payload is literal.
        (ws-raw-send conn (concat (ws-make-upgrade-request)
                                  (unibyte-string #x81 #x82 #x00 #x00 #x00 #x00 ?H ?i)))
        (ws-await received)
        (should (equal received "Hi"))))))

(provide 'http-server-ws-tests)
;;; http-server-ws-tests.el ends here
