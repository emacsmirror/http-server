;;; http-server-tests.el --- Test suite for http-server.el -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'ert)
(require 'http-server)
(require 'plz)
(require 'rfc2231)

;;; Utilities

(defmacro with-server (server form &rest body)
  "Bind SERVER to FORM, execute BODY, then stop the server."
  (declare (indent 2) (debug (symbolp sexp body)))
  `(let* ((,server ,form))
     (unwind-protect
         (progn ,@body)
       (http-server-stop ,server))))

(cl-defun plz-server (method server path &rest args &key (timeout 1) &allow-other-keys)
  "Send a METHOD request to SERVER / PATH with plz.

Fail after at most TIMEOUT seconds.  ARGS are passed as-is to `plz'."
  (let (response curl-error)
    (cl-flet* ((decode-response (r)
                 (let* ((headers (mapcar (lambda (header)
                                           (cons (intern (capitalize (symbol-name (car header))))
                                                 (cdr header)))
                                         (plz-response-headers r)))
                        (coding-system
                         (if-let* ((content-type (alist-get 'Content-Type headers))
                                   (params (cdr (rfc2231-parse-string content-type)))
                                   (charset (alist-get 'charset params))
                                   (coding-system (coding-system-from-name charset)))
                             coding-system
                           'utf-8))
                        (body (decode-coding-string (plz-response-body r) coding-system)))
                   (setq response `( :status ,(plz-response-status r)
                                     :headers ,headers
                                     :body ,body))))
               (plz-else (e)
                 (if-let* ((err (plz-error-curl-error e)))
                     (setq curl-error err)
                   (decode-response (plz-error-response e)))))
      (with-timeout (timeout (error (format "Request timed out: %s %s %s" method server path)))
        (let ((plz-curl-default-args (append `("--request-target" ,path) plz-curl-default-args)))
          (apply #'plz method (http-server-url server)
                 :as 'response
                 :then (lambda (r) (decode-response r))
                 :else #'plz-else
                 ;; plz's decoding does not read the coding system correctly from Content-Type
                 :decode nil
                 args))
        (while (not (or curl-error response))
          (accept-process-output nil 0.01))))
    (if curl-error
        (signal 'error (format "curl error: %s" curl-error))
      response)))

(cl-defun respond-with (response &key
                                 (status nil check-status)
                                 (headers nil check-headers)
                                 (body nil check-body))
  "Check the response RESPONSE for STATUS, HEADERS and BODY."
  (and (or (not check-status)
           (= status (plist-get response :status)))
       (or (not check-headers)
           (cl-loop for (name . value) in headers
                    for received = (alist-get name (plist-get response :headers))
                    always (and received (or (not value) (equal value received)))))
       (or (not check-body)
           (equal body (plist-get response :body)))))

(cl-defun respond-with-explain (response &key
                                         (status nil check-status)
                                         (headers nil check-headers)
                                         (body nil check-body))
  "Explainer for `respond-with'.

Returns a failure message string if RESPONSE does not match the expected
STATUS, HEADERS, or BODY, or nil if all checked fields match."
  (let (failures)
    (when (and check-status (/= status (plist-get response :status)))
      (push (format "Expected status %s, got %s"
                    status (plist-get response :status))
            failures))
    (when check-headers
      (cl-loop for (name . value) in headers
               for received = (alist-get name (plist-get response :headers))
               unless (and received (or (not value) (equal value received)))
               do (push (format "Header %s: expected %S, got %S"
                                name value received)
                        failures)))
    (when (and check-body (not (equal body (plist-get response :body))))
      (push (format "Expected %s bytes, got %s bytes.\nGot:     %S\nReceived: %S"
                    (length body)
                    (length (plist-get response :body))
                    body
                    (plist-get response :body))
            failures))
    (when failures
      (mapconcat #'identity (nreverse failures) "\n"))))

(put 'respond-with 'ert-explainer #'respond-with-explain)

;;; Request parsing

(ert-deftest http-server-parse-a-get-request ()
  "Parse a basic GET request."
  (with-temp-buffer
    (insert "GET /some/path HTTP/1.1\r
Accept: text/elisp\r
\r
")
    (should (equal (http-server--parse-http-request)
                   '( :method "GET"
                      :target "/some/path"
                      :headers (("Accept" . "text/elisp")))))))

(ert-deftest http-server-parse-a-post-request-with-body ()
  "Parse a POST request with body."
  (with-temp-buffer
    (insert "POST /the/form HTTP/1.1\r
Content-Type: application/x-www-form-urlencoded\r
Content-Length: 27\r
\r
field1=value1&field2=value2")
    (should (equal (http-server--parse-http-request)
                   '( :method "POST"
                      :target "/the/form"
                      :headers (("Content-Type" . "application/x-www-form-urlencoded")
                                ("Content-Length" . "27"))
                      :body "field1=value1&field2=value2")))))

(ert-deftest http-server-parse-a-post-request-with-empty-body ()
  "Parse a POST request with body."
  (with-temp-buffer
    (insert "POST /the/form HTTP/1.1\r
Content-Length: 0\r
\r
")
    (should (equal (http-server--parse-http-request)
                   '( :method "POST"
                      :target "/the/form"
                      :headers (("Content-Length" . "0"))
                      :body "")))))

(ert-deftest http-server-parse-a-request-with-newline-in-body ()
  "Parse a request with a newline character in the body."
  (with-temp-buffer
    (insert "PATCH / HTTP/1.1\r
Content-Length: 3\r
\r
a
b")
    (should (equal (http-server--parse-http-request)
                   '( :method "PATCH"
                      :target "/"
                      :headers (("Content-Length" . "3"))
                      :body "a\nb")))))

(ert-deftest http-server-parse-request-chunked-transmission ()
  "Parse a request arriving in chunks from the network."
  (with-temp-buffer
    (insert "POST /chunked HTTP/")
    (save-excursion
      (should (equal (http-server--parse-http-request) 'incomplete)))
    (insert "1.1\r
Content-Length: ")
    (save-excursion
      (should (equal (http-server--parse-http-request) 'incomplete)))
    (insert "4\r
")
    (save-excursion
      (should (equal (http-server--parse-http-request) 'incomplete)))
    (insert "\r
abc")
    (save-excursion
      (should (equal (http-server--parse-http-request) 'incomplete)))
    (insert "d")
    (should (equal (http-server--parse-http-request)
                   '( :method "POST"
                      :target "/chunked"
                      :headers (("Content-Length" . "4"))
                      :body "abcd")))))

(ert-deftest http-server-parse-transfer-encoding-chunked ()
  "Parse a request in chunked transfer encoding."
  (with-temp-buffer
    (insert "POST /upload HTTP/1.1\r
Content-Type: text/markdown; charset=utf-8\r
Transfer-Encoding: chunked\r
\r
10\r
* a heading\nwith\r
B\r
 body text.\r
0\r
\r
")
    (should (equal (http-server--parse-http-request)
                   '( :method "POST"
                      :target "/upload"
                      ;; No Transfer-Encoding here
                      :headers (("Content-Type" . "text/markdown; charset=utf-8"))
                      :body "* a heading
with body text.")))))

;;; Parsing incomplete request

(ert-deftest http-server-parse-a-request-with-incomplete-body ()
  "Parse a request with incomplete body."
  (with-temp-buffer
    (insert "PUT / HTTP/1.1\r
Content-Length: 10\r
\r
four")
    (should (equal (http-server--parse-http-request) 'incomplete))))

(ert-deftest http-server-parse-an-incomplete-request-line ()
  "Parse an incomplete request line."
  (with-temp-buffer
    (insert "GET /some/path H")
    (should (equal (http-server--parse-http-request) 'incomplete))))

(ert-deftest http-server-parse-an-incomplete-header-line ()
  "Parse an incomplete header line."
  (with-temp-buffer
    (insert "GET /some/path HTTP/1.1\r
Authoriz")
    (should (equal (http-server--parse-http-request) 'incomplete))))

;;; Parsing invalid requests

(ert-deftest http-server-parse-an-invalid-request-line ()
  "Parse an incomplete request line."
  (with-temp-buffer
    (insert "GET /some/path HTXX/1.1\r
Accept: text/html\r
\r
")
    (should (equal (car (http-server--parse-http-request)) 'Bad-Request))))

(ert-deftest http-server-parse-an-invalid-header-line ()
  "Parse an invalid header line."
  (with-temp-buffer
    (insert "PATCH / HTTP/1.1\r
Authorizationno-sep-bearer\r
\r
The update")
    (should (equal (car (http-server--parse-http-request)) 'Bad-Request))))

(ert-deftest http-server-parse-request-with-content-length-and-transfer-encoding ()
  "Parsing fails with Content-Length and Transfer-Encoding."
  (with-temp-buffer
    (insert "PATCH / HTTP/1.1\r
Content-Length: 0\r
Transfer-Encoding: chunked\r
\r
")
    (should (equal (car (http-server--parse-http-request)) 'Bad-Request))))

(ert-deftest http-server-parse-transfer-encoding-chunk-too-long ()
  "Request with a TE chunk longer than announced."
  (with-temp-buffer
    (insert "POST / HTTP/1.1\r
Transfer-Encoding: chunked\r
\r
4\r
long text\r
0\r
\r
")
    (should (equal (car (http-server--parse-http-request)) 'Bad-Request))))

(ert-deftest http-server-parse-request-with-unknown-transfer-encoding ()
  "Request with unknown transfer encoding fails."
  (with-temp-buffer
    (insert "PATCH / HTTP/1.1\r
Transfer-Encoding: gzip, chunked\r
\r
0\r
\r
")
    (should (equal (car (http-server--parse-http-request)) 'Not-Implemented))))

(ert-deftest http-server-parse-transfer-encoding-chunked-trailers ()
  "Request with TE trailers fails."
  (with-temp-buffer
    (insert "POST / HTTP/1.1\r
Transfer-Encoding: chunked\r
\r
4\r
text\r
0\r
Content-Type: text/html
\r
")
    (should (equal (car (http-server--parse-http-request)) 'Not-Implemented))))

;;; Parsing request targets

(ert-deftest http-server-decode-hex-target ()
  "Decode %xx encoded UTF-8 characters in the URI."
  (let* ((path "/€")
         (query "pet=🐜&weather=⛅")
         (target (concat (url-hexify-string path url-path-allowed-chars)
                         "?"
                         (url-hexify-string query url-path-allowed-chars))))
    (should (equal (http-server--decode-origin-form-target target)
                   (cons path query)))))

;;; On-the-network HTTP interactions

(ert-deftest http-server-rejects-invalid-content-length ()
  (with-server server (http-server-start :log-level 'trace)
    (should (respond-with (plz-server 'post server "/"
                                      :headers '((Content-Length . "-1"))
                                      :body "test")
                          :status 400))))

(ert-deftest http-server-rejects-unsupported-methods ()
  (dolist (method '(trace options connect))
    (with-server server (http-server-start :log-level 'trace)
      (should (respond-with (plz-server method server "/")
                            :status 501)))))

(ert-deftest http-server-rejects-unsupported-request-targets ()
  (dolist (path '("*" "no/slash/first" "/€"))
    (with-server server (http-server-start :log-level 'trace)
      (should (respond-with (plz-server 'get server path) :status 400)))))

(ert-deftest http-server-accepts-extra-methods ()
  (with-server server (http-server-start :log-level 'trace)
    (should (respond-with (plz-server 'extra server "/")
                          :status 400)))
  (with-server server (http-server-start :on-request (lambda (_r) '(:status OK))
                                         :extra-methods '(EXTRA)
                                         :log-level 'trace)
    (should (respond-with (plz-server 'extra server "/")
                          :status 200))))

(ert-deftest http-server-not-found-without-request-handler ()
  (with-server server (http-server-start :log-level 'trace)
    (should (respond-with (plz-server 'get server "/") :status 404))))

(ert-deftest http-server-sends-default-status ()
  (with-server server (http-server-start :on-request (lambda (request) nil)
                                         :default-status 'Method-Not-Allowed
                                         :log-level 'trace)
    (should (respond-with (plz-server 'get server "/")
                          :status 405))))

(ert-deftest http-server-sends-numerical-status ()
  (with-server server (http-server-start :on-request
                                         (lambda (_r)
                                           '(:status 201))
                                         :log-level 'trace)
    (should (respond-with (plz-server 'get server "/") :status 201))))

(ert-deftest http-server-sends-string-status ()
  (with-server server (http-server-start :on-request
                                         (lambda (_r)
                                           '(:status "418 I'm a teapot"))
                                         :log-level 'trace)
    (should (respond-with (plz-server 'get server "/") :status 418))))

(ert-deftest http-server-sends-status-headers-and-body ()
  (with-server server (http-server-start :on-request (lambda (_request)
                                                       '( :status Created
                                                          :headers ((Location . "http://server/item"))
                                                          :body "some\ntext"))
                                         :log-level 'trace)
    (should (respond-with (plz-server 'get server "/")
                          :status 201
                          :headers '((Location . "http://server/item")
                                     (Content-Length . "9"))
                          :body "some\ntext"))))

(ert-deftest http-server-passes-request-to-handler ()
  "Check that the server passes request to the :on-request handler."
  (let (request)
    (with-server server (http-server-start :on-request (lambda (r) (setq request r) nil)
                                           :log-level 'trace)
      (plz-server 'post server "/path?name=Emacs"
                  :headers '(("Accept" . "text/elisp")
                             ;; Check for case-insensitive headers
                             ("coNTent-tyPE" . "text/plain"))
                  :body "The Body")
      (should (equal (plist-get request :method) 'POST))
      (should (equal (plist-get request :path) "/path"))
      (should (equal (plist-get request :query) "name=Emacs"))
      (should (equal (alist-get 'Accept (plist-get request :headers)) "text/elisp"))
      (should (equal (alist-get 'Content-Type (plist-get request :headers)) "text/plain"))
      (should (equal (plist-get request :body) "The Body"))
      (should (processp (plist-get request :connection))))))

(ert-deftest http-server-accepts-extra-headers ()
  "Check that the server passes extra headers to the :on-request handler."
  (let (request)
    (with-server server (http-server-start :on-request
                                           (lambda (r)
                                             (setq request r)
                                             '(:status OK))
                                           :extra-headers '(X-Emacs-Header)
                                           :log-level 'trace)
      (should (respond-with (plz-server 'get server "/" :headers '(("X-Emacs-Header" . "1.0")))
                            :status 200))
      (should (equal (alist-get 'X-Emacs-Header (plist-get request :headers)) "1.0")))))

(ert-deftest http-server-unknown-headers-ignored-by-default ()
  "Check that unknown headers are silently dropped by default."
  (let (request)
    (with-server server (http-server-start :on-request
                                           (lambda (r)
                                             (setq request r)
                                             '(:status OK))
                                           :log-level 'trace)
      (should (respond-with (plz-server 'get server "/" :headers '(("X-Custom" . "value")))
                            :status 200))
      (should-not (alist-get 'X-Custom (plist-get request :headers))))))

(ert-deftest http-server-unknown-headers-function ()
  "Check that :unknown-headers function can transform or drop headers."
  (let (request)
    (with-server server (http-server-start :on-request
                                           (lambda (r)
                                             (setq request r)
                                             '(:status OK))
                                           :unknown-headers
                                           (lambda (name)
                                             (when (string-prefix-p "x-keep" (downcase name))
                                               (intern name)))
                                           :log-level 'trace)
      (should (respond-with (plz-server 'get server "/"
                                        :headers '(("X-Keep-This" . "yes")
                                                   ("X-Drop-This" . "no")))
                            :status 200))
      (should (equal (alist-get 'X-Keep-This (plist-get request :headers)) "yes"))
      (should-not (alist-get 'X-Drop-This (plist-get request :headers))))))

(ert-deftest http-server-sends-async-body ()
  (with-server server (http-server-start
                       :on-request
                       (lambda (_request)
                         (list :status 'OK
                               :headers '((Content-Type . "text/plain"))
                               :body
                               (lambda (send-chunk)
                                 (funcall send-chunk "hello world, with a long-ish\n" :keep-open t)
                                 (funcall send-chunk "message to test hex chunk length"))))
                       :log-level 'trace)
    (should (respond-with (plz-server 'get server "/")
                          :body "hello world, with a long-ish\nmessage to test hex chunk length"))))

(ert-deftest http-server-async-response-handler-with-async-body ()
  (with-server server (http-server-start
                       :kill-log-buffer nil
                       :on-request
                       (lambda (_request send-response)
                         (funcall send-response
                                  `( :status Not-Acceptable
                                     :body
                                     ,(lambda (send-chunk)
                                        (funcall send-chunk "body async or not?")))))
                       :log-level 'trace)
    (should (respond-with (plz-server 'put server "/")
                          :status 406
                          :body "body async or not?"))))

(provide 'http-server-tests)
;;; http-server-tests.el ends here
