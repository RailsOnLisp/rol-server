;;
;;  RoL-server  -  Rails on Lisp application server
;;
;;  Copyright 2012-2015 Thomas de Grivel <thomas@lowh.net>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :RoL-server)

;;  Config

(defun backend-request-method ()
  (thot:request-method))

(defun backend-request-header (name)
  (thot:request-header name))

(defun backend-request-uri ()
  (thot:request-uri))

(defun backend-request-remote-addr ()
  (thot:request-remote-addr))

;;  Forms

(defun backend-read-request-data ()
  (thot:request-data))

(defun backend-read-form-data ()
  (let ((content-type (string-downcase
                       (thot:request-header "content-type"))))
    (cond ((cl-ppcre:scan "^application/x-www-form-urlencoded\\b" content-type)
           (parse-www-form-url-encoded (backend-read-request-data)))
          ((cl-ppcre:scan "^application/json\\b" content-type)
           (parse-www-form-json-encoded (backend-read-request-data))))))

;;  Reply

(defgeneric backend-send (data))

(defmethod backend-send ((data string))
  (when (debug-p :reply)
    (log-msg :debug "SEND ~S" (if (eq +crlf+ data)
                                  '+crlf+
                                  data)))
  (stream:stream-write-sequence (thot:reply-stream) data)
  (values))

(defmethod backend-send ((data simple-array))
  (when (debug-p :reply)
    (log-msg :debug "SEND ~D bytes" (length data)))
  (stream:stream-write-sequence (thot:reply-stream) data)
  (values))

(defmethod backend-send ((data vector))
  (when (debug-p :reply)
    (log-msg :debug "SEND ~D bytes" (length data)))
  (stream:stream-write-sequence (thot:reply-stream) data)
  (values))

;;  Reply headers

(defun backend-status (&rest parts)
  (thot:status (str parts)))

(defun backend-header (name &rest parts)
  (apply #'thot:header (string-capitalize name) ": " parts))

(defun backend-send-headers ()
  (thot:end-headers))

;;  Running

(defvar *headers-output*)

(defun thot-handler ()
  '(route-request))

(defun backend-run ()
  (let ((thot:*url-handlers*
         '((thot:file-handler "public/" "/")
           (thot-handler))))
    (log-msg :info "starting thot at 0.0.0.0:~A" *port*)
    (thot:start :host "0.0.0.0" :port *port*)))
