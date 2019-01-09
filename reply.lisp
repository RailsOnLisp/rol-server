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

(defun reply-send ()
  (unless *reply-sent*
    (setq *reply-sent* t)
    (let ((content (flexi-streams:get-output-stream-sequence
                    (flexi-streams:flexi-stream-stream *reply-stream*))))
      (content-length (length content))
      (backend-send-headers)
      (backend-send content))))

(defmacro with-printed-errors ((&optional msg) &body body)
  `(handler-case
       (progn ,@body)
     (warning (c)
       (log-msg :WARN "~@[~A~]~A" msg c)
       nil)
     (t (c)
       (log-msg :ERROR "~@[~A~]~A" msg c)
       nil)))

(defmacro with-reply-handlers (&body body)
  `(with-simple-restart (reply "Send HTTP reply")
     (handler-bind ((error
                     (lambda (c)
                       (let ((status (http-error-status c))
                             (msg (http-error-message c))
                             backtrace)
                         (log-msg (if (char= #\5 (char status 0))
                                      :error
                                      :info)
                                  "~A ~A" status msg)
                         (ignore-errors
                           (trivial-backtrace:map-backtrace
                            (lambda (x) (push x backtrace))))
                         (flexi-streams:get-output-stream-sequence *reply-stream*)
                         (render-error status msg c backtrace)
                         (unless (debug-p :conditions)
                           (invoke-restart 'reply))))))
       ,@body)))

(defclass reply-stream (flexi-streams:flexi-output-stream) ()
  (:default-initargs
   :flexi-stream-external-format (flexi-streams:make-external-format :utf-8)
   :stream (flexi-streams:make-in-memory-output-stream
            :element-type '(unsigned-byte 8))))

(defmethod stream-element-type ((stream reply-stream))
  '(unsigned-byte 8))

(defmethod flexi-streams:get-output-stream-sequence ((stream reply-stream)
                                                     &key as-list)
  (flexi-streams:get-output-stream-sequence
   (flexi-streams:flexi-stream-stream stream) :as-list as-list))

(defmacro with-reply (&body body)
  `(let ((*reply-sent* nil)
         (*reply-status* nil)
         (*reply-stream* (make-instance 'reply-stream)))
     (with-reply-handlers
       ,@body)
     (reply-send)))

;;  Send file

(defun send-file (path)
  (let ((write-date (file-write-date path))
        (if-modified-since (request-header :if-modified-since)))
    (cond ((and if-modified-since
                (= (parse-rfc1123-date-time if-modified-since)
                   write-date))
           (status "304 not modified"))
          (t (header :last-modified (rfc1123-date-time write-date))
             (with-open-file (stream path :if-does-not-exist nil
                                     :element-type '(unsigned-byte 8))
               (unless stream
                 (http-error "404 Not found" "File not found"))
               (header :content-type (mime-type stream))
               (header :content-length (file-length stream))
               (backend-send-headers)
               (loop with buf = (make-array 4096 :element-type '(unsigned-byte 8))
                  for r = (read-sequence buf stream)
                  while (< 0 r)
                  do (backend-send (if (= r 4096) buf (subseq buf 0 r)))))))))
