;;
;;  LowH Triangle Server
;;
;;  Copyright 2012 Thomas de Grivel <billitch@gmail.com>
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

(in-package :lowh-triangle-server)

(define-constant +secret-file+ #P"config/secret.bin" :test 'equal)
(define-constant +secret-length+ 32)
(defvar *secret* (make-array +secret-length+ :element-type '(unsigned-byte 8)))

(defun read-secret (file)
  (with-input-from-file (stream file :element-type '(unsigned-byte 8))
    (read-sequence *secret* stream)
    t))

(defun create-secret ()
  (with-output-to-file (stream +secret-file+ :element-type '(unsigned-byte 8))
    (read-secret #P"/dev/urandom")
    (sb-posix:fchmod (sb-sys:fd-stream-fd stream) #o600)
    (write-sequence *secret* stream)
    (sb-posix:fchmod (sb-sys:fd-stream-fd stream) #o400)
    t))

(defun load-secret ()
  (if (fad:file-exists-p +secret-file+)
      (read-secret +secret-file+)
      (create-secret)))

(defun make-random-string (length)
  (let ((seq (make-array (ceiling length 4/3) :element-type '(unsigned-byte 8))))
    (with-input-from-file (r #P"/dev/random" :element-type '(unsigned-byte 8))
      (read-sequence seq r))
    (subseq (cl-base64:usb8-array-to-base64-string seq :uri t) 0 length)))

(defun hmac (&rest parts)
  (let ((h (ironclad:make-hmac *secret* :sha256)))
    (dolist (part parts)
      (ironclad:update-hmac h (trivial-utf-8:string-to-utf-8-bytes
			       (string part) :null-terminate t)))
    (ironclad:hmac-digest h)))
