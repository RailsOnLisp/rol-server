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

(in-package :RoL-server)

;;  Secret

(define-constant +secret-file+ #P"config/secret.bin" :test 'equal)
(define-constant +secret-length+ 256)
(defvar *secret* (make-array +secret-length+ :element-type '(unsigned-byte 8)))

(defun read-secret (file)
  (with-input-from-file (stream file :element-type '(unsigned-byte 8))
    (assert (= +secret-length+ (read-sequence *secret* stream)))
    (values)))

(defun create-secret ()
  (with-output-to-file (stream +secret-file+ :element-type '(unsigned-byte 8))
    (read-secret #P"/dev/random")
    (sb-posix:fchmod (sb-sys:fd-stream-fd stream) #o600)
    (write-sequence *secret* stream)
    (force-output stream)
    (sb-posix:fchmod (sb-sys:fd-stream-fd stream) #o400)
    (values)))

(defun load-secret ()
  (if (fad:file-exists-p +secret-file+)
      (read-secret +secret-file+)
      (create-secret)))

;;  HMAC

(defun hmac (&rest parts)
  (let ((h (ironclad:make-hmac *secret* :sha512)))
    (dolist (part parts)
      (ironclad:update-hmac h (trivial-utf-8:string-to-utf-8-bytes
			       (string part) :null-terminate t)))
    (ironclad:hmac-digest h)))

(defun hmac-string (&rest parts)
  (cl-base64:usb8-array-to-base64-string (apply #'hmac parts)
					 :uri t))

;;  Random

(defun make-random-bytes (length)
  (let ((seq (make-array length :element-type '(unsigned-byte 8))))
    (with-input-from-file (r #P"/dev/random" :element-type '(unsigned-byte 8))
      (read-sequence seq r))
    seq))

(defun make-random-string (length)
  (subseq (cl-base64:usb8-array-to-base64-string (make-random-bytes
						  (ceiling length 4/3))
						 :uri t)
	  0 length))
