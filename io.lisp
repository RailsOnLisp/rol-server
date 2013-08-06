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

(in-package :lowh.triangle.server)

(defclass fcgi-stream (trivial-gray-stream-mixin)
  ((req :initarg :req)
   (pos :initform 0)))

(defmethod stream-write-sequence ((stream fcgi-stream)
				  (sequence sequence)
				  start
				  end
				  &key)
  (stream-write-sequence stream
			 (subseq sequence
				 (or start 0)
				 end)
			 0 nil))

(defmethod stream-write-sequence ((stream fcgi-stream)
				  (sequence simple-base-string)
				  (start (eql 0))
				  (end (eql nil))
				  &key)
  (with-slots (req pos) stream
    (sb-fastcgi:fcgx-puts req sequence)
    (incf pos (length sequence))))

(defmethod stream-write-sequence ((stream fcgi-stream)
				  (sequence string)
				  (start (eql 0))
				  (end (eql nil))
				  &key)
  (with-slots (req pos) stream
    (sb-fastcgi:fcgx-puts-utf-8 req sequence)
    (incf pos (length sequence))))
