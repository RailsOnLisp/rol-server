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

(cl-json:set-decoder-simple-clos-semantics)

(defgeneric j (thing)
  (:documentation "Returns the JSON code for THING."))

(defmethod j (thing)
  (json:encode-json-to-string thing))

(defmethod print-object ((object json:fluid-object) stream)
  (write-string (j object) stream))

(defun read-json-object (stream char)
  (with-input-from-string (char-stream (make-string 1 :initial-element char))
    (json:decode-json (make-concatenated-stream char-stream stream))))

(defun enable-json-object-syntax ()
  (set-macro-character #\{ #'read-json-object t))

(enable-json-object-syntax)

(defmethod lessp:lessp ((a json:fluid-object) (b json:fluid-object))
  (lessp:lessp (j a) (j b)))

(defmethod make-load-form ((obj json:fluid-object) &optional env)
  (declare (ignore env))
  `(json:decode-json ,(j obj)))

(defgeneric bound-slots (object))

(defmethod bound-slots (object)
  (loop :for def :in (closer-mop:class-slots (class-of object))
     :for name = (closer-mop:slot-definition-name def)
     :when (slot-boundp object name)
     :collect name))
