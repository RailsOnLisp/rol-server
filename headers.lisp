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

;;  Headers

(defun crlf (&optional (stream *standard-output*))
  (write-char #\CR stream)
  (write-char #\LF stream))

(defun header (&rest parts)
  (dolist (part parts)
    (when part
      (write-string (string part) *headers-output*)))
  (crlf *headers-output*))

(defmacro define-header-function (name)
  `(defun ,name (&rest parts)
     (apply #'header
	    ,(concatenate 'string (string-capitalize name) ": ")
	    parts)))

(define-header-function status)
(define-header-function content-type)

(defun content-length (bytes)
  (format *headers-output* "Content-Length: ~D" bytes)
  (crlf *headers-output*))

;;  Redirections

(defun redirect-to (target)
  (etypecase target
    (string (header "Status: 303 See Other")
	    (header "Location: " target))
    (cons (redirect-to (facts:first-bound ((?url :routed-by target)))))))

;;  Cookies

(define-constant +rfc822-day+
    (coerce '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
	    '(simple-array (simple-array base-char (3)) (7)))
  :test 'equalp)

(define-constant +rfc822-month+
    (coerce '("Jan" "Feb" "Mar" "Apr"
	      "May" "Jun" "Jul" "Aug"
	      "Sep" "Oct" "Nov" "Dec")
	    '(simple-array (simple-array base-char (3)) (12)))
  :test 'equalp)

(defun rfc1123-date-time (universal-time)
  (multiple-value-bind (second minute hour day month year dow)
      (decode-universal-time universal-time 0)
    (format nil "~A, ~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D GMT"
	    (aref +rfc822-day+ dow) day (aref +rfc822-month+ month) year
	    hour minute second)))

(defun set-cookie (name value expires &optional (domain *host*) (path "/")
		   secure (http-only t))
  (header "Set-Cookie: " name "=" value
	  "; Expires=" (rfc1123-date-time expires)
	  "; Domain=" domain
	  "; Path=" path
	  (when secure "; Secure")
	  (when http-only "; HttpOnly")))
