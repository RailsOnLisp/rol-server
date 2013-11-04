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

#-hunchentoot
(defun header (name &rest parts)
  (write-rope `(,(string-capitalize name) ": " ,@parts ,+crlf+)
	      *headers-output*))

#+hunchentoot
(defun header (name &rest parts)
  (setf (hunchentoot:header-out name)
	(apply #'str parts)))

(defmacro define-header-function (name)
  `(defun ,name (&rest parts)
     (apply #'header ,(intern (string-upcase name) :keyword) parts)))

(define-header-function content-type)

(defun content-length (bytes)
  #+hunchentoot
  (setf (hunchentoot:header-out :content-length) bytes)
  #-hunchentoot
  (header :content-length (format nil "~D" bytes)))

;;  Redirections

(defun redirect-to (target)
  (etypecase target
    (string (header "Status" "303 See Other")
	    (header "Location" target))
    (cons (redirect-to (route-reverse target)))))

;;  Cookies

#-hunchentoot
(define-constant +rfc822-day+
    (coerce '("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
	    '(simple-array (simple-array base-char (3)) (7)))
  :test 'equalp)

#-hunchentoot
(define-constant +rfc822-month+
    (coerce '("Jan" "Feb" "Mar" "Apr"
	      "May" "Jun" "Jul" "Aug"
	      "Sep" "Oct" "Nov" "Dec")
	    '(simple-array (simple-array base-char (3)) (12)))
  :test 'equalp)

#-hunchentoot
(defun rfc1123-date-time (universal-time)
  (multiple-value-bind (second minute hour day month year dow)
      (decode-universal-time universal-time 0)
    (format nil "~A, ~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D GMT"
	    (aref +rfc822-day+ dow) day (aref +rfc822-month+ month) year
	    hour minute second)))

#-hunchentoot
(defun set-cookie (name value expires &optional (domain *host*) (path "/")
		   secure (http-only t))
  (header "Set-Cookie"
    name "=" value
    "; Expires=" (rfc1123-date-time expires)
    "; Domain=" domain
    "; Path=" path
    (when secure "; Secure")
    (when http-only "; HttpOnly")))

#+hunchentoot
(defun set-cookie (name value expires &optional (domain *host*) (path "/")
		   secure (http-only t))
  (hunchentoot:set-cookie name
			  :value value
			  :expires expires
			  :path path
			  :domain domain
			  :secure secure
			  :http-only http-only))
