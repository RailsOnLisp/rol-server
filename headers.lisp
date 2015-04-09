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

;;  Headers

(defun status (&rest parts)
  (log-msg :info (str parts))
  (apply #'backend-status parts))

(defun header (name &rest parts)
  (apply #'backend-header name parts))

(defmacro define-header-function (name)
  `(defun ,name (&rest parts)
     (apply #'header ,(intern (string-upcase name) :keyword) parts)))

(define-header-function content-type)

(defun content-length (bytes)
  (header :content-length bytes))

;;  Redirections

(defun redirect-to (target &rest params &key &allow-other-keys)
  (let ((query (when params (format nil "?~{~A=~A~^&~}" params))))
    (etypecase target
      (string (status "303 See Other")
	      (header "Location" target query))
      (cons (redirect-to (str (route-reverse target) query))))))

(defun not-modified ()
  (status "403 Not Modified"))

;;  Cookies

(define-constant +rfc822-day+
    #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun")
  :test 'equalp)

(define-constant +rfc822-month+
    #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
  :test 'equalp)

(defun rfc1123-date-time (universal-time)
  (multiple-value-bind (second minute hour day month year dow)
      (decode-universal-time universal-time 0)
    (format nil "~A, ~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D GMT"
	    (aref +rfc822-day+ dow) day (svref +rfc822-month+ (1- month)) year
	    hour minute second)))

(defun parse-rfc1123-day (string)
  (1+ (position string +rfc822-day+ :test #'string-equal)))

(defun parse-rfc1123-month (string)
  (1+ (position string +rfc822-month+ :test #'string-equal)))

(defpackage rfc1123-timezone)

(defun parse-rfc1123-timezone (string)
  (symbol-value (find-symbol (string-upcase string) :rfc1123-timezone)))

(defun parse-rfc1123-date-time (string)
  "FIXME: check against RFC1123"
  (cl-ppcre:register-groups-bind ((#'parse-rfc1123-day dow)
				  (#'parse-integer d)
				  (#'parse-rfc1123-month mo)
				  (#'parse-integer y h mi s)
				  (#'parse-rfc1123-timezone tz))
      ("([A-Za-z]{3}), ([0-9]+) ([A-Za-z]{3}) ([0-9]+) ([0-9]+):([0-9]+):([0-9]+) ([A-Z]{3})"
       string)
    (values (encode-universal-time s mi h d mo y tz) dow)))

(defun set-cookie (name value expires &optional (domain *host*) (path "/")
		   secure (http-only t))
  (header :set-cookie
    name "=" value
    "; Expires=" (rfc1123-date-time expires)
    "; Domain=" domain
    "; Path=" path
    (when secure "; Secure")
    (when http-only "; HttpOnly")))

(defun delete-cookie (name)
  (header :set-cookie name "=; Max-Age=0"))

(in-package :rfc1123-timezone)
(cl:defconstant GMT 0)
