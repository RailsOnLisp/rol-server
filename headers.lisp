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

(defun header (line)
  (write-sequence line (reply-headers-stream *reply*)))

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

(defun set-cookie (name value expires domain path &key secure (http-only t))
  (push
   (format
    nil
    "Set-Cookie: ~A=~A; Expires=~A; Domain=~A; Path=~A~:[~;; Secure~]~:[~;; HttpOnly~]"
    name value (rfc1123-date-time expires) domain path secure http-only)
   (reply-header :set-cookie)))

(cookie :ltsess "abc" (+ 3600 (get-universal-time)) "lowh.net" "/")
