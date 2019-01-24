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

;; public

(defvar *layout* nil)
(defvar *port* nil)
(defvar *session*)
(defvar *session-timeout* (* 8 3600))
(defvar *session-cookie* :rails_on_lisp_sid)
(defvar *smtp-server* "localhost")
(defvar *smtp-user* "noreply")
(defvar *smtp-password* "")

(defvar *compile-assets* t)

(define-constant +crlf+ (coerce #(#\CR #\LF) 'string)
  :test 'string=)

;; transient

(defvar *method*)
(defvar *uri*)
(defvar *host*)
(defvar *form-data*)
(defvar *reply-sent*)
(defvar *reply-status*)
(defvar *reply-stream*)
