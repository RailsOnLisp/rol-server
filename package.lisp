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

(defpackage :lowh-triangle-server
  (:nicknames :/s)
  (:use :cl :alexandria :assets :triangle.files)
  (:export
   ;;  Config
   #:*debug*
   #:*port*
   ;;  Crypto
   #:load-secret
   #:hmac
   ;;  Data
   #:load-facts
   ;;  Routing
   #:*method*
   ;;  Forms
   #:with-form-data
   ;;  HTTP Headers
   #:header
   #:status
   #:content-type
   #:content-length
   #:redirect-to
   #:set-cookie
   ;;  Session
   #:*session*
   #:*session-cookie*
   #:*session-timeout*
   #:session-create
   ;;  Helpers
   #:odd/even
   #:to-url
   ;;  Views
   #:*layout*
   #:render-view
   #:render-error
   ;;  Run
   #:run
   ;;  Assets
   . #.(let (list)
	 (dolist (pkg '(:assets :triangle.files))
	   (do-external-symbols (sym pkg)
	     (push (symbol-name sym) list)))
	 list)
   ))
