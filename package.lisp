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

(in-package :cl-user)

(defpackage :lowh.triangle.server
  (:nicknames :L>server)
  (:use :cl
	:alexandria
	:L>assets
	:L>ext
	:L>files
	:L>log
	:L>template
	:L>uri
	:let-over-lambda
	:str
	:trivial-gray-streams)
  (:export
   ;;  Config
   #:*debug*
   #:*port*
   ;;  Conditions
   #:http-error
   #:http-error-status
   #:http-error-message
   ;;  Crypto
   #:load-secret
   #:hmac
   #:hmac-string
   #:make-random-string
   ;;  Data
   #:load-facts
   ;;  Resource
   #:define-resource
   #:has-one
   #:has-many
   #:define-resource-macro
   ;;  Request
   #:accept-p
   ;;  Routing
   #:*method*
   #:define-route
   #:find-route
   #:route-reverse
   #:list-routes
   #:clear-routes
   ;;  Asset routes
   #:asset-controller
   #:define-assets-route
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
   #:session-attach-or-create
   #:session-create
   #:session-delete
   #:session-reset
   #:session-hmac
   ;;  Helpers
   #:+crlf+
   #:odd/even
   #:to-url
   #:uri-for
   #:j
   #:h
   #:markdown
   #:print-markdown
   #:bound-slots
   ;;  Views
   #:*layout*
   #:print-asset-tag
   #:render-error
   #:render-json
   #:render-text
   #:render-view
   ;;  Resource
   #:define-resource
   #:has-one
   #:has-many
   #:define-action
   #:index #:create #:update #:delete
   #:get #:post #:put
   ;;  Run
   #:load-app
   #:run
   ;;  Re-export other symbols
   . #.(let (list)
	 (dolist (pkg '(:L>assets :L>files :L>template :L>uri))
	   (do-external-symbols (sym pkg)
	     (push (symbol-name sym) list)))
	 list)
   ))
