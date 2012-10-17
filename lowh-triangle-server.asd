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

(defpackage :lowh-triangle-server.system
  (:use :cl :asdf))

(in-package :lowh-triangle-server.system)

(asdf:defsystem :lowh-triangle-server
  :name "lowh-triangle-server"
  :author "Thomas de Grivel <billitch@gmail.com>"
  :version "0.1"
  :description "LowH Triangle Server"
  :depends-on ("alexandria"
	       "assets"
	       "cl-base64"
	       "html-template"
	       "ironclad"
	       "lowh-facts"
	       "sb-fastcgi"
	       "triangle.files"
	       "trivial-utf-8")
  :components
  ((:file "package")
   (:file "config" :depends-on ("package"))
   (:file "secret" :depends-on ("package"))
   (:file "vars" :depends-on ("package"))
   (:file "logging" :depends-on ("package"))
   (:file "conditions" :depends-on ("package"))
   (:file "headers" :depends-on ("package" "vars"))
   (:file "render" :depends-on ("headers"))
   (:file "templates" :depends-on ("headers"))
   (:file "helpers" :depends-on ("templates"))
   (:file "routing" :depends-on ("render" "templates"))
   (:file "facts" :depends-on ("package"))
   (:file "running" :depends-on ("routing" "facts"))))
