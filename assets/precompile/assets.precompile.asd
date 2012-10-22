;;
;;  Assets  -  Asset pipeline
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

(defpackage :assets.precompile.system
  (:use :cl :asdf))

(in-package :assets.precompile.system)

(asdf:defsystem :assets.precompile
  :name "assets.precompile"
  :author "Thomas de Grivel <billitch@gmail.com>"
  :version "0.1"
  :description "Precompile assets"
  :depends-on ("alexandria"
	       "assets"
	       "cl-uglify-js"
	       "exec-js"
	       "triangle.files")
  :components
  ((:file "msg")
   (:file "process" :depends-on ("msg"))
   (:file "preprocess" :depends-on ("process"))
   (:file "precompile" :depends-on ("preprocess"))
   (:file "generate" :depends-on ("msg"))))
