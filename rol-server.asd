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

(in-package #:cl-user)

(defpackage #:RoL-server.system
  (:use #:cl #:asdf))

(in-package #:RoL-server.system)

#.(let* ((backend (if (boundp 'cl-user::*backend*)
                      (symbol-value 'cl-user::*backend*)
                      :hunchentoot))
         (backend-file (concatenate 'string "backend-"
                                    (string-downcase (symbol-name backend)))))
    (pushnew backend *features*)
    `(asdf:defsystem #:RoL-server
       :name "RoL-server"
       :author "Thomas de Grivel <thomas@lowh.net>"
       :version "0.2"
       :description "Application server / core module"
       :depends-on ("3bmd"
                    "alexandria"
                    "babel"
                    "cfg"
                    "cl-base64"
                    "cl-debug"
                    "ironclad"
                    "local-time"
                    "facts"
                    "flexi-streams"
                    "random-sequence"
                    "re"
                    "rol-assets"
                    "rol-files"
                    "rol-log"
                    "rol-template"
                    "rol-uri"
                    "str"
                    "trivial-backtrace"
                    "trivial-email-utf-8"
                    "trivial-utf-8"
                    "cl-inflector"
                    ,@(case backend
                            ((:fastcgi) '("cl-fastcgi"))
                            ((:hunchentoot) '("hunchentoot"))
                            ((:thot) '("thot"))))
       :components
       ((:file "package")
        (:file "conditions"  :depends-on ("package"))
        (:file "json"        :depends-on ("package"))
        (:file "resource"    :depends-on ("package"))
        (:file "secret"      :depends-on ("package"))
        (:file "vars"        :depends-on ("package"))
        (:file "assets"      :depends-on ("package" "routing" "vars"))
        (:file ,backend-file :depends-on ("package" "headers" "vars"))
        (:file "forms"       :depends-on ("json" ,backend-file))
        (:file "headers"     :depends-on ("package" "vars"))
        (:file "render"      :depends-on ("templates" ,backend-file))
        (:file "email"       :depends-on ("render"))
        (:file "reply"       :depends-on ("render" ,backend-file))
        (:file "templates"   :depends-on ("headers"))
        (:file "helpers"     :depends-on ("templates"))
        (:file "request"     :depends-on ("forms" ,backend-file))
        (:file "session"     :depends-on ("request" "secret"))
        (:file "routing"     :depends-on ("reply" "request"
                                          "templates" ,backend-file))
        (:file "facts"       :depends-on ("package"))
        (:file "running"     :depends-on ("routing" "facts" ,backend-file)))))
