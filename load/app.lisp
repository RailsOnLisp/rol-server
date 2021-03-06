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

(require :str)
(use-package :str)

(dolist (asd (directory "**/*.asd"))
  (unless (char= #\. (char (pathname-name asd) 0))
    (load asd)))

(asdf:load-system :alexandria)
(use-package :alexandria)

(asdf:load-system :RoL-files)
(use-package :RoL-files)

(asdf:load-system :RoL-server)
#+sbcl(shadow 'sb-ext:create)
(use-package :RoL-server)

(asdf:load-system :RoL-assets)
(use-package :RoL-ext)

(asdf:load-system :cl-debug)
(use-package :debug)

;;  Clear old definitions

(RoL-server:clear-app-cache)
(RoL-server:clear-routes)
(RoL-server:clear-template-cache)

(RoL-server:load-secret)
(RoL-server:load-app)

(defun reload ()
  (load "lib/rol/server/load/app"))

(defun build (core)
  (cache-assets)
  (sb-ext:save-lisp-and-die core
                            :toplevel #'run
                            #+sb-core-compression :compression
                            #+sb-core-compression t))
