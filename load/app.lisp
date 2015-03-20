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

(require :str)
(use-package :str)

(dolist (asd (directory "**/*.asd"))
  (unless (char= #\. (char (pathname-name asd) 0))
    (load asd)))

(require :alexandria)
(use-package :alexandria)

(require :lowh.triangle.files)
(use-package :L>files)

(require :lowh.triangle.server)
(use-package :L>server)

(require :lowh.triangle.assets)
(use-package :L>ext)

(require :cl-debug)
(use-package :debug)

(let ((env (cfg:getenv "RAILS_ENV")))
  (when env
    (setq cfg:*environment* (kw env))))

;;  Clear old definitions

(L>server:clear-app-cache)
(L>server:clear-routes)
(L>server:clear-template-cache)

(L>server:load-secret)
(L>server:load-app)

(defun reload ()
  (load "lib/triangle/server/load/app"))

(defun build (core)
  (sb-ext:save-lisp-and-die core
			    :toplevel #'run
			    #+sb-core-compression :compression
			    #+sb-core-compression t))
