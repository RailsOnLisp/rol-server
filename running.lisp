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

(in-package :lowh.triangle.server)

(defvar *app-cache*
  (make-hash-table :test 'equal)
  "Hash table for storing modification time of loaded app sources.")

(defun clear-app-cache ()
  (clrhash *app-cache*))

(defvar *app-modules*
  nil
  "Modules to be loaded along with the app.")

(defun clear-modules ()
  (setq *app-modules* nil))

(defun require-module (name)
  (pushnew name *app-modules* :test #'string=))

(defun load-app ()
  (dolist (dir '("config/*.lisp"
		 "app/models/*.lisp"
		 "app/controllers/*.lisp"))
    (dolist (module (cons nil (reverse *app-modules*)))
      (when module
	(setq dir (str "lib/triangle/" module "/" dir)))
      (dolist (file (directory dir))
	(when (alphanumericp (char (pathname-name file) 0))
	  (let* ((name (enough-namestring file))
		 (date (file-write-date file))
		 (cached (gethash name *app-cache* -1)))
	    (unless (= cached date)
	      (setf (gethash name *app-cache*) date)
	      (log-msg :info "loading ~S (~S < ~S)" name cached date)
	      (load name))))))))

(defun run-handled ()
  (load-facts)
  (backend-run))

(defun run-protected ()
  (handler-bind ((error
		  (lambda (e)
		    (log-msg :error "~A" e)))
		 (sb-sys:interactive-interrupt
		  (lambda (c)
		    (declare (ignore c))
		    (log-msg :emerg "caught interrupt")
		    (return-from run-protected 0))))
    (with-logged-warnings
      (run-handled))))

(defun run ()
  (unwind-protect (run-protected)
    (log-msg :info "exit")))
