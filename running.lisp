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

(defun run-handled ()
  (setf html-template:*default-template-pathname*
	(merge-pathnames #P"app/views/"))
  (load-facts)
  (log-msg :info "starting fastcgi at 127.0.0.1:~A" *port*)
  (sb-fastcgi:socket-server #'route :inet-addr "127.0.0.1" :port *port*)
  (error "fastcgi socket server exited"))

(defun run-protected ()
  (handler-bind ((sb-sys:interactive-interrupt
		  (lambda (c)
		    (declare (ignore c))
		    (log-msg :emerg "caught interrupt")
		    (return-from run-protected 0)))
		 (warning
		  (lambda (w)
		    (log-msg :warn "~A" w)
		    (muffle-warning w)))
		 (condition
		  (lambda (c)
		    (let ((status (http-error-status c)))
		      (log-msg (if (char= #\5 (char status 0)) :error :info)
			       "~A" c)
		      (render-error status (http-error-message c))))))
    (run-handled)))

(defun run ()
  (let ((status 1))
    (unwind-protect (setf status (run-protected))
      (log-msg :info "exit")
      (sb-ext:exit :code status))))
