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

(setf facts:*db-path* #P"data/app")

(defun maybe-rename-file (file new-name)
  (when (probe-file file)
    (log-msg :INFO "rename ~S -> ~S" file new-name)
    (rename-file file new-name)))

(defun load-facts ()
  (facts:clear-db)
  (init-session-db)
  (maybe-rename-file "data/facts-log.lisp" "app.facts-log")
  (dolist (file (directory "data/*.facts"))
    (when (alphanumericp (char (pathname-name file) 0))
      (log-msg :info "loading facts from ~S" (enough-namestring file))
      (let ((*package* (find-package :cl-user)))
	(facts:load-db file))))
  (dolist (file (directory "data/*.facts-log"))
    (when (alphanumericp (char (pathname-name file) 0))
      (log-msg :info "replaying log from ~S" (enough-namestring file))
      (load file)))
  t)
