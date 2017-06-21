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

(in-package :RoL-server)

(setf facts:*db-path* #P"data/app")

(defun maybe-rename-file (file new-name)
  (when (probe-file file)
    (log-msg :INFO "rename ~S -> ~S" file (namestring new-name))
    (rename-file file new-name)))

(defun pathname-without-dir (pathname)
  (make-pathname :name (pathname-name pathname)
                 :type (pathname-type pathname)))

(defun load-facts ()
  (facts:clear-db)
  (init-session-db)
  (maybe-rename-file "data/facts-log.lisp"
                     (pathname-without-dir (facts:db-log-path)))
  (dolist (file (directory "data/*.facts"))
    (when (alphanumericp (char (pathname-name file) 0))
      (log-msg :info "loading facts from ~S" (enough-namestring file))
      (let ((*package* (find-package :cl-user)))
        (facts:load-db file))))
  (dolist (file (directory "data/*.facts-log"))
    (when (alphanumericp (char (pathname-name file) 0))
      (log-msg :info "replaying log from ~S" (enough-namestring file))
      (load file)))
  (save-facts)
  t)

(defun save-facts ()
  (flet ((file-not-empty (path)
           (when (probe-file path)
             (with-open-file (s path)
               (< 0 (file-length s)))))
         (add-extension (file ext)
           (let ((path (str (pathname-name file)
                            "." (pathname-type file)
                            "." ext)))
             (maybe-rename-file file path))))
    (facts:with-transaction
      (let ((path (make-pathname :type "facts"
                                 :defaults facts:*db-path*))
            (log-path (facts:db-log-path))
            (time (format-timestring nil (now)
                                     :format +iso-8601-format+
                                     :timezone +utc-zone+)))
        (when (file-not-empty log-path)
          (add-extension log-path (str "merged-" time)))
        (when (file-not-empty path)
          (add-extension path (str "snapshot-" time)))
        (log-msg :INFO "saving facts into ~S" (namestring path))
        (facts:save-db :into path)))))
