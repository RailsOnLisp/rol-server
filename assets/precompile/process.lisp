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

(in-package :assets)

;;  Process assets

(defun process-js (input)
  (write-string
   (cl-uglify-js:ast-gen-code (cl-uglify-js:ast-mangle
			       (cl-uglify-js:ast-squeeze
				(parse-js:parse-js input)))
			      :beautify nil))
  (values))

#+nil(defun recess (path* &rest options)
  (let* ((path* (mapcar (lambda (p) (format nil "~A" p))
		       (if (listp path*) path* `(,path*))))
	 (fmt "~
var recess = require('recess')
  , src = ~A
  , opt = ~A
recess(src, opt, function (err, obj) {
  if (err) throw err
  process.stdout.write(JSON.stringify(obj))
})
")
	 (js (format nil fmt
		     (json:encode-json-to-string path*)
		     (json:encode-json-plist-to-string options))))
    (exec-js:from-string js :safely nil)))

(defun less (src parser-options css-options)
  (let* ((fmt "~
var path = require('path'),
    fs = require('fs'),
    sys = require('util'),
    os = require('os');
var less = require('less');
var src = path.resolve(process.cwd(), ~A);
var parser_opts = ~A;
var css_opts = ~A;

var print_error = function (e) {
  less.writeError(e);
  process.exit(2);
}
var print_tree = function (e, tree) {
  if (e)
    print_error(e);
  try {
    var css = tree.toCSS(css_opts);
    process.stdout.write(css);
  } catch (e) {
    print_error(e);
  }
}
var parse_data = function (e, data) {
  if (e)
    print_error(e);
  try {
    new(less.Parser)(parser_opts).parse(data, print_tree)
  } catch (e) {
    print_error(e);
  }
}
try {
  fs.readFile(src, 'utf8', parse_data);
} catch (e) {
  print_error(e);
}
")
	 (js (format nil fmt
		     (json:encode-json-to-string src)
		     (json:encode-json-plist-to-string parser-options)
		     (json:encode-json-plist-to-string css-options))))
    #+nil(format *error-output* "~%~A~%" js)
    (exec-js:from-string js :safely nil)))

(defun process-css (input)
  (with-temporary-file (tmp (format nil "tmp/~A.~A"
				    (pathname-name input)
				    (pathname-type input)))
    (cl-fad:copy-stream input tmp)
    (force-output tmp)
    (close tmp)
    (write-string (less (pathname tmp)
			(list :paths (mapcar #'truename (assets-dirs))
			      :filename (enough-namestring (pathname input)))
			(list :yuicompress t))))
  (values))

(defun process (type input)
  (ecase type
    (:js  (process-js  input))
    (:css (process-css input))))
