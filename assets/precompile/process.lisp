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

(defgeneric process-asset (asset output included-p))

;;  CSS

(defun less (src-path parser-options css-options &optional out)
  (let* ((fmt "~
var path = require('path'),
    fs = require('fs'),
    sys = require('util'),
    os = require('os');
var less = require('less');
var src = ~A;
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
  fs.readFile(path.resolve(process.cwd(), src), 'utf8', parse_data);
} catch (e) {
  print_error(e);
}
")
	 (js (format nil fmt
		     (json:encode-json-to-string src-path)
		     (json:encode-json-plist-to-string parser-options)
		     (json:encode-json-plist-to-string css-options))))
    #+nil(format *error-output* "~%~A~%" js)
    (exec-js:from-string js :safely nil :out out)))

(defmethod process-asset ((asset css-asset)
			  (output stream)
			  included-p)
  (let ((true-assets-dirs (cache-1 (eq *assets-dirs*)
			    (mapcar #'truename (assets-dirs))))
	(path (asset-source-path asset)))
    (less path
	  (list :paths true-assets-dirs	:filename path)
	  (list :yuicompress t)
	  output))
  (values))

;;  JS

(defmethod process-asset ((asset js-asset)
			  (output stream)
			  included-p)
  (write-string (with-input-from-file/utf-8 (js (asset-source-path asset))
		  (cl-uglify-js:ast-gen-code (cl-uglify-js:ast-mangle
					      (cl-uglify-js:ast-squeeze
					       (parse-js:parse-js js)))
					     :beautify nil))
		output)
  (values))
