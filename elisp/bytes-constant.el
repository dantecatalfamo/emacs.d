;;; bytes-constant.el -- Add byte shorthand to elisp

;;; Commentary:

;; Just thought this might be a neat thing to add
;; My first macro, created without reference

;;; Code:
(require 'subr-x)
(require 'cl-lib)

(defmacro bytes-constant (byte-units)
  "Convert BYTE-UNITS to number.
Byte units are in the form [0-9]+[BKMGTPEZ].
Example: (bytes 12G)"
  (if-let* ((units '(B K M G T P E Z))
            (symbol-name (symbol-name byte-units))
            (unit (replace-regexp-in-string "[0-9]+" "" symbol-name))
            (number (string-to-number (replace-regexp-in-string "[BKMGTPEZ]" "" symbol-name)))
            (pow (cl-position unit units :test 'string=)))
      `(* ,number (expt 1024 ,pow))
    (error "Unrecognized unit %s" byte-units)))

(defalias 'byteconst #'bytes-constant)

(provide 'bytes-constant)
;;; bytes-constant.el ends here
