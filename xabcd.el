;;; xabcd.el --- convert raw bytes to the X'ABCD' notation and back

;; Copyright (C) 2009 Florian v. Savigny

;; Author: Florian v. Savigny (florian at fsavigny dot dee ee)
;; (Please direct any comments there.)
;;
;; Thanks to Juanma Barranquero, who (via gnu.emacs.help) pointed out
;; to me where the missing information in the manual is.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;;; Commentary:
;;
;;  The X'ABCD' notation is a string representation of bytes which
;;  uses only the characters 0-9 and a to f, i.e. the hexadecimal
;;  digits. Every byte is represented as a two-digit hexadecimal
;;  number (meaning that the X'ABCD' notation has exactly double the
;;  size of the bytes it represents). The format is used by SQLite for
;;  passing binary data over the command line (and possibly
elsewhere).

;;; History:
;;
;;  (I really don't think that would be interesting.)


;;; Code:

(defun xabcd-to-byte-string (xabcd)
 "Convert the XABCD string representation of a byte sequence to the
bytes.
XABCD must be a string of two-digit hexadecimal numbers."
 (let ((ind 0)
       (length (length xabcd))
       next-hex-char
       byte-string)
   (while (< ind length)
     (setq next-hex-char (substring xabcd
                                    ind (+ ind 2)))
     (setq byte-string
           (concat byte-string
                   (char-to-string (string-to-number next-hex-char 16))))
     (setq ind (+ ind 2)))
   ;; return:
   byte-string))


(defun string-to-xabcd (string)
 "Convert STRING - byte by byte - to its xabcd representation.
The xabcd representation is a string of two-digit hexadecimal numbers,
e.g. the ascii newline character is represented as the string \"0a\",
an a umlaut encoded in UTF-8 is represented as \"81e4\", and so on."
 (let* ((byte-string (string-as-unibyte string))
        (length (length byte-string))
        (ind 0)
        next-byte
        xabcd)
   (while (< ind length)
     (setq next-byte (aref byte-string ind))
     (setq xabcd
           (concat xabcd
                   (format "%02x" next-byte))) ; pad on the left with 0 to make two
digits
     (setq ind (1+ ind)))
   ;; return:
   xabcd))


(defun file-to-xabcd (filename)
 "Return contents of FILENAME in xabcd notation."
 (let ((bytes-buffer (generate-new-buffer "*file contents*"))
        xabcd)
   (set-buffer bytes-buffer)
   (insert-file-contents-literally filename)
   (setq xabcd (string-to-xabcd (buffer-string)))
   (kill-buffer bytes-buffer)
   xabcd))


(defun xabcd-to-file (xabcd-string filename &optional silently
mustbenew)
 "Convert XABCD-STRING back to its bytes and write them to FILENAME.
If SILENTLY is t, don\'t emit the \"Wrote ... \" message.
MUSTBENEW is passed to `write-region', which see."
 (let
     ((coding-system-for-write 'no-conversion)
      (silently (if silently 'silence nil))) ; 'silence: neither t
nor nil nor a string
   (write-region
    (xabcd-to-byte-string xabcd-string) ; START (a string)
    nil                                 ; END (ignored when START is
a string)
    filename                            ; FILENAME
    nil                                 ; APPEND (seems an unlikely
idea)
    silently                            ; VISIT (neither t nor nil
nor a string: no message)
    nil                                 ; LOCKNAME
    mustbenew)))                        ; MUSTBENEW


(provide 'xabcd)

;;; xabcd.el ends here
