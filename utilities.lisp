;;; -*- coding: utf-8; mode: lisp -*-
;;;
;;;  Zanox REST API Client
;;;  Copyright (c) 2014 Deterministic Arts 
;;;
;;;  Licensed under the Apache License, Version 2.0 (the "License");
;;;  you may not use this file except in compliance with the License.
;;;  You may obtain a copy of the License at
;;;
;;;       http://www.apache.org/licenses/LICENSE-2.0
;;;
;;;  Unless required by applicable law or agreed to in writing, software
;;;  distributed under the License is distributed on an "AS IS" BASIS,
;;;  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or 
;;;  implied. See the License for the specific language governing permissions 
;;;  and limitations under the License.
;;;

(in-package "ZANOX-CLIENT")


(defun generate-noise (&optional (length 128))
  "generate-noise &optional LENGTH => BYTES

Creates and returns a byte array which contains at least LENGTH bits of 
random data. The function uses the best available method to generate the 
data, in particular, it will try to use a crypto-level secure random noise
generator, if one is available."
  ;; FIXME: This is definitely too simplistic... It should do for now, 
  ;; but must be replaced before this guy goes public!
  (let* ((size (floor (+ length 7) 8))
	 (array (make-array size :element-type '(unsigned-byte 8))))
    (declare (type (vector (unsigned-byte 8)) array) (type fixnum size))
    (loop :for k :upfrom 0 :below size
       :do (setf (aref array k) (random 256))
       :finally (return array))))


(defun encode-as-hex-string (array &key (start 0) (end (length array)) (upper-case t))
  "encode-as-hex-string ARRAY &key START END UPPER-CASE => STRING

Encodes the given vector of bytes using hexadecimal notation. If
START is supplied, then it defines the index of the first octet
to encode. If END is supplied, it defines the index of the first 
byte ARRAY, which will not be part of the result. If UPPER-CASE
is non-nil (the default), then the result string will contain 
only digits and upper-case letters, otherwise it will be digits 
and lower-case letters."
  (check-type array (vector (unsigned-byte 8)))
  (check-type start integer)
  (check-type end integer)
  (assert (<= 0 start end (length array)))
  (locally
    (declare (type (vector (unsigned-byte 8)) array))
    (let* ((size (- end start))
	   (answer (make-string (* 2 size) :element-type 'character))
	   (digits (if upper-case "0123456789ABCDEF" "0123456789abcdef")))
      (declare (type string answer) (type fixnum size))
      (loop 
	 :for k :of-type fixnum :upfrom start :below end
	 :for p :of-type fixnum :upfrom 0 :by 2
	 :do (let* ((byte (aref array k))
		    (low (char digits (logand byte #xF)))
		    (high (char digits (logand (ash byte -4) #xF))))
	       (declare (type character low high) (type fixnum byte))
	       (setf (char answer p) high) 
	       (setf (char answer (1+ p)) low))
	 :finally (return answer)))))


(defun generate-nonce (&key (length 128) (digest :md5))
  "generate-nonce &key LENGTH DIGEST => STRING

This function returns a string composed of only letters and 
digits. It is created by generating at least LENGTH bits of 
random information, and and generating a digest of type DIGEST
from this byte sequence. The resulting string is then encoded
as string of hexadecimal characters."
  (let ((noise (generate-noise length))
	(digest (make-digest digest)))
    (update-digest digest noise)
    (encode-as-hex-string (produce-digest digest))))
      
    
(defparameter *month-abbrevs*
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defparameter *day-of-week-abbrevs*
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))


(defun format-timestamp (&optional (timestamp (get-universal-time)))
  "format-timestamp TIMESTAMP => STRING

Formats the given TIMESTAMP (assumed to be an integer number obtained 
from get universal time) according to the guidelines required by the
HTTP standard and the Zanox authentication protocol. The format is

   www, dd mmm yyyy HH:MM:SS GMT#

where 

   www is the abbreviated name of the day-of-week (US english),
    dd is the day of the month (two digits)
   mmm is the abbreviated name of the month (again, US english)
  yyyy is the current year (four digits)
    HH is the hour (0 to 23, two digits)
    MM is the minute (0 to 59, two digits)
    SS is the second (0 to 59, two digits)

The 'GMT' above is a fixed part of the result, designating, that the
date/time is given in the GMT/UTC timezone."
  (multiple-value-bind (second minute hour day month year dow) (decode-universal-time timestamp 0)
    (format nil "~A, ~2,'0D ~A ~4,'0D ~2,'0D:~2,'0D:~2,'0D GMT"
	    (aref *day-of-week-abbrevs* dow) day (aref *month-abbrevs* (- month 1)) year
	    hour minute second)))


(declaim (ftype (function (character) t) whitespacep))

(defun whitespacep (char)
  "whitespacep CHARACTER => BOOLEAN

Tests, whether CHARACTER is a whitespace character according to 
the XML specification and returns true, if this is the case, or 
false otherwise. The specification currently defines the following
characters to be whitespace: ASCII 9 (Tab), ASCII 10 (Newline),
ASCII 13 (Return), ASCII 32 (Space)."
  (and (find char #.(concatenate 'string '(#\tab #\space #\return #\newline)) 
			 :test #'char=) t))

(declaim (ftype (function (string) string) trim-string))

(defun trim-string (string)
  "trim-string STRING => RESULT

Returns a copy of a string with leading and trailing whitespace
removed."
  (check-type string string)
  (let ((length (length string)))
	(declare (type string string))
	(declare (type fixnum length))
	(declare (inline whitespacep))
	(loop 
	   :for start :of-type fixnum :upfrom 0 :below length
	   :for char :of-type character = (char string start)
	   :unless (whitespacep char)
	   :do (loop 
			  :for end :of-type fixnum :downfrom length :above start
			  :for char :of-type character = (char string (- end 1))
			  :unless (whitespacep char)
			  :do (return-from trim-string (subseq string start end))
			  :finally (return-from trim-string "")))
	""))

(declaim (ftype (function (string) t) all-whitespace-p))

(defun all-whitespace-p (string)
  "all-whitespace-p STRING => BOOLEAN

Tests, whether the string STRING contains only whitespace characters.
Whitespace is defined as the set of all characters, for which the
whitespacep predicate returns true."
  (check-type string string)
  (locally (declare (type string string) (inline whitespacep))
	(loop
	   :for char :across string
	   :unless (whitespacep char)
	   :do (return-from all-whitespace-p nil)
	   :finally (return-from all-whitespace-p t))))


(defun parse-decimal-number (string &key (errorp t) (default nil))
  "parse-decimal STRING &key ERRORP DEFAULT => VALUE FLAG

Parses the string representation of a decimal number, returning the
parsed number value. Unlike parsing numbers using the built-in read
function, this function will always return an exact value, i.e.,
a ratio (if necessary), but never a float.

If parsing succeeds, then this function returns the numeric value
as primary result, and t as secondary value. Otherwise, if ERRORP, 
the function signals a condition of type simple-error, if the given 
STRING is not a valid string representation for a decimal number. 
If ERRORP is false, then the function returns the value supplied 
as DEFAULT as primary value, and nil as secondary value."
  (check-type string string)
  (flet ((raise (reason &rest args) 
		   (if errorp 
			   (error "malformed decimal ~S representation: ~A" string (apply #'format nil reason args))
			   (return-from parse-decimal-number (values default nil)))))
	(loop
	   :named parse
	   :with value = 0
	   :and exponent = 0
	   :and shift = 0
	   :and mode = :before-sign
	   :and sign = 1
	   :and exp-sign = 1
	   :for char :across string
	   :for code = (char-code char)
	   :for digit = (- code #.(char-code #\0))
	   :do (case char
			 ((#\+ #\-)
			  (case mode
				((:before-sign) 
				 (setf sign (if (char= char #\-) -1 1)
					   mode :after-sign))
				((:after-exp-marker) 
				 (setf exp-sign (if (char= char #\-) -1 1)
					   mode :after-exp-sign))
				(t (raise "found sign character ~A at invalid position" char))))
			 ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
			  (case mode
				((:integer) (setf value (+ (* value 10) digit)))
				((:exponent) (setf exponent (+ (* exponent 10) digit)))
				((:before-sign :after-sign) 
				 (setf value digit 
					   mode :integer))
				((:after-dot) 
				 (setf value (+ (* value 10) digit)
					   shift (1+ shift)
					   mode :fraction))
				((:fraction) 
				 (setf value (+ (* value 10) digit)
					   shift (1+ shift)))
				((:after-exp-marker :after-exp-sign)
				 (setf exponent digit
					   mode :exponent))
				(t (raise "found unexpected digit"))))
			 ((#\e #\E)
			  (case mode
				((:integer :fraction) (setf mode :after-exp-marker))
				(t (raise "found ~A at unexpected position" char))))
			 ((#\.)
			  (case mode
				((:integer) (setf mode :after-dot))
				(t (raise "found ~A at unexpected position" char))))
			 (t (raise "found unexpected character ~S" char)))
	   :finally (case mode
				  ((:integer) (return-from parse (values (* sign value) t)))
				  ((:fraction :exponent) (return-from parse (values (/ (* sign value) 
																	   (expt 10 (- shift (* exp-sign exponent)))) 
																	t)))
				  (t (raise "malformed syntax"))))))



(defmacro with-unique-names ((&rest names) &body body)
  (labels ((parse (elt)
			 (cond ((symbolp elt) (list elt `(gensym ,(concatenate 'string (symbol-name elt) "-"))))
				   ((consp elt) (list (car elt) `(gensym ,(cadr elt))))
				   (t (error "invalid binding form ~S" elt)))))
	(let ((binds (loop :for name :in names :collecting (parse name))))
	  `(let ,binds ,@body))))
