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
