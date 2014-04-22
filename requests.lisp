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


(defclass client ()
  ((connect-id
     :reader client-connect-id
     :initarg :connect-id)
   (secret-key 
     :reader client-secret-key
     :initarg :secret-key))
  (:documentation "Zanox API Client

Instances of this class represent the Zanox API. They contain all
necessary information the library needs to contact the Zanox servers,
authenticate, etc."))


(defvar *client* nil
  "The default client instance used, if no other client is specified
in one of the request APIs.")


(defun make-client (&key (connect-id nil connect-id-p)
                         (secret-key nil secret-key-p))
  "make-client &key CONNECT-ID SECRET-KEY => CLIENT

Creates a new API client instance, with properties as supplied."
  
  (unless connect-id-p (error "missing connect-id"))
  (unless secret-key-p (error "missing secret key"))
  (make-instance 'client
    :connect-id connect-id
    :secret-key secret-key))


(defmacro with-client ((var &rest init-args) &body forms)
  "with-client (VARIABLE INIT-FORMS*) BODY* => RESULT*"
  (let ((variable (gensym)))
	`(let ((,variable (make-client ,@init-args)))
	   (let ((*client* ,variable)
			 (,var ,variable))
		 ,@forms))))



(deftype http-method ()
  `(member :post :get :put :delete :head))



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
    (loop 
      :for k :upfrom 0 :below size
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



(defun generate-signature (client
                           &key (resource nil have-resource)
                                (http-method :get)
                                (timestamp nil have-timestamp)
                                (nonce nil have-nonce))
  (unless have-timestamp (error "missing timestamp"))
  (unless have-nonce (error "missing random bits"))
  (unless have-resource (error "missing resource"))
  (macrolet ((utf8 (form) `(string-to-octets ,form :encoding :utf-8)))
    (let ((plain-timestamp (format-timestamp timestamp))
          (hmac-key (utf8 (client-secret-key client))))
      (let ((formatted-timestamp (utf8 plain-timestamp))
            (formatted-nonce (utf8 nonce))
            (formatted-method (utf8 (symbol-name http-method)))
            (formatted-resource (utf8 (string-downcase resource)))
            (hmac (make-hmac hmac-key :sha1)))
        (update-hmac hmac formatted-method)
        (update-hmac hmac formatted-resource)
        (update-hmac hmac formatted-timestamp)
        (update-hmac hmac formatted-nonce)
        (usb8-array-to-base64-string (hmac-digest hmac))))))


(defun send-request (resource 
                     &key (client *client*)
                          (parser #'read-json-value)
                          (parameters '()) 
                          (http-method :get)
                          (api-version "2011-03-01")
                          (result-format :json)
                          (securep nil))
  (check-type http-method http-method)
  (check-type resource string)
  (check-type result-format (member :xml :json))
  (let ((uri (format nil "http://api.zanox.com/~A/~A~A" (string-downcase result-format) api-version resource))
        (headers nil))
    (if (not securep)
        (let ((date-header (format-timestamp))
              (auth-header (format nil "ZXWS ~A" (client-connect-id client))))
          (push (cons "Date" date-header) headers)
          (push (cons "Authorization" auth-header) headers))
        (let* ((timestamp (get-universal-time))
               (nonce (generate-nonce))
               (signature (generate-signature client :http-method http-method :resource resource :timestamp timestamp :nonce nonce))
               (date-header (format-timestamp timestamp))
               (auth-header (format nil "ZXWS ~A:~A" (client-connect-id client) signature)))
          (if (eq securep :use-parameters)
              (progn 
                (push (cons "signature" signature) parameters)
                (push (cons "connectid" (client-connect-id client)) parameters)
                (push (cons "date" date-header) parameters)
                (push (cons "nonce" nonce) parameters))
              (progn
                (push (cons "Date" date-header) headers)
                (push (cons "Authorization" auth-header) headers)
                (push (cons "nonce" nonce) headers)))))
    (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
        (http-request uri 
                      :method http-method 
                      :parameters parameters
                      :additional-headers headers
                      :want-stream t
                      :close t)
      (declare (ignorable headers uri))
      (unwind-protect (progn 
                        (unless (eql status-code 200)
                          (error "HTTP error ~D received: ~A" status-code reason-phrase))
                        (funcall parser body))
        (when must-close
          (close stream))))))
