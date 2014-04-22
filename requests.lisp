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
