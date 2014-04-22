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

(asdf:defsystem :zanox-client
  :version "0.1.1"
  :licence "Apache Licence 2.0"
  :serial t
  :depends-on (:drakma :ironclad :cxml :cl-base64 :split-sequence :cl-ppcre 
               :babel :darts.lib.trivia)
  :components ((:file "package")
			   (:file "utilities")
			   (:file "requests")))