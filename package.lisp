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

(defpackage "ZANOX-CLIENT"
  (:shadowing-import-from "COMMON-LISP" "NULL")
  (:use "COMMON-LISP" "IRONCLAD" "DRAKMA" "BABEL" "CL-BASE64" "CXML" 
        "SPLIT-SEQUENCE" "CL-PPCRE" "DARTS.LIB.TRIVIA")
  (:export "CLIENT" "MAKE-CLIENT" "WITH-CLIENT" "*CLIENT*" "SEND-REQUEST"))
