;;;; +----------------------------------------------------------------+
;;;; | Google Cloud Messaging (GCM)                                   |
;;;; +----------------------------------------------------------------+

;;; System definition

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:gcm
  :description "Google Cloud Messaging (GCM) library for Common Lisp"
  :author "death <github.com/death>"
  :license "MIT"
  :depends-on (#:drakma #:com.gigamonkeys.json #:babel)
  :components
  ((:file "gcm")))
