;;;; $Id: package.lisp,v 1.3 2006/12/01 19:30:56 xach Exp $

(defpackage #:zpb-http
  (:use :cl)
  (:export :snarf :*user-agent*)
  (:shadow :listen))

