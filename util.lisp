;;;; $Id: util.lisp,v 1.1 2006/05/10 19:14:20 xach Exp $

(in-package #:zpb-http)

(defun octet-vector (&rest contents)
  (make-array (length contents)
              :element-type 'octet
              :initial-contents contents))
