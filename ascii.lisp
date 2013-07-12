;;;; $Id: ascii.lisp,v 1.3 2006/05/10 19:25:11 xach Exp $

(in-package #:zpb-http)

(defun ascii-code-equal (a b)
  (= (+ a (if (<= 65 a 90) #x20 0))
     (+ b (if (<= 65 b 90) #x20 0))))

(defun ascii (string)
  (let ((result (make-array (length string) :element-type 'octet)))
    (loop for char across string
          for code = (char-code char)
          for i from 0
          if (< code 128)
          do (setf (aref result i) code)
          else do (error "Character at position ~D is not in ASCII range" i))
    result))

(define-compiler-macro ascii (&whole form arg)
  (if (stringp arg)
      (ascii arg)
      form))

(defun ascii-string (ascii-codes)
  (sb-ext:octets-to-string ascii-codes :external-format :ascii))
