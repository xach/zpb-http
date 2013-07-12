;;;; $Id: conditions.lisp,v 1.2 2006/05/10 19:24:04 xach Exp $

(in-package #:zpb-http)

(define-condition syntax-error (error)
  ((input
    :initarg :input
    :accessor input)
   (pos
    :initarg :pos
    :accessor pos)
   (message
    :initarg :message
    :accessor message)))
    

(define-condition url-syntax-error (syntax-error) ()
  (:report
   (lambda (c s)
     (format s "Invalid URL syntax at position ~D of ~S: ~A"
             (pos c)
             (input c)
             (message c)))))
           
(define-condition dotted-quad-syntax-error (syntax-error) ())

(define-condition connection-error (error)
  ((remote-ip
    :initarg :remote-ip
    :accessor remote-ip)
   (remote-port
    :initarg :remote-port
    :accessor remote-port)))

(define-condition connection-refused (connection-error) ())
(define-condition connection-timed-out (connection-error) ())

(define-condition protocol-error (error)
  ((input
    :initarg :input
    :accessor input)
   (pos
    :initarg :pos
    :accessor pos)))

(define-condition bad-http-magic (protocol-error) ())
(define-condition header-parse-error (protocol-error) ())
