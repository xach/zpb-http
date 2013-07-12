;;;; $Id: match.lisp,v 1.3 2008/05/22 19:08:29 xach Exp $

(in-package #:zpb-http)

(defclass matcher ()
  ((pattern
    :initarg :pattern
    :reader pattern)
   (pos
    :initform 0
    :accessor match-pos)
   (matchedp
    :initform nil
    :accessor matchedp)))

(defun reset-match (matcher)
  (setf (match-pos matcher) 0
        (matchedp matcher) nil))
  
(define-condition match-failure (error) ())

(defun match (matcher input &key (start 0) end error)
  (let ((i start)
        (end (or end (length input)))
        (match-end (length (pattern matcher))))
    (with-slots (pattern pos)
        matcher
      (loop
       (cond ((= pos match-end)
              (let ((match-start (- i pos)))
                (setf pos 0)
                (setf (matchedp matcher) t)
                (return match-start)))
             ((= i end)
              (return nil))
             ((= (aref pattern pos)
                 (aref input i))
              (incf i)
              (incf pos))
             (t
              (if error
                  (error 'match-failure)
                  (setf pos 0
                        i (1+ i)))))))))

(defun make-matcher (pattern)
  (make-instance 'matcher
                 :pattern pattern))

(defun magic-match (input magic)
  (= (mismatch magic input)
     (length magic)))
