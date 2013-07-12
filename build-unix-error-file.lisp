;;; $Id: build-unix-error-file.lisp,v 1.1 2006/04/27 21:01:16 xach Exp $ 

(defpackage :build-unix-errors
  (:use :cl)
  (:shadow :number))

(in-package :build-unix-errors)

(defclass unix-error-number ()
  ((name
    :initarg :name
    :accessor name)
   (number
    :initarg :number
    :accessor number)
   (description
    :initarg :description
    :accessor description)))

(defun extract-description (line)
  (let* ((start (search "/* " line))
         (end (search " */" line :start2 start)))
    (subseq line (+ start 3) end)))

(defun parse-error-header-line (line)
  (let ((*readtable* (copy-readtable))
        (*read-eval* nil))
    (set-syntax-from-char #\# #\a)
    (ignore-errors
      (with-input-from-string (stream line)
        (let ((define (read stream))
              (name (read stream))
              (number-or-alias (read stream))
              (description-comment (read-line stream)))
          (make-instance 'unix-error-number
                         :name name
                         :number number-or-alias
                         :description (extract-description description-comment)))))))

    

(defun read-error-header-file (file)
  (with-open-file (stream file :direction :input)
    (loop for line = (read-line stream nil)
          while line
          if (parse-error-header-line line)
          collect it)))

(defmethod print-object ((object unix-error-number) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~A ~A (~A)"
            (name object)
            (number object)
            (description object))))

(defun normalize-numbers (error-list)
  (let ((table (make-hash-table)))
    (dolist (error error-list)
      (if (symbolp (number error))
          (setf (number error) (gethash (number error) table))
          (setf (gethash (name error) table) (number error))))
    error-list))


(defun output-constants (error-list stream)
  (dolist (error error-list)
    (format stream "(defconstant ~A ~D ~S)~%"
            (name error)
            (number error)
            (description error))))

(defun output-conditions (error-list stream)
  (pprint `(define-condition unix-error (error)
             ((name
               :initarg :name
               :accessor name)
              (number
               :initarg :number
               :accessor unix-error-number)
              (description
               :initarg :description
               :accessor unix-error-description)))
          stream)
  (terpri stream)
  (dolist (e error-list)
    (pprint `(define-condition ,(name e) (unix-error)
               ((name :initform ',(name e))
                (number :initform ,(number e))
                (description :initform ,(description e))))
            stream)
    (terpri stream)))

(defun error-vector (error-list)
  (let ((vector (make-array 128 :initial-element nil)))
    (loop for e in error-list
          for number = (number e)
          when (null (aref vector number))
          do (setf (aref vector number) e))
    vector))

(defun output-names (error-vector stream)
  (flet ((output (e)
           (if e
               (format stream "~4T~A ~24T; ~A~%"
                       (name e) (description e))
               (format stream "~4TNIL~%"))))
    (format stream "(defvar *error-names*~%~
                  ~0T #(~%")
    (loop for e across error-vector
          do (output e))
    (format stream "    ))~%")))

(defun output-descriptions (error-vector stream)
  (flet ((output (e)
           (if e
               (format stream "~4T~S ~56T; ~A~%"
                       (description e) (name e))
               (format stream "~4TNIL~%"))))
    (format stream "(defvar *error-descriptions*~%~
                  ~0T #(~%")
    (loop for e across error-vector
          do (output e))
    (format stream "    ))~%")))


(defun make-error-file (header-file output-file)
  (let* ((errors (normalize-numbers (read-error-header-file header-file)))
         (errors-vector (error-vector errors))
         (*print-case* :downcase))
    (with-open-file (stream output-file :direction :output :if-exists :supersede)
      (format stream ";;; Auto-generated from unix-errors.lisp~%~%")
      (pprint `(defpackage :unix-error
                 (:use :cl)
                 (:export #:unix-error-number #:unix-error-description)
                 (:export ,@(mapcar (lambda (e)
                                      (make-symbol (symbol-name (name e))))
                                    errors)))
              stream)
      (terpri stream)
      (pprint `(in-package :unix-error)
              stream)
      (terpri stream)
      (output-constants errors stream)
      (terpri stream)
      (output-conditions errors stream)
      (terpri stream)
      (output-names errors-vector stream)
      (terpri stream)
      (output-descriptions errors-vector stream)
      (terpri stream)
      (truename stream))))