;;;; $Id: http.lisp,v 1.6 2008/05/22 18:57:15 xach Exp $

(in-package #:zpb-http)

(defparameter *crlf* (octet-vector 13 10))
(defparameter *end-of-headers* (octet-vector 13 10 13 10))
(defparameter *degenerate-end-of-headers* (octet-vector 10 10))
(defparameter *http-header-size-limit* 50000)

(defun dump-buffer (buffer)
  (with-open-file (stream #p"/tmp/debug.buf.out"
			  :element-type '(unsigned-byte 8)
			  :direction :output
			  :if-exists :supersede)
    (write-sequence buffer stream)))

(defun read-http-header (socket)
  (let ((buffers '())
        (start 0)
        (byte-count 0)
        (magic (ascii "HTTP/1."))
        (eoh-matcher (make-matcher *end-of-headers*))
        (eoh-0a-matcher (make-matcher *degenerate-end-of-headers*))
        (buffer (make-socket-buffer)))
    (loop
     (multiple-value-bind (buf end)
         (read-octets socket :buffer buffer :start start)
       (declare (ignore buf))
       (incf byte-count (- end start))
       (when (= end start)
	 (dump-buffer buffer)
         (error "End of data when looking for header (start ~D end ~D byte-count ~D buffer-len ~D)"
		start end
		byte-count
		(length buffers)))
       (when (> byte-count *http-header-size-limit*)
         (error "Header not seen in first ~D bytes of input" byte-count))
       (when (and magic (> byte-count (length magic)))
         (if (magic-match buffer magic)
             (setf magic nil)
             (error 'bad-http-magic :input buffer :position 0)))
       (let ((end-of-headers (or (match eoh-matcher buffer :start start :end end)
                                 (match eoh-0a-matcher buffer :start start :end end)))
             (eoh-size 4)
             (eol-size 2))
         (when (matchedp eoh-0a-matcher)
           ;; The header includes naked #x0A line-endings
           (setf eol-size 1
                 eoh-size 2))
         (when end-of-headers
           (let* ((content-byte-count (- end end-of-headers eoh-size))
                  (content (make-socket-buffer content-byte-count))
                  (header-byte-count (- (- byte-count content-byte-count)
                                        eol-size))
                  (headers (make-socket-buffer header-byte-count))
                  (i 0))
             (dolist (buffer (nreverse buffers))
               (replace headers buffer :start1 i)
               (incf i (length buffer)))
             (replace headers buffer :start1 i)
             (replace content buffer :start2 (+ end-of-headers eoh-size)
                      :end2 end)
                      (return (values headers content)))))
         (setf start end)
         (when (<= (length buffer) start)
           (setf start 0)
           (push buffer buffers)
           (setf buffer (make-socket-buffer)))))))

(defclass parsed-header ()
  ((data
    :initarg :data
    :accessor data)
   (status
    :initarg :status
    :accessor status)
   (name-index
    :initarg :name-index
    :accessor name-index)
   (value-index
    :initarg :value-index
    :accessor value-index)))

(defun header-status (header)
  (when (and (> (length header) (length "HTTP/1.1 XXX"))
             (= (aref header 8) 32))
    (let ((code (sb-ext:octets-to-string (subseq header 9 12)
                                         :external-format :ascii)))
      (parse-integer code))))
    

(defun parse-header (header)
  (let ((state :init)
        (i 0)
        (input nil)
        (name nil)
        (value nil)
        (names (make-array 5 :adjustable t :fill-pointer 0))
        (values (make-array 5 :adjustable t :fill-pointer 0)))
    (loop
     (when (<= (length header) i)
       (when value
         (vector-push-extend (cons value (- i 2)) values))
       (return (make-instance 'parsed-header
                              :data header
                              :status (header-status header)
                              :name-index names
                              :value-index values)))
     (setf input (aref header i))
     (case state
       (:init
        (case input
          (10 (setf state :after-crlf))
          (13 (setf state :after-cr))))
       (:after-cr
         (case input
           (10 (setf state :after-crlf))
           (t (error "Bad header input: naked CR at ~D" i))))
       (:after-crlf
         (case input
           ((9 32)
            (setf state :in-value))
           (t
            (setf state :in-name)
            (when value
              (vector-push-extend (cons value (- i 2)) values)
              (setf value nil))
            (setf name i))))
       (:in-value
         (case input
           (10 (setf state :after-crlf))
           (13 (setf state :after-cr))))
       (:in-name
         (case input
           (58
            (vector-push-extend (cons name i) names)
            (setf name nil)
            (setf state :after-colon))))
       (:after-colon
         (case input
           ((9 32) :after-colon)
           (t
            (setf state :in-value)
            (setf value i)))))
     (incf i))))

(defun header-value (name parsed-header)
  (declare (optimize (speed 0) (safety 3) (debug 3)))
  (let ((name (ascii name)))
    (with-slots (data name-index value-index)
        parsed-header
      (loop for (start . end) across name-index
            for i from 0
            when (and (= (- end start)
                         (length name))
                      (= (mismatch data name
                                   :start1 start
                                   :test 'ascii-code-equal)
                         end))
            return (destructuring-bind (start . end)
                       (aref value-index i)
                     (sb-ext:octets-to-string (subseq data start end)
                                              :external-format :ascii))))))
(defvar *crlf*
  (make-array 2 :element-type 'octet
              :initial-contents '(13 10)))

(defun make-request-buffer (&rest vectors)
  (let* ((total-size (loop for v in vectors summing (length v)))
         (output-buffer (make-array total-size :element-type 'octet))
         (pos 0))
    (dolist (v vectors output-buffer)
      (let ((v2 (etypecase v
                  (string (ascii v))
                  ((or vector null) v)))) 
        (replace output-buffer v2 :start1 pos)
        (incf pos (length v2))))))

