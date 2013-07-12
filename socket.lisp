;;;; $Id: socket.lisp,v 1.10 2006/05/10 19:14:02 xach Exp $

(in-package #:zpb-http)

(sb-bsd-sockets::define-socket-condition
    sb-posix:einprogress
    in-progress-error)

(unexport 'in-progress-error)

(sb-bsd-sockets::define-socket-option-int
  sockopt-error sockint::sol-socket sockint::so-error)

(unexport 'sockopt-error)

(defconstant +shutdown-read+ 0)
(defconstant +shutdown-write+ 1)
(defconstant +shutdown-readwrite+ 2)

(sb-alien:define-alien-routine ("shutdown" socket-shutdown) integer
  (socket integer)
  (how integer))

(sb-alien:define-alien-routine ("send" socket-send) integer
  (socket integer)
  (buffer (* t))
  (length integer)
  (flags integer))

(defmacro with-sane-socket-errors (&body body)
  `(handler-case
       (progn ,@body)
     (sb-bsd-sockets:socket-error (c)
       (unix-error:signal (sb-bsd-sockets::socket-error-errno c)
                          (sb-bsd-sockets::socket-error-syscall c)))))

(defclass ip-address ()
  ((address
    :initarg :address
    :reader address)))

(defgeneric dotted-quad-string (object)
  (:method (object)
    (let ((address (address object)))
      (format nil "~D.~D.~D.~D"
              (aref address 0)
              (aref address 1)
              (aref address 2)
              (aref address 3)))))

(defmethod print-object ((ip-address ip-address) stream)
  (print-unreadable-object (ip-address stream :type t)
    (write-string (dotted-quad-string ip-address) stream)))

(defun parse-dotted-quad (string)
  (let ((pos 0))
    (flet ((parse-octet ()
             (unless (< pos (length string))
               (error "End of string when parsing as IP address"))
             (unless (digit-char-p (char string pos))
               (error "Invalid IP address character in string at ~D" pos))
             (let ((end (position-if-not #'digit-char-p string :start pos)))
               (if end
                   (when (char/= (char string end) #\.)
                     (error "Invalid character at ~D" end))
                   (setf end (length string)))
               (let ((integer (parse-integer string :start pos :end end)))
                 (setf pos (1+ end))
                 (if (<= 0 integer 255)
                     integer
                     (error "Integer ~D out of range" integer))))))
      (let ((result (vector (parse-octet)
                            (parse-octet)
                            (parse-octet)
                            (parse-octet))))
        (if (>= pos (length string))
            result
            (error "Junk after IP address in string (/= ~D ~D)" pos (length string)))))))

(defun read-potential-address (stream)
  (with-output-to-string (output)
    (loop
     (let ((char (read-char stream)))
       (case char
         (#\]
            (return))
         ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.)
          (write-char char output))
         (t (error "Invalid character ~C in address" char)))))))


(defun ip-integer-vector (integer)
  (make-array 4
              :element-type 'octet
              :initial-contents (list (ldb (byte 8 24) integer)
                                      (ldb (byte 8 16) integer)
                                      (ldb (byte 8  8) integer)
                                      (ldb (byte 8  0) integer))))


(defun ip-address (address)
  (let ((representation (etypecase address
                          (string (parse-dotted-quad address))
                          (integer (ip-integer-vector address))
                          (dns:address (dns:data address))
                          (vector address))))
    (make-instance 'ip-address
                   :address representation)))

(defun \#\[-reader (stream character arg)
  (declare (ignore character arg))
  (let ((address (read-potential-address stream)))
    (ip-address (parse-dotted-quad address))))

(defclass socket-address ()
  ((address
    :initarg :address
    :accessor address)
   (port
    :initarg :port
    :accessor port)))

(defmethod print-object ((socket-address socket-address) stream)
  (print-unreadable-object (socket-address stream :type t)
    (format stream "~A:~D"
            (dotted-quad-string (address socket-address))
            (port socket-address))))


(defclass socket (epoll:epollable)
  ((system-socket
    :initarg :system-socket
    :accessor system-socket)
   (local-name
    :initarg :local-name
    :writer (setf local-name)
    :reader %local-name)
   (peer-name
    :reader %peer-name
    :writer (setf peer-name))))

(defmethod print-object ((socket socket) stream)
  (print-unreadable-object (socket stream :type t)
    (format stream "local ~A peer ~A"
            (local-name socket)
            (peer-name socket))))


(defgeneric file-descriptor (socket)
  (:method (socket)
    (sb-bsd-sockets:socket-file-descriptor (system-socket socket))))

(defmethod epoll:file-descriptor ((socket socket))
  (file-descriptor socket))

(defun set-nonblocking (fd)
  (sb-posix:fcntl fd sb-posix:f-setfl sb-posix:o-nonblock))

(defun make-socket ()
  (let* ((system-socket (make-instance 'sb-bsd-sockets:inet-socket
                                       :type :stream)))
    (make-instance 'socket
                   :system-socket system-socket)))

(defun make-udp-socket ()
  (make-instance 'socket
                 :system-socket (make-instance 'sb-bsd-sockets:inet-socket
                                               :type :datagram
                                               :protocol :udp)))


(defun connect (socket address port &key (blocking t))
  (unless blocking
    (set-nonblocking (file-descriptor socket)))
  (let ((system-address (etypecase address
                          (ip-address (address address))
                          (string (parse-dotted-quad address))
                          (vector address))))
    (with-sane-socket-errors 
      (handler-case 
          (sb-bsd-sockets:socket-connect (system-socket socket)
                                         system-address port)
        (in-progress-error (c)
          (when blocking
            (error c))))
      (setf (peer-name socket)
            (make-instance 'socket-address
                           :address (ip-address system-address)
                           :port port))
      socket)))

(defun disconnect (socket)
  (ignore-errors (sb-bsd-sockets:socket-close (system-socket socket)))
  (values))

(defun accept (socket)
  (let ((new-socket (sb-bsd-sockets:socket-accept (system-socket socket))))
    (when new-socket
      (make-instance 'socket
                     :system-socket new-socket))))

(defun bind (socket &key address port (reuse t))
  (setf address (or address (ip-address "0.0.0.0")))
  (with-sane-socket-errors
    (sb-bsd-sockets:socket-bind (system-socket socket) (address address) port)
    (setf (sb-bsd-sockets:sockopt-reuse-address (system-socket socket)) reuse))
  (values))

(defun listen (socket &optional (backlog 5))
  (with-sane-socket-errors
    (sb-bsd-sockets:socket-listen (system-socket socket) backlog)))

#+nil
(defun read-ready (socket &optional (timeout 0))
  (let ((fd (file-descriptor socket)))
    (multiple-value-bind (result readable)
        (sb-unix:unix-select (1+ fd) (ash 1 fd) 0 0 timeout 0)
      (declare (ignore result))
      (plusp readable))))

(defun read-ready (socket &optional timeout)
  (epoll::fd-readable (file-descriptor socket) timeout))

(defun write-ready (socket &optional timeout)
  (epoll::fd-writable (file-descriptor socket) timeout))

(defun urgent-ready (socket &optional timeout)
  (epoll::fd-urgent-readable (file-descriptor socket) timeout))

(defparameter *default-buffer-size* 8192)

(defun make-socket-buffer (&optional (size *default-buffer-size*))
  (make-array size :element-type 'octet))

(defun connection-error (socket)
  (sockopt-error (system-socket socket)))

(defmethod read-octets (socket &key buffer (start 0) end)
  (let* ((fd (file-descriptor socket))
         (buf (or buffer (make-socket-buffer)))
         (end (or end (length buf)))
         (length (- end start)))
    (finish-output)
    (sb-sys:with-pinned-objects (buf)
      (multiple-value-bind (octets-read errno)
          (sb-unix:unix-read fd (sb-sys:sap+ (sb-sys:vector-sap buf)
                                             start)
                             length)
        (if octets-read
            (if (and (not buffer) (< octets-read (length buf)))
                (values (subseq buf 0 octets-read) octets-read)
                (values buf octets-read))
            (cond ((= errno unix-error:ewouldblock)
                   (error "Would block"))
                  (t (error "Unknown Unix error ~D" errno))))))))

(cl:defconstant MSG-NOSIGNAL 16384 "NIL")
(cl:defconstant MSG-OOB 1 "NIL")
(cl:defconstant MSG-PEEK 2 "NIL")
(cl:defconstant MSG-TRUNC 32 "NIL")
(cl:defconstant MSG-WAITALL 256 "NIL")
(cl:defconstant MSG-EOR 128 "NIL")
(cl:defconstant MSG-DONTROUTE 4 "NIL")
(cl:defconstant MSG-DONTWAIT 64 "NIL")
(cl:defconstant MSG-NOSIGNAL 16384 "NIL")
(cl:defconstant MSG-CONFIRM 2048 "NIL")
(cl:defconstant MSG-MORE 32768 "NIL")

(defmacro integer-flags (&rest expressions)
  `(logand #xFFFFFFFF
    (logior ,@(loop for ((expression value)) on expressions
                    collect `(if ,expression ,value 0)))))

(defmethod write-octets (socket buffer &key
                         (start 0) end
                         (route t) (oob nil) (eor nil)
                         (wait t) (signal nil))
  (let* ((end (or end (length buffer)))
         (length (- end start)))
    (sb-sys:with-pinned-objects (buffer)
      (let ((result
             (socket-send (file-descriptor socket)
                          (sb-sys:sap+ (sb-sys:vector-sap buffer)
                                       start)
                          length
                          (integer-flags (oob msg-oob)
                                         (eor msg-eor)
                                         ((not route) msg-dontroute)
                                         ((not wait) msg-dontwait)
                                         ((not signal) msg-nosignal)))))
        (when (minusp result)
          (unix-error:signal (unix-error:errno)
                             "send()"))))))

(defmethod socket-address (address port)
  (let ((socket-address (make-array 8 :element-type 'octet)))
    (setf (aref socket-address 0) 2
          (aref socket-address 1) 0
          (aref socket-address 2) (ldb (byte 8 8) port)
          (aref socket-address 3) (ldb (byte 8 0) port))
    (replace socket-address (address address) :start1 4)))
    
(defmethod send-octets (socket buffer address port &key (start 0) end)
  (let* ((end (or end (length buffer)))
         (length (- end start))
         (socket-address (socket-address address port)))
    (sb-sys:with-pinned-objects (buffer socket-address)
      (let ((result 
             (sockint::sendto (file-descriptor socket) 
                              (sb-sys:sap+ (sb-sys:vector-sap buffer)
                                           start)
                              length
                              0
                              (sb-sys:vector-sap socket-address)
                              16)))
        (if (plusp result)
            result
            (error "sendto() system call error: ~D" (sb-alien:get-errno)))))))



              
(defmethod local-name (socket)
  (if (slot-boundp socket 'local-name)
      (%local-name socket)
      (ignore-errors
        (multiple-value-bind (address port)
            (sb-bsd-sockets:socket-name (system-socket socket))
          (setf (local-name socket)
                (make-instance 'socket-address
                               :address (ip-address address)
                               :port port))))))

(defmethod peer-name (socket)
  (if (slot-boundp socket 'peer-name)
      (%peer-name socket)
      (ignore-errors
        (multiple-value-bind (address port)
            (sb-bsd-sockets:socket-peername (system-socket socket))
          (setf (peer-name socket)
                (make-instance 'socket-address
                               :address (ip-address address)
                               :port port))))))

(defmethod shutdown (socket type)
  (loop
   (let ((result (socket-shutdown (file-descriptor socket) type)))
     (cond ((zerop result)
            (return t))
           ((/= (unix-error:errno) unix-error:eintr)
            (unix-error:signal (unix-error:errno) "shutdown"))))))

(defmethod shutdown-read (socket)
  (shutdown socket +shutdown-read+))

(defmethod shutdown-write (socket)
  (shutdown socket +shutdown-write+))

