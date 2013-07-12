;; epoll.lisp

(defpackage :epoll
  (:use :cl :sb-alien)
  (:export :epoll-create :epoll-control :epoll-wait
           :open-controller :close-controller :control
           :add-watch :delete-watch :modify-watch
           :flags :data1 :data2
           :file-descriptor
           :epollable
           :pending-events))

(in-package :epoll)

(defconstant +epoll-add+ 1)
(defconstant +epoll-delete+ 2)
(defconstant +epoll-modify+ 3)

(defconstant +epoll-in+  #b00001)
(defconstant +epoll-pri+ #b00010)
(defconstant +epoll-out+ #b00100)
(defconstant +epoll-msg+ #b01000)
(defconstant +epoll-hup+ #b10000)

(defconstant +epoll-oneshot+ (ash 1 30))
(defconstant +epoll-edge+    (ash 1 31))

(define-alien-routine ("epoll_create" epoll-create) integer (size integer))

(define-alien-routine ("epoll_ctl" epoll-control) integer
  (epoll-fd integer)
  (operation integer)
  (fd integer)
  (epoll-event (* t)))

(define-alien-routine ("epoll_wait" epoll-wait) integer
  (epoll-fd integer)
  (epoll-events (* t))
  (max-events integer)
  (timeout-ms integer))

(defun test-epoll (fd)
  (let ((epoll-fd nil)
        (event (make-array 3
                           :element-type '(unsigned-byte 32)
                           :initial-element 0)))
    (unwind-protect
         (progn
           (setf epoll-fd (epoll-create 1))
           (setf (aref event 0) +epoll-in+)
           (setf (aref event 1) fd)
           (when (minusp (epoll-control epoll-fd
                                        +epoll-add+
                                        fd
                                        (sb-sys:vector-sap event)))
             (error "epoll-ctl failed"))
           (sb-sys:with-pinned-objects (event)
             (let ((available (epoll-wait epoll-fd
                                          (sb-sys:vector-sap event)
                                          1
                                          -1)))
               (format t "Available: ~D~%" available)
               (format t "XXX ~A~%" event))))
      (when (and epoll-fd (plusp epoll-fd))
        (sb-posix:close epoll-fd)))))


(defun make-event ()
  (make-array 3 :element-type '(unsigned-byte 32)))

(defun set-event-mask (event &rest options)
  (let ((mask 0))
    (dolist (option options)
      (setf mask (logior mask option)))
    (setf (aref event 0) mask)))

(defmacro with-epoll-fd ((fd size) &body body)
  `(let (,fd)
    (unwind-protect
         (progn
           (setf ,fd (epoll-create ,size))
           ,@body)
      (when (and ,fd (plusp ,fd))
        (sb-posix:close ,fd)))))

(defun epoll-status (fd)
  (let ((event (make-event)))
    (set-event-mask event +epoll-in+ +epoll-out+ +epoll-pri+)
    (with-epoll-fd (efd 1)
      (sb-sys:with-pinned-objects (event)
        (epoll-control efd +epoll-add+ fd (sb-sys:vector-sap event))
        (epoll-wait efd (sb-sys:vector-sap event) 1 0))
      (let ((events (aref event 0))
            (result '()))
        (when (logtest events +epoll-in+)
          (push :readable result))
        (when (logtest events +epoll-out+)
          (push :writable result))
        (when (logtest events +epoll-pri+)
          (push :urgent result))
        (values result events)))))

(defmacro epoll-call (form)
  (let ((result (gensym)))
    `(let ((,result ,form))
      (if (minusp ,result)
          (unix-error:signal (unix-error:errno) ',(car form))
          ,result))))

(defun fd-status (fd status timeout)
  (let ((event (make-event)))
    (set-event-mask event status)
    (setf (aref event 1) fd)
    (with-epoll-fd (efd 1)
      (sb-sys:with-pinned-objects (event)
        (epoll-call (epoll-control efd +epoll-add+
                                   fd (sb-sys:vector-sap event)))
        (fill event 0)
        (loop
         (let ((result (epoll-wait efd (sb-sys:vector-sap event)
                                   1 timeout)))
           (cond ((and (minusp result)
                       (/= (unix-error:errno)
                           unix-error:eintr))
                  (unix-error:signal (unix-error:errno) "epoll-wait"))
                 ((>= result 0)
                  (return))))))
      (and (logtest status (aref event 0))
           (= (aref event 1) fd)))))

(defun fd-readable (fd &optional timeout)
  (fd-status fd +epoll-in+ (or timeout -1)))

(defun fd-writable (fd &optional timeout)
  (fd-status fd +epoll-out+ (or timeout -1)))

(defun fd-urgent-readable (fd &optional timeout)
  (fd-status fd +epoll-pri+ (or timeout -1)))
        
                          
        
    



;;; Testing, 1 2 3

(defvar *pending-count* 256)

(defgeneric file-descriptor (object)
  (:documentation "Return the Unix file descriptor for OBJECT.")
  (:method ((object integer))
    object))

(defclass epollable ()
  ((file-descriptor
    :initarg :file-descriptor
    :accessor file-descriptor)))

(defun make-events-vector (event-count)
  (make-array (* event-count 3) :element-type '(unsigned-byte 32)))

(defclass controller (epollable)
  ((file-descriptor
    :initarg :file-descriptor
    :accessor file-descriptor)
   (pending
    :initform (make-events-vector *pending-count*)
    :reader pending)))

(defclass event ()
  ((data
    :initarg :data
    :accessor data)))

(defmethod flags ((event event))
  (aref (data event) 0))

(defmethod data1 ((event event))
  (aref (data event) 1))

(defmethod data2 ((event event))
  (aref (data event) 2))

(defun open-controller (&optional (backing-store-size 256))
  (let ((fd (epoll-create backing-store-size)))
    (if (plusp fd)
        (let ((controller (make-instance 'controller
                                         :file-descriptor fd)))
          (sb-ext:finalize controller
                           (lambda ()
                             (sb-posix:close fd)))
          controller)
        (unix-error:signal (unix-error:errno) "epoll_create"))))

(defun close-controller (controller)
  (sb-ext:cancel-finalization controller)
  (sb-posix:close (file-descriptor controller)))

(defun event-set (&key read write urgent one-shot edge-triggered)
  (flet ((flag (flag value)
           (if flag value 0)))
    (logior (flag read     +epoll-in+)
            (flag urgent   +epoll-pri+)
            (flag write    +epoll-out+)
            (flag one-shot +epoll-oneshot+)
            (flag edge-triggered +epoll-edge+))))

(defun control (controller operation fd
                &key read write urgent one-shot edge-triggered data1 data2)
  (let ((event (make-event)))
    (setf (aref event 0) (event-set :read read :write write :urgent urgent
                                    :one-shot one-shot
                                    :edge-triggered edge-triggered)
          (aref event 1) data1
          (aref event 2) data2)
    (sb-sys:with-pinned-objects (event)
      (if (minusp (epoll-control (file-descriptor controller)
                                 operation
                                 (file-descriptor fd)
                                 (sb-sys:vector-sap event)))
          (unix-error:signal (unix-error:errno) "epoll_ctl")
          t))))

(macrolet ((define-watch (name constant)
             `(defun ,name (controller fd
                                &key read write urgent one-shot edge-triggered
                                (data1 fd) (data2 0))
                (control controller ,constant fd
                         :read read :write write :urgent urgent
                         :one-shot one-shot
                         :edge-triggered edge-triggered
                         :data1 data1 :data2 data2))))
  (define-watch add-watch +epoll-add+)
  (define-watch modify-watch +epoll-modify+)
  (define-watch delete-watch +epoll-delete+))

(defun event-ref (event-vector i)
  (make-instance 'event
                 :data (subseq event-vector (* i 3) (+ (* i 3) 3))))

(defun %epoll-wait (epoll-fd events count timeout)
  (loop
   (sb-sys:with-pinned-objects (events)
     (let ((event-count (epoll-wait epoll-fd
                                    (sb-sys:vector-sap events)
                                    count timeout)))
       (if (minusp event-count)
           (let ((errno (unix-error:errno)))
             (when (/= errno unix-error:EINTR)
               (unix-error:signal errno "epoll_wait")))
           (return event-count))))))

(defun pending-events (controller &optional events (timeout -1))
  (let ((fd (file-descriptor controller))
        (pending (pending controller))
        (pending-count (if events
                           (min (length events) *pending-count*)
                           *pending-count*)))
    (fill pending 0)
    (let ((event-count (%epoll-wait fd pending pending-count timeout)))
      (when (plusp event-count)
        (let ((events (or events (make-array event-count))))
          (loop for i below event-count
                do
                (setf (aref events i) (event-ref pending i)))
          (values events event-count))))))
            
    

