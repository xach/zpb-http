;;;; $Id: resolver.lisp,v 1.7 2006/05/10 19:26:49 xach Exp $

(in-package #:zpb-http)

(defvar *default-resolv-conf* #p"/etc/resolv.conf")
(defvar *default-resolver-timeout* 5000)
(defvar *default-resolver-retries* 2)

(defun parse-nameserver-line (line)
  (when line
    (let ((trimmed (string-trim '(#\Space #\Tab) line)))
      (when (and (> (length trimmed) (length "nameserver"))
                 (string-equal trimmed "nameserver"
                               :end1 (length "nameserver")))
        (let* ((start (1+ (length "nameserver")))
               (end (position-if-not (lambda (c) (find c "0123456789."))
                                     trimmed
                                     :start start)))
          (subseq trimmed start end))))))

(defun resolv-conf-nameservers (&optional (file *default-resolv-conf*))
  (with-open-file (stream file
                   :direction :input
                   :external-format :ascii)
    (loop for line = (read-line stream nil)
          for nameserver-string = (parse-nameserver-line line)
          while line
          if nameserver-string collect (ip-address nameserver-string))))

(eval-when (:load-toplevel :execute)
  (defparameter *name-servers* (coerce (resolv-conf-nameservers) 'vector)))

(defun random-name-server ()
  (aref *name-servers* (random (length *name-servers*))))

(defun resource-records (query response)
  (let* ((question (aref (dns:question-records query) 0))
         (target (dns:name question))
         answer i results)
    (tagbody
     restart
       (setf i 0)
     loop
       (when (>= i (dns:answer-count response))
         (go end))
       (setf answer (aref (dns:answer-records response) i))
       (when (dns:labels-equal target (dns:name answer))
         (when (typep answer 'dns:canonical-name)
           (setf target (dns:canonical-name answer))
           (go restart))
         (push answer results))
       (incf i)
       (go loop)
     end)
    results))

(defun dns-query (name &key (type :a) (timeout *default-resolver-timeout*)
		  (retry *default-resolver-retries*))
  (let ((query (dns:make-query name :type type))
        (socket (make-udp-socket))
        (response-vector (make-array 512 :element-type 'octet)))
    (unwind-protect
         (block nil
           (dotimes (i retry nil)
             (send-octets socket (dns:message-vector query)
                          (random-name-server)
                          53)
             (let ((readable (read-ready socket timeout)))
               (cond (readable
                      (read-octets socket :buffer response-vector)
                      (let ((response (dns:get-message response-vector)))
                        (when (dns:query-response-p query response)
                          (return (resource-records query response)))))
                     (t (error "Query timeout"))))))
      (disconnect socket))))

(defun host-addresses (name &optional
		       (timeout *default-resolver-timeout*)
		       (retry *default-resolver-retries*))
  (let ((addresses (dns-query name
                              :type :a
                              :timeout timeout
                              :retry retry)))
    (mapcar #'ip-address addresses)))



(defun one-hostname-address (hostname)
  (let ((ip (ignore-errors (ip-address hostname))))
    (if ip
        ip
        (let ((addresses (host-addresses hostname)))
          (if (null addresses)
              (error "Hostname lookup failed")
              (first addresses))))))




#|
   (defun get-host-entry-in-thread (name)
     (let* ((mutex (sb-thread:make-mutex))
            (queue (sb-thread:make-waitqueue))
            result error)
       (sb-thread:make-thread
        (lambda ()
          (block nil
            (sb-thread:with-mutex (mutex)
              (handler-case
                  (multiple-value-setq (result error)
                    (ignore-errors (sb-bsd-sockets:get-host-by-name name)))
                (sb-bsd-sockets:host-not-found-error (e)
                  (setf error e)
                  (return)))))
          (sb-thread:condition-notify queue)))
       (sb-thread:with-mutex (mutex)
         (loop
          (when (or result error)
            (return))
          (sb-thread:condition-wait queue mutex)))
       (if error
           (error error)
           result)))

   (defun hostname-addresses (hostname)
     (let ((entry (get-host-entry-in-thread hostname)))
       (map 'vector
            (lambda (address)
              (make-instance 'ip-address
                             :address address))
            (sb-bsd-sockets:host-ent-addresses entry))))

   (defun one-hostname-address (hostname)
     (let ((addresses (hostname-addresses hostname)))
       (if (plusp (length addresses))
           (aref addresses (random (length addresses)))
           (error "No addresses associated with ~A" hostname))))
|#