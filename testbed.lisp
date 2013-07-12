(in-package :zpb-http)


;;; test telnet server

(defconstant +iac+ 255)
(defconstant +se+ 240)
(defconstant +nop+ 242)
(defconstant +sb+ 250)
(defconstant +will+ 251)
(defconstant +wont+ 252)
(defconstant +do+ 253)
(defconstant +dont+ 254)
(defconstant +is+ 0)
(defconstant +send+ 1)

(defconstant +terminal-type+ 24)
(defconstant +naws+ 31)

(defun send-dump (socket octets)
  (when (write-ready socket 5000)
    (format t ">> ~A~%" octets)
    (write-octets socket octets)
    (when (read-ready socket 5000)
      (format t "<< ~A~%" (read-octets socket)))))

(defun telnet-interact (client)
  (format t "Got a connection~%")
  (send-dump client (octet-vector +iac+ +do+ +terminal-type+))
  (send-dump client (octet-vector +iac+ +sb+ +terminal-type+ +send+
                                  +iac+ +se+))
  (send-dump client (octet-vector +iac+ +do+ +naws+))
  (send-dump client (sb-ext:string-to-octets "Hello, world"))
  (disconnect client))

(defun telnet-test (port)
  (let ((server-socket (make-socket)))
    (unwind-protect
         (progn
           (bind server-socket :port port)
           (listen server-socket)
           (loop
            (when (read-ready server-socket)
              (funcall 'telnet-interact (accept server-socket)))))
      (disconnect server-socket))))

(defparameter

(defparameter *http-response*
  (make-request-buffer "HTTP/1.1 200 OK" *crlf*
                       "Date: Thu, 27 Apr 2006 14:59:45 GMT" *crlf*
                       "Server: Apache" *crlf*
                       "Content-Type: text/html" *crlf*
                       "Content-Length: 12" *crlf*
                       "Last-Modified: Thu, 27 Apr 2006 14:59:36 GMT" *crlf*
                       "Cache-Control: max-age=60, private" *crlf*
                       "Vary: Accept-Encoding,User-Agent" *crlf*
                       "Expires: Thu, 27 Apr 2006 15:00:36 GMT" *crlf*
                       "Connection: close" *crlf*
                       *crlf*
                       "Hello, world"))


(defun echo (socket)
  (let ((octets (read-octets socket)))
    (format t "~&<<(~D) ~A~%" (file-descriptor socket) octets)
    (format t "~&>>(~D) ~A~%" (file-descriptor socket) octets)
    (write-octets socket octets)
    (length octets)))

(defun http (socket)
  (let ((octets (read-octets socket)))
    (when (plusp (length octets))
      (write-octets socket *http-response*)
    (length octets))))

(defun echo-epoll-test (port)
  (let ((server-socket (make-socket))
        (epoll (epoll:open-controller))
        (clients (make-hash-table))
        (client-id 0)
        (connection-count 0))
    (unwind-protect
         (progn
           (bind server-socket :port port)
           (listen server-socket)
           (set-nonblocking (file-descriptor server-socket))
           (epoll:add-watch epoll (file-descriptor server-socket)
                            :read t :data1 0 :edge-triggered t)
           (loop
            (when (> (incf connection-count) 10)
              (return))
            (let ((events (epoll:pending-events epoll)))
              (dotimes (i (length events))
                (let* ((event (aref events i))
                       (data (epoll::data1 event)))
                  (cond ((zerop data)
                         (loop
                          (let ((client (accept server-socket)))
                            (if client
                                (let ((id (incf client-id)))
                                  (setf (gethash id clients) client)
                                  (epoll:add-watch epoll
                                                   (file-descriptor client)
                                                   :edge-triggered t
                                                   :read t :data1 id))
                                (return)))))
                        ((plusp data)
                         (let ((socket (gethash data clients)))
                           (http socket)
                           (shutdown-read socket)
                           (shutdown-write socket)
                           (epoll:delete-watch epoll (file-descriptor socket))
                           (disconnect socket)
                           (remhash data clients)))))))))
      (disconnect server-socket)
      (epoll:close-controller epoll)
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (ignore-errors (disconnect v)))
               clients))))

(defun client-test (port)
  (let ((epc (epoll:open-controller))
        (socket (make-socket)))
    (unwind-protect
         (progn
           (connect socket (ip-address "127.0.0.1") port)
           (write-octets socket (octet-vector 32 32 32 32 10 13))
           (read-ready socket)
           (read-octets socket)
           (write-octets socket (octet-vector 32 32 32 32 10 13))
           (write-octets socket (octet-vector 32 32 32 32 10 13))
           (write-octets socket (octet-vector 32 32 32 32 10 13)))
      (epoll:close-controller epc)
      (disconnect socket))))
