;;;; $Id: snarf.lisp,v 1.6 2006/04/26 14:21:31 xach Exp $

(in-package #:zpb-http)

(defparameter *user-agent*
  "snarf.lisp (on behalf of http://wigflip.com/saywhat/)")

(defun make-http-request (hostname port path)
  (make-request-buffer "GET " path " HTTP/1.1" *crlf*
                       "Host: " hostname
                       (format nil "~:[~;~D~]" (/= port 80) port)
                       *crlf*
                       "User-Agent: " *user-agent* *crlf*
                       "Connection: close" *crlf*
                       *crlf*))


(defparameter *max-file-size* 2500000)
(defvar *debug-header*)

(defun sanity-check-header (header)
  (setf *debug-header* header)
  (let* ((parsed (parse-header header))
         (content-length (header-value "content-length" parsed))
         (transfer-encoding (header-value "transfer-encoding" parsed)))
    (when (/= (status parsed) 200)
      (error "Status ~D /= 200" (status parsed)))
    (when (null content-length)
      (error "No content-length available"))
    (when transfer-encoding
      (error "Transfer-encoding ~A not supported" transfer-encoding))
    (let ((size (parse-integer content-length)))
      (when (> size *max-file-size*)
        (error "Content too big: ~D > ~D"
               size
               *max-file-size*)))))

(defun cautiously-connect (socket address port timeout)
  (handler-case
      (sb-ext:with-timeout timeout
        (connect socket address port))
    (sb-bsd-sockets:connection-refused-error ()
      (error 'connection-refused
             :remote-ip address
             :remote-port port))
    (sb-ext:timeout ()
      (error 'connection-timed-out
                        :remote-ip address
                        :remote-port port))))

(defun snarf (url output-file &key (if-exists :supersede))
  (multiple-value-bind (hostname port path)
      (parse-url url)
    (unless (and hostname port path)
      (error "Can't parse ~A as an URL: ~S ~S ~S"
             url hostname port path))
    (unless (= port 80)
      (error "Cowardly refusing to connect to port /= 80 (~A)"
             url))
    (let (socket)
      (unwind-protect
           (let ((address (one-hostname-address hostname))
                 (request (make-http-request hostname 80 path)))
             (setf socket (make-socket))
             (cautiously-connect socket address port 5)
             (write-octets socket request)
             (multiple-value-bind (header content-start)
                 (read-http-header socket)
               (sanity-check-header header)
               (with-open-file (stream output-file
                                :direction :output
                                :if-exists if-exists
                                :element-type 'octet)
                 (write-sequence content-start stream)
                 (let ((buffer (make-socket-buffer)))
                   (loop
                    (multiple-value-bind (buf count)
                        (read-octets socket :buffer buffer)
                      (when (zerop count)
                        (return))
                      (write-sequence buf stream :end count))))))
             (probe-file output-file))
        (when socket
          (disconnect socket))))))
      