;;;; $Id: url.lisp,v 1.8 2006/05/10 19:27:11 xach Exp $

(in-package #:zpb-http)

(defun url-syntax-error (input pos message)
  (error 'url-syntax-error
         :input input
         :pos pos
         :message message))

(defun escape-path (path)
  (with-output-to-string (stream)
    (loop for char across path
          for code = (char-code char)
          if (or (<= 0 code #x20)
                 (= code 127))
          do
          (format stream "%~2,'0X" code)
          else do
          (write-char char stream))))

(defun parse-url (url)
  (setf url (string-trim " " url))
  (when (string= url "")
    (url-syntax-error url 0 "Empty URL"))
  (let ((i (mismatch url "http://" :test 'char-equal))
        (state :in-host)
        mark
        input
        host
        (port "80")
        path)
    (flet ((mark (&optional (delta 0)) (setf mark (+ i delta)))
           (save (&optional (pos i)) (subseq url mark pos)))
      (mark)
      (loop
       (when (<= (length url) i)
         (case state
           (:in-host
            (setf host (save) port "80" path "/"))
           (:in-port
            (setf port (save) path "/"))
           (:in-path
            (setf path (save))))
         (return (values host (parse-integer port) path)))
       (setf input (char url i))
       (case state
         (:in-host
          (cond ((char= input #\/)
                 (setf host (save))
                 (mark)
                 (setf state :in-path))
                ((char= input #\:)
                 (setf host (save))
                 (mark 1)
                 (setf state :in-port))
                ((not (position input
                                "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_."
                                :test 'char-equal))
                 (url-syntax-error url i "Invalid hostname in URL"))))
         (:in-port
          (case input
            (#\/
               (setf port (save))
               (mark)
               (setf state :in-path))
            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
            (t (error "Bad input in port: ~A" input))))
         (:in-path
          (case input
            (#\#
               (setf path (save)
                     state :in-fragment))))
         (:in-fragment))
       (incf i)))))
