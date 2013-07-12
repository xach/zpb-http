
(defpackage :snarf-system
  (:use :cl :asdf))

(in-package :snarf-system)

(defsystem :snarf
  :depends-on (:sb-bsd-sockets :sb-posix)
  :components ((:file "snarf")))
