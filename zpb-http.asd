;;;; $Id: zpb-http.asd,v 1.7 2006/05/10 19:14:39 xach Exp $

(defpackage #:zpb-http-system
  (:use #:cl #:asdf))

(in-package #:zpb-http-system)

(defsystem #:zpb-http
  :depends-on (#:sb-posix #:sb-bsd-sockets)
  :components ((:file "unix-error")
               (:file "epoll"
                      :depends-on ("unix-error"))
               (:file "dns")
               (:file "package")
               (:file "types"
                      :depends-on ("package"))
               (:file "util"
                      :depends-on ("types"
                                   "package"))
               (:file "conditions"
                      :depends-on ("package"))
               (:file "match"
                      :depends-on ("package"))
               (:file "ascii"
                      :depends-on ("package"))
               (:file "url"
                      :depends-on ("package"))
               (:file "socket"
                      :depends-on ("package"
                                   "util"
                                   "unix-error"
                                   "dns"
                                   "epoll"))
               (:file "resolver"
                      :depends-on ("package"
                                   "dns"
                                   "socket"))
               (:file "http"
                      :depends-on ("package"
                                   "util"
                                   "conditions"
                                   "types"
                                   "match"
                                   "ascii"))
               (:file "snarf"
                      :depends-on ("package"
                                   "http"
                                   "conditions"))))
