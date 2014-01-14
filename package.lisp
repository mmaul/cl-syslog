;;;; $Id: package.lisp,v 1.1 2003/11/13 18:32:45 eenge Exp $
;;;; $Source: /project/cl-syslog/cvsroot/cl-syslog/package.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :cl-user)

(cl:eval-when (:execute :load-toplevel :compile-toplevel)
  (cl:defpackage :cl-syslog
      (:use :cl)
    (:nicknames :syslog)
    (:shadow :log)
    (:export :log :get-facility :get-priority
             :+log-pid+ :+log-cons+ :+log-odelay+
             :+log-ndelay+ :+log-nowait+ :+log-perror+)
    (:documentation "Common Lisp interface to syslog."))

  (cl:defpackage :cl-syslog-udp
                 (:nicknames :syslog-udp)
                 (:use :cl :babel :usocket)
                 (:export :udp-logger :ulog :ulog-raw :epoch-to-syslog-time
                          )
                 (:Documentation "UDP Syslog interface"))
  )
