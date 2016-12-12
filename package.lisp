;;;; See the LICENSE file for licensing information.

(defpackage #:cl-syslog
  (:nicknames #:syslog)
  (:use #:cl)
  (:shadow #:log)
  (:export #:log
           #:get-facility
           #:get-priority
           #:+log-pid+
           #:+log-cons+
           #:+log-odelay+
           #:+log-ndelay+
           #:+log-nowait+
           #:+log-perror+
           #:invalid-priority
           #:invalid-facility)
  (:documentation "Common Lisp interface to syslog."))




  
