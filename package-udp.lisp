;;;; See the LICENSE file for licensing information.

(defpackage #:cl-syslog.udp
  (:nicknames #:syslog.udp)
  (:use #:cl #:babel #:usocket)
  (:shadow #:log)
  (:export #:log
           #:udp-logger
           #:ulog
           #:ulog-raw
           #:ulog-min
           #:epoch-to-syslog-time
           #:udp-logger-close)
  (:import-from #:cl-syslog
                #:get-priority
                #:get-facility
                #:+log-pid+
                #:+log-cons+
                #:+log-odelay+
                #:+log-ndelay+
                #:+log-nowait+
                #:+log-perror+)
  (:Documentation "UDP Syslog interface"))
