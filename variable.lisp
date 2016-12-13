;;;; See the LICENSE file for licensing information.

(in-package #:syslog)

(defparameter *priorities*
  '((:emerg . 0)
    (:alert . 1)
    (:crit . 2)
    (:err . 3)
    (:warning . 4)
    (:notice . 5)
    (:info . 6)
    (:debug . 7)))

(defparameter *facilities*
  '((:kern . 0)
    (:user . 1)
    (:mail . 2)
    (:daemon . 3)
    (:auth . 4)
    (:syslog . 5)
    (:lpr . 6)
    (:news . 7)
    (:uucp . 8)
    (:cron . 9)
    (:authpriv . 10)
    (:ftp . 11)
    (:local0 . 16)
    (:local1 . 17)
    (:local2 . 18)
    (:local3 . 19)
    (:local4 . 20)
    (:local5 . 21)
    (:local6 . 22)
    (:local7 . 23)))

(defconstant +log-pid+ #x01
  "Log the pid with each message.")
(defconstant +log-cons+ #x02
  "Log on the console if errors in sending.")
(defconstant +log-odelay+ #x04
  "Delay open until first syslog() (default).")
(defconstant +log-ndelay+ #x08
  "Don't delay open.")
(defconstant +log-nowait+ #x10
  "Don't wait for console forks: deprecated.")
(defconstant +log-perror+ #x20
  "Log to stderr as well.")
