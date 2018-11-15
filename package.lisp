;;;; See the LICENSE file for licensing information.

(defpackage #:cl-syslog
  (:nicknames #:syslog)
  (:use #:cl)
  (:shadow #:log)
  ;; Basic syslog logging interface.
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
  ;; RFC 5424 logging
  (:export #:define-structured-data-id  ; MACRO
           #:malformed-rfc5424-input    ; CONDITION
           #:rfc5424-logger             ; CLASS
           #:current-time               ; GENERIC, METHOD
           #:log-string                 ; GENERIC, METHOD
           #:rfc-log                    ; MACRO
           )
  ;; RFC 5424 IETF-reserved structured data names
  (:export
   #:|timeQuality|
   #:|tzKnown|
   #:|isSynced|
   #:|syncAccuracy|

   #:|origin|
   #:|ip|
   #:|enterpriseId|
   #:|software|
   #:|swVersion|

   #:|meta|
   #:|sequenceId|
   #:|sysUpTime|
   #:|language|)
  (:documentation "Common Lisp interface to syslog."))




  
