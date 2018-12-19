;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-syslog
  (:nicknames #:syslog)
  (:use #:cl)
  (:shadow #:log)
  ;; *FEATURES* symbols
  (:export #:rfc5424)
  ;; Raw C interface
  (:export
   #:openlog
   #:closelog
   #:syslog)

  ;; Basic syslog logging interface.
  (:export
   #:log
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
  (:export
   #:define-structured-data-id          ; MACRO
   #:malformed-rfc5424-input            ; CONDITION
   #:null-log-writer                    ; FUNCTION
   #:syslog-log-writer                  ; FUNCTION
   #:stream-log-writer                  ; FUNCTION
   #:udp-log-writer                     ; FUNCTION
   #:tee-to-stream                      ; FUNCTION
   #:join-log-writers                   ; FUNCTION
   #:rfc5424-logger                     ; CLASS
   #:current-time                       ; GENERIC, METHOD
   #:format-log                         ; GENERIC, METHOD
   #:rfc-log                            ; MACRO
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

(eval-when (:load-toplevel :execute)
  ;; :CL-SYSLOG-RFC5424 indicates that the loaded syslog conforms to
  ;; the RFC 5424 standard. We sould like to indicate this feature is
  ;; present only until after the logging library is loaded.
  (pushnew 'cl-syslog::rfc5424 *features*))
