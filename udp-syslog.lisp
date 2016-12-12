;;;; NAME: udp-syslog.lisp
;;;; AUTHOR: Mike Maul
;;;; UDP Syslog - Extensions to cl-syslog to allow for streaming to
;;;; syslog destination.

;;; TODO: Implement structured data

;;;; See the LICENSE file for licensing information.

(in-package #:cl-syslog.udp)
;;

(defparameter *udp-syslog-socket* nil)

(define-condition unset-logger (error)
  ((msg :initarg :msg :reader msg)))

(defun udp-logger (host &key (port 514) transient)
  "
  Constructs a UDP socket to host:port
  :transient returns udp socket with out setting global socket
  "
  (let ((ulogger (usocket:socket-connect host port :protocol :datagram)))
    (when (not transient)
      (setf *udp-syslog-socket* ulogger))
    ulogger))

(defun udp-logger-close (&optional udp-logger-socket)
  "
  Closes udp socket
  "
  (let ((sock (or udp-logger-socket *udp-syslog-socket*)))
    (socket-close sock)))

(defun ulog-raw (message &key logger)
  "
  Streams contents of message string to UDP destination.
  If a strict syslog formatted message is desired see (ulog ...),
  otherwise this function would be okay to simple udp listener.
  
  ##Parameters##
  :logger
  The destination is specified by either setting the global udp logger by
  calling the udp-logger function or by specifying a logger with the  
  :logger parameter. 
  "
  (usocket:socket-send (or logger *udp-syslog-socket*
                           (error (make-condition 'unset-logger)))
                       (if (typep message 'string)
                           (babel:string-to-octets message)
                         message)
                       (length message)))

#||
   SYSLOG Message Format from RFC 5425
      SYSLOG-MSG      = HEADER SP STRUCTURED-DATA [SP MSG]
      HEADER          = PRI VERSION SP TIMESTAMP SP HOSTNAME
                        SP APP-NAME SP PROCID SP MSGID
      PRI             = "<" PRIVAL ">"
      PRIVAL          = 1*3DIGIT ; range 0 .. 191
      VERSION         = NONZERO-DIGIT 0*2DIGIT
      HOSTNAME        = NILVALUE / 1*255PRINTUSASCII
      APP-NAME        = NILVALUE / 1*48PRINTUSASCII
      PROCID          = NILVALUE / 1*128PRINTUSASCII
      MSGID           = NILVALUE / 1*32PRINTUSASCII
      TIMESTAMP       = NILVALUE / FULL-DATE "T" FULL-TIME
      FULL-DATE       = DATE-FULLYEAR "-" DATE-MONTH "-" DATE-MDAY
      DATE-FULLYEAR   = 4DIGIT
      DATE-MONTH      = 2DIGIT  ; 01-12
      DATE-MDAY       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on
                                ; month/year
      FULL-TIME       = PARTIAL-TIME TIME-OFFSET
      PARTIAL-TIME    = TIME-HOUR ":" TIME-MINUTE ":" TIME-SECOND
                        [TIME-SECFRAC]
      TIME-HOUR       = 2DIGIT  ; 00-23
      TIME-MINUTE     = 2DIGIT  ; 00-59
      TIME-SECOND     = 2DIGIT  ; 00-59
      TIME-SECFRAC    = "." 1*6DIGIT
      TIME-OFFSET     = "Z" / TIME-NUMOFFSET
      TIME-NUMOFFSET  = ("+" / "-") TIME-HOUR ":" TIME-MINUTE
      STRUCTURED-DATA = NILVALUE / 1*SD-ELEMENT
      SD-ELEMENT      = "[" SD-ID *(SP SD-PARAM) "]"
      SD-PARAM        = PARAM-NAME "=" %d34 PARAM-VALUE %d34
      SD-ID           = SD-NAME
      PARAM-NAME      = SD-NAME
      PARAM-VALUE     = UTF-8-STRING ; characters '"', '\' and
                                     ; ']' MUST be escaped.
      SD-NAME         = 1*32PRINTUSASCII
                        ; except '=', SP, ']', %d34 (")
      MSG             = MSG-ANY / MSG-UTF8
      MSG-ANY         = *OCTET ; not starting with BOM
      MSG-UTF8        = BOM UTF-8-STRING
      BOM             = %xEF.BB.BF
      UTF-8-STRING    = *OCTET ; UTF-8 string as specified
                        ; in RFC 3629
      OCTET           = %d00-255
      SP              = %d32
      PRINTUSASCII    = %d33-126
      NONZERO-DIGIT   = %d49-57
      DIGIT           = %d48 / NONZERO-DIGIT
      NILVALUE        = "-"
||#

#+clisp
(defmacro getpid ()  `(funcall ,(symbol-function
                              ;; Decide at load time which function to use.
                              (or (and (member :unix *features* :test #'eq)
                                       (or (find-symbol "PROCESS-ID" "SYS")
                                           (find-symbol "PROGRAM-ID" "SYS")))
                                  'getpid-from-environment))))
#+ccl
(defun getpid () (ccl::getpid))

#+(or cmu scl)
(defun getpid () (unix:unix-getpid))

#+sbcl
(defun getpid () (sb-unix:unix-getpid))

#+:allegro
(defun getpid () (excl.osi:getpid))

(defun epoch-to-syslog-time (&optional epoch &key (tz local-time:*DEFAULT-TIMEZONE*))
  "
  Syslog timestamp formatter defaults to current time.
  Optional arg epoch as epoch seconds
  Example format:2013-12-14T21:09:57.0Z-5
  Timezone can be specified using local-time time zone variables
  for example to specify GMT use local-time:+GMT-ZONE+
  "
  (let ((time (if epoch (local-time:unix-to-timestamp epoch)
             (local-time:now))))
    (local-time:format-timestring nil time :timezone tz)
    ))

(defmacro log (name facility priority text &optional (option 0)
                    &key procid timestamp logger)
  "
  Macro wrapping ulog providing backwards signature compatibility with
  cl-syslog:log function. The purpose of this is to allow the switching
  of log destinations by switching bewteen the cl-syslog and cl-syslog.udp
  namespaces.

  See documentation for ulog details not covered here.

  ##Parameters##
  name - Application name displayed in application name section if syslog message

  facility - See ulog documentation for acceptable values

  priority - See ulog documentation for acceptable values

  text - Body of the message

  option - Ignored, however parameter is necessary to maintain signature
           compatibility with cl-syslog:log

  :procid - See ulog documentation

  :timestamp - See ulog documentation

  :logger - See ulog documentation

  ##Return values##
  value of text parameter, on error condition will be thrown.  
  "
  `(ulog ,text :pri ,priority :fac ,facility :app-name ,name
            :procid ,procid :timestamp ,timestamp :logger ,logger)
  text
  )

(defun ulog ( msg &key pri fac 
                  hostname app-name procid msgid timestamp
                  logger)
  "
  Streams a syslog formatted message (rfc5424) to a UDP destination.
  Below is a sample of the equivalant string representation of a syslog 
  message streamed with this function:
    <165>1 2003-08-24T05:14:15.000003-07:00 192.0.2.1 myproc 8710 - - %% It's time to make the do-nuts.

  ##Parameters##
  The paremeters map into the syslog message as shown below
  :PRI VERSION SP :TIMESTAMP SP :HOSTNAME
       SP :APP-NAME SP :PROCID SP :MSGID SP STRUCTURED-DATA [SP :MSG]
 
  :logger
  The destination is specified by either setting the global udp logger by
  calling the udp-logger function or by specifying a logger with the  
  :logger parameter. 

  :pri
  Must be one of the following priorities (default :info)
    :emerg   :alert   :crit :err
    :warning :notice  :info :debug

  :fac
  Facility parameter  must be one of (default :local7)
    :kern   :user    :mail     :daemon
    :auth   :syslog  :lpr      :news
    :uucp   :cron    :authpriv :ftp
    :local0 :local1  :local2   :local3
    :local4 :local5  :local6   :local7

  :app-name
  Application name defaults to *package*

  :hostname
  Hostname defaults to (machine-instance)
  "
  (ulog-raw
      (coerce (concatenate 'vector (babel:string-to-octets
                     (format nil "<~d>1 ~a ~a ~a ~a ~a ~a "
                             (+  (* 8 (syslog:get-facility (or fac :local7)))
                                 (syslog:get-priority (or pri :info)))
                             (or timestamp (epoch-to-syslog-time)) 
                             (or hostname (machine-instance)) 
                             (or app-name (package-name *package*))
                             (or procid (getpid) "")
                             (or msgid "")
                             "-" ; Unimplemented structured data section
                             ))
                       #(#xef #xbb #xbf)
                       (babel:string-to-octets msg))
              '(vector (unsigned-byte 8)))
      :logger logger)
)
#|
26-Feb-2014 19:12:27.115 queries: info: client 134.67.18.22#49871: query: LC3040BJBRISB02.rtpnc.epa.gov IN A + (134.67.208.10)
|#


(defun ulog-min ( msg &key pri fac logger)
  "
  Streams a syslog formatted message (rfc5424) to a UDP destination.
  Below is a sample of the equivalant string representation of a syslog 
  message streamed with this function:
    <165>1 2003-08-24T05:14:15.000003-07:00 192.0.2.1 myproc 8710 - - %% It's time to make the do-nuts.

  ##Parameters##
  The paremeters map into the syslog message as shown below
  :PRI VERSION SP :TIMESTAMP SP :HOSTNAME
       SP :APP-NAME SP :PROCID SP :MSGID SP STRUCTURED-DATA [SP :MSG]
 
  :logger
  The destination is specified by either setting the global udp logger by
  calling the udp-logger function or by specifying a logger with the  
  :logger parameter. 

  :pri
  Must be one of the following priorities (default :info)
    :emerg   :alert   :crit :err
    :warning :notice  :info :debug

  :fac
  Facility parameter  must be one of (default :local7)
    :kern   :user    :mail     :daemon
    :auth   :syslog  :lpr      :news
    :uucp   :cron    :authpriv :ftp
    :local0 :local1  :local2   :local3
    :local4 :local5  :local6   :local7

  :app-name
  Application name defaults to *package*

  :hostname
  Hostname defaults to (machine-instance)
  "
  (ulog-raw
      (coerce (concatenate 'vector (babel:string-to-octets
                     (format nil "<~d> "
                             (+  (* 1 (syslog:get-facility (or fac :local7)))
                                 (syslog:get-priority (or pri :info)))
                             ))
                           (babel:string-to-octets msg))
              '(vector (unsigned-byte 8)))
      :logger logger)
)
