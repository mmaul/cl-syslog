
(in-package :cl-syslog-tests)

(nst:def-test-group lookups ()
  (:documentation "Lookup functions")

  (nst:def-test invalid-priorities 
      (:eql :fired) (handler-case (get-priority :misplaced)
		      (invalid-priority () :fired)))

  (nst:def-test priorities 
      (:eql 4) (get-priority :warning))
   
  (nst:def-test invalid-facilities 
      (:eql :fired) (handler-case (get-facility :misplaced)
		      (invalid-facility () :fired)))

  (nst:def-test facilities 
      (:eql 32) (get-facility :auth))
  )

(nst:def-test-group udp-logging ()
  (:documentation "Test UDP logging and time")

  (nst:def-test log-lo 
      (:true) (> (cl-syslog.udp:ulog "Hello World" 
		 :logger (cl-syslog.udp:udp-logger "127.0.0.1" 
                 :transient t)) 0))

  (nst:def-test test-epoch-to-syslog-time 
      (:equal "1969-12-31T19:00:0.0-5:00") (cl-syslog.udp:epoch-to-syslog-time 0))
  )