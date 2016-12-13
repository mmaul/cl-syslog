
(in-package :cl-syslog-tests)

(nst:def-fixtures f-lo-logger
                  (:cleanup (cl-syslog.udp:udp-logger-close lo-logger))
                  (lo-logger (cl-syslog.udp:udp-logger "127.0.0.1" :transient t)))
  
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

(nst:def-test-group syslog-logging ()
  (:documentation "Test Syslog logging")

  (nst:def-test (log-syslog)
      (:equal "Test cl-syslog-tests:log-syslog")
      (syslog:log "myprog" :local7 :info "Test cl-syslog-tests:log-syslog")
    )
  )

(nst:def-test-group udp-logging ()
  (:documentation "Test UDP logging and time")

  (nst:def-test (log-lo :fixtures (f-lo-logger)) 
                (:true) (> (cl-syslog.udp:ulog "Hello World" :logger lo-logger)
                 0))

  (nst:def-test test-epoch-to-syslog-time 
      (:equal "1970-01-01T00:00:00.000000Z")
              (cl-syslog.udp:epoch-to-syslog-time 0 :tz local-time:+GMT-ZONE+))
  )

(defun run-all-tests ()
  (run-tests)
  (passed-testsp))

(defun passed-testsp ()
  (let ((tr (map 'list #'identity
                 (nth-value 1
                            (cl-ppcre:scan-to-strings "Package CL-SYSLOG-TESTS: (\\d+) of (\\d+) passed"
                                                      (with-output-to-string (s)
                                                        (nst-control-api:report-package 'cl-syslog-tests s)))))))
    (if tr (apply #'string= tr) nil))
  )
