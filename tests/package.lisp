

(defpackage :cl-syslog-tests
  (:use :cl)
  (:import-from :cl-syslog :get-priority :get-facility 
		invalid-priority invalid-facility) 
  (:import-from :cl-syslog.udp :udp-logger)
  (:shadowing-import-from :cl-syslog.udp :log) 
  (:export :run-tests))

(in-package :cl-syslog-tests)

(defun run-tests ()
  (let ((*print-pretty* t))
    (nst:nst-cmd :run-package #.*package*)))
