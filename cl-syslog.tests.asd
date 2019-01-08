(asdf:defsystem #:cl-syslog.tests
  :description "tests for cl-syslog library"
  :version (:read-file-form "VERSION.txt")
  :author "Mike Maul <mike.maul@gmail.com>"
  :licence "MIT"
  :depends-on (#:cl-syslog #:nst #:cl-ppcre)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :cl-syslog.tests '#:run-all-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "tests")))
