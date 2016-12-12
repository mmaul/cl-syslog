(asdf:defsystem #:cl-syslog
  :author "Erik Enge, Mike Maul"
  :version (:read-file-form "VERSION.txt")
  :licence "MIT (See LICENSE)"
  :description "Common Lisp syslog interface."
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-syslog-tests)))
  :depends-on (#:cl-syslog.local #:cl-syslog.udp))

(asdf:defsystem #:cl-syslog.local
  :license "MIT (See LICENSE)"
  :version (:read-file-form "VERSION.txt")
  :description "Local-only syslog logging."
  :depends-on (#:cffi)
  :serial t
  :components ((:file "package")
               (:file "variable")
               (:file "cl-syslog")))

(asdf:defsystem #:cl-syslog.udp
  :license "MIT (See LICENSE)"
  :version (:read-file-form "VERSION.txt")
  :description "Local-only syslog logging."
  :depends-on (#:cl-syslog.local #:babel #:cffi #:usocket #:local-time)
  :serial t
  :components ((:file "package-udp")
               (:file "udp-syslog")))


(asdf:defsystem #:cl-syslog-tests
  :description "tests for cl-syslog library"
  :version (:read-file-form "VERSION.txt")
  :author "Mike Maul <mike.maul@gmail.com>"
  :licence "MIT"
  :depends-on (#:cl-syslog #:nst #:cl-ppcre)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :cl-syslog-tests '#:run-all-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "tests")))
