(asdf:defsystem #:cl-syslog
  :author "Erik Enge, Mike Maul"
  :version (:read-file-form "VERSION.txt")
  :licence "MIT (See LICENSE)"
  :description "Common Lisp syslog interface."
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-syslog.tests)))
  :depends-on (#:cl-syslog.local #:cl-syslog.udp))
