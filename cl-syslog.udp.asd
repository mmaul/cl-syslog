(asdf:defsystem #:cl-syslog.udp
  :license "MIT (See LICENSE)"
  :version (:read-file-form "VERSION.txt")
  :description "UDP syslog logging."
  :depends-on (#:cl-syslog.local #:babel #:cffi #:usocket #:local-time)
  :serial t
  :components ((:file "package-udp")
               (:file "udp-syslog")))
