(asdf:defsystem #:cl-syslog.local
  :license "MIT (See LICENSE)"
  :version (:read-file-form "VERSION.txt")
  :description "Local-only syslog logging."
  :depends-on (#:alexandria #:cffi #:global-vars #:usocket #:split-sequence
                            #+sbcl #:sb-posix
                            )
  :serial t
  :components ((:file "package")
               (:file "variable")
               (:file "cl-syslog")
               (:file "rfc5424")
               (:file "rfc5424-reserved")))
