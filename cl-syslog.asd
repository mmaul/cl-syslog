(asdf:defsystem #:cl-syslog
  :author "Erik Enge, Mike Maul, Robert Smith"
  :version (:read-file-form "VERSION.txt")
  :licence "MIT (See LICENSE)"
  :description "Common Lisp syslog interface."
  :depends-on (#:alexandria #:cffi #:global-vars #:usocket #:split-sequence #:babel #:local-time
               #+sbcl #:sb-posix)
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-syslog-tests)))
  :components ((:file "package")
               (:file "variable")
               (:file "cl-syslog")
               (:file "rfc5424")
               (:file "rfc5424-reserved")
               )
  )
