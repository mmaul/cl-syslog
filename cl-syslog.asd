;;;; $Id: cl-syslog.asd,v 1.4 2006/11/28 19:46:09 lnostdal Exp $
;;;; $Source: /project/cl-syslog/cvsroot/cl-syslog/cl-syslog.asd,v $

;;;; See the LICENSE file for licensing information.

(in-package #:cl-user)

(defpackage #:cl-syslog-system
    (:use #:cl #:asdf)
  (:documentation "Package to create the ASDF system for the cl-syslog
package"))

(in-package #:cl-syslog-system)

(defsystem cl-syslog
    :name "cl-syslog"
    :author "Erik Enge"
    :version "0.1.0"
    :licence "MIT"
    :description "Common Lisp syslog interface"
    :depends-on (:cffi :usocket :simple-date-time)
    :properties ((#:author-email . "cl-syslog-devel@common-lisp.net")
                 (#:date . "$Date: 2006/11/28 19:46:09 $")
                 ((#:albert #:output-dir) . "doc/api-doc/")
                 ((#:albert #:formats) . ("docbook"))
                 ((#:albert #:docbook #:template) . "book")
                 ((#:albert #:docbook #:bgcolor) . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))
    :components ((:file "package")
                 (:file "variable"
                        :depends-on ("package"))
                 (:file "cl-syslog"
                        :depends-on ("variable"))
                 (:file "udp-syslog"
                        :depends-on ("cl-syslog"))
                 ))


