;;;; $Id: cl-syslog.lisp,v 1.3 2006/11/28 19:46:09 lnostdal Exp $
;;;; $Source: /project/cl-syslog/cvsroot/cl-syslog/cl-syslog.lisp,v $

;;;; See the LICENSE file for licensing information.

(in-package :syslog)

;;
;; Condition
;;

(define-condition invalid-facility (error)
  ((facility
    :reader facility
    :initarg :facility))
  (:report (lambda (condition stream)
             (format stream "Invalid facility ~A." (facility condition)))))

(define-condition invalid-priority (error)
  ((priority
    :reader priority
    :initarg :priority))
  (:report (lambda (condition stream)
             (format stream "Invalid priority ~A." (priority condition)))))

;;
;; Foreign function
;;

(cffi:defcfun "openlog" :void
  (ident :string)
  (option :int)
  (facility :int))

(cffi:defcfun "closelog" :void)

(cffi:defcfun "syslog" :void
  (priority :int)
  (format :string)
  &rest)

;;
;; Utility
;;

(defun get-facility (facility-name)
  "Return facility number given the facility's name.  If there is no
such facility, signal `invalid-facility' error."
  (ash (or (cdr (assoc facility-name *facilities*))
           (error (make-condition 'invalid-facility :facility facility-name)))
       3))

(defun get-priority (priority-name)
  "Return priority number given the priority's name.  If there is no
such priority, signal `invalid-priority' error."
  (or (cdr (assoc priority-name *priorities*))
      (error (make-condition 'invalid-priority :priority priority-name))))

;;
;; Log function
;;

(defun log (name facility priority text &optional (option 0) &rest r)
  "Print message to syslog.
'option' can be any of the +log...+ constants"
  (openlog name option (get-facility facility))
  (syslog (get-priority priority) text)
  (closelog)
  text
  )

;; Still need to handle options
;; example (format nil ,fmt ,@options-and-args)
(defmacro logva (name facility priority fmt &rest options-and-args)
  "Print message to syslog. and supports variable list of arguments for the printf style format string 'fmt'
 Arguments for the fmt string must be prefixed by a keyword indicating tye argument type using
 CFFI style type specifiers. for example for the fmt string \"MSG:%a %d\" :string \"Hello\" :int 2
"
  `(progn (openlog ,name 0 (get-facility ,facility))
          (syslog (get-priority ,priority) ,fmt ,@options-and-args)
          (closelog)
          ))
