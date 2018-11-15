(in-package #:cl-syslog)

;;;; Section 7.
;;;;
;;;; These are IETF-reserved structure ID's.

(define-structured-data-id |timeQuality| (:standard t)
  (|tzKnown| :length (integer 1 1)
             :validator (lambda (x) (member x '("0" "1") :test #'string=)))
  (|isSynced| :length (integer 1 1)
              :validator (lambda (x) (member x '("0" "1") :test #'string=)))
  (|syncAccuracy| :validator (lambda (x) (every #'digit-char-p x))))

(defun ip-address-p (x)
  (declare (ignore x))
  ;; TODO: actually parse an IP address
  t)

(Define-structured-data-id |origin| (:standard t)
  (|ip| :allow-repetitions t
        :validator 'ip-address-p)
  |enterpriseId|
  (|software| :length (integer 0 40))
  (|swVersion| :length (integer 0 32)))

(define-structured-data-id |meta| (:standard t)
  |sequenceId|
  |sysUpTime|
  |language|)
