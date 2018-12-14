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

(defun ip4-address-p (x)
  (flet ((valid-group-p (x)
           (and (<= 1 (length x) 3)
                (every #'digit-char-p x)
                (or (string= "0" x)
                    (not (char= #\0 (char x 0))))
                (<= 0 (parse-integer x) 255))))
    (and (stringp x)
         (= 3 (count #\. x))
         (every #'valid-group-p (split-sequence:split-sequence #\. x)))))

(defun ip6-address-p (x)
  (labels ((hex-digit-char-p (c)
             (or (digit-char-p c)
                 (find c "aAbBcCdDeEfF")))
           (valid-group-p (x)
             (or (zerop (length x))
                 (and (= 4 (length x))
                      (every #'hex-digit-char-p x)
                      (<= 0 (parse-integer x :radix 16) #xFFFF)))))
    (and (stringp x)
         (= 7 (count #\: x))
         (every #'valid-group-p (split-sequence:split-sequence #\: x)))))

(defun ip-address-p (x)
  (or (ip4-address-p x)
      (ip6-address-p x)))

(define-structured-data-id |origin| (:standard t)
  (|ip| :allow-repetitions t
        :validator 'ip-address-p)
  |enterpriseId|
  (|software| :length (integer 0 40))
  (|swVersion| :length (integer 0 32)))

(define-structured-data-id |meta| (:standard t)
  |sequenceId|
  |sysUpTime|
  |language|)
