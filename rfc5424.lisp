(in-package #:syslog)

;;; In this file, the section names refer to sections of the RFC 5424
;;; of March 2009.
;;;
;;; For all WRITE-* functions, and for most format-checking predicate
;;; functions, refer to Section 6 "Syslog Message Format", which has
;;; the ABNF grammar for the messages.

(alexandria:define-constant NILVALUE "-"
  :test 'string=
  :documentation "The RFC 5424 \"NILVALUE\".")

(declaim (inline ascii-char-p))
(defun ascii-char-p (c)
  (<= 0 (char-code c) 127))

(declaim (inline ascii-control-char-p))
(defun ascii-control-char-p (c)
  (let ((code (char-code c)))
    (or (= code 32)
        (= code 127))))

(declaim (inline ascii-graphic-char-p))
(defun ascii-graphic-char-p (c)
  (not (ascii-control-char-p c)))

(declaim (inline ascii-graphic-string-p))
(defun ascii-graphic-string-p (x)
  (and (stringp x)
       (every #'ascii-graphic-char-p x)))

(declaim (inline ascii-whitespace-char-p))
(defun ascii-whitespace-char-p (c)
  (and (member c '(#\Tab #\Newline #\Linefeed #\Page #\Return #\Space))
       t))

;;; Section 6.2. HEADER

;;; Section 6.2.1. PRI

(deftype rfc5424-pri ()
  ;; A <PRI> value is constructed by the formula
  ;;
  ;;     PRIORITY(0-7) + 8 * FACILITY(0-23)
  ;;
  ;; which means there are 8 * 24 possible choices.
  `(integer 0 (,(* 8 24))))

(defun valid-pri-p (thing)
  (typep thing 'rfc5424-pri))

(defun write-pri (stream pri)
  (format stream "<~D>" pri))


;;; Section 6.2.2. VERSION
;;;
;;; The version is just "1".
(defun write-version (stream)
  (write-char #\1 stream))


;;; Section 6.2.3. TIMESTAMP
(defun write-two-digit-number (stream number)
  (format stream "~2,'0D" number))

(defun valid-year-p (x)
  ;; A year must be a 4-digit integer.
  (typep x '(integer 1000 9999)))

(defun valid-month-p (x)
  (typep x '(integer 1 12)))

(defun valid-day-p (x)
  (typep x '(integer 1 31)))

(defun write-date (stream year month day)
  (format stream "~D" year)
  (write-char #\- stream)
  (write-two-digit-number stream month)
  (write-char #\- stream)
  (write-two-digit-number stream day))

(defun valid-hour-p (x)
  (typep x '(integer 0 23)))

(defun valid-minute-p (x)
  (typep x '(integer 0 59)))

(defun valid-second-p (x)
  (typep x '(integer 0 59)))

(defun valid-fraction-of-a-second-p (x)
  (typep x '(or null (real 0 (1)))))

(defun write-time (stream hour minute second fraction-of-a-second)
  (write-two-digit-number stream hour)
  (write-char #\: stream)
  (write-two-digit-number stream minute)
  (write-char #\: stream)
  (write-two-digit-number stream second)
  (when fraction-of-a-second
    (write-char #\. stream)
    ;; six digits are allowed!
    (if (zerop fraction-of-a-second)
        (write-string "00" stream)
        (format stream "~D" (floor (* fraction-of-a-second #.(expt 10 6))))))
  nil)

;;; RFC 5424 cross-references RFC 3339 for the meaning of timestamps.
(defun write-utc-timestamp (stream year month day hour minute second fraction-of-a-second)
  (write-date stream year month day)
  (write-char #\T stream)
  (write-time stream hour minute second fraction-of-a-second)
  ;; We *ALWAYS* require UTC time. RFC 5424 allows any time offset,
  ;; but we'd prefer to be conservative here.
  (write-char #\Z stream)
  nil)


;;; Section 6.2.4. HOSTNAME
;;;
;;; RFC 5424 cross-references RFC 1034 for the format of a domain
;;; name, but it's only a SHOULD-requirement. It does mandate,
;;; however, that the string is no more than 255 characters in length.
(defun valid-hostname-p (thing)
  (or (null thing)
      (and (ascii-graphic-string-p thing)
           (<= 1 (length thing) 255))))

(defun write-hostname (stream hostname)
  (cond
    ((null hostname) (write-string NILVALUE stream))
    (t (write-string hostname stream)))
  nil)


;;; Section 6.2.5. APP-NAME

(defun valid-app-name-p (thing)
  (or (null thing)
      (and (ascii-graphic-string-p thing)
           (<= 1 (length thing) 48))))

(defun write-app-name (stream app-name)
  (cond
    ((null app-name) (write-string NILVALUE stream))
    (t (write-string app-name stream)))
  nil)


;;; Section 6.2.6. PROCID

(defun valid-procid-p (thing)
  (or (null thing)
      (and (ascii-graphic-string-p thing)
           (<= 1 (length thing) 128))))

(defun write-procid (stream procid)
  (cond
    ((null procid) (write-string NILVALUE stream))
    (t (write-string procid stream)))
  nil)


;;; Section 6.2.7. MSGID

(defun valid-msgid-p (thing)
  (or (null thing)
      (and (ascii-graphic-string-p thing)
           (<= 1 (length thing) 32))))

(defun write-msgid (stream msgid)
  (cond
    ((null msgid) (write-string NILVALUE stream))
    (t (write-string msgid stream)))
  nil)


;;; Section 6.3. STRUCTURED-DATA
;;;
;;; This is quite a complicated section, because it allows arbitrary,
;;; categorized key-values pairs to be present in the message. Some of
;;; the pairs are IETF-standardized, some aren't, and there's
;;; syntactic distinction.

(defstruct structured-data-field-description
  "A description of a field of structured data."
  ;; The printable name of the field.
  (name nil :type alexandria:string-designator :read-only t)
  ;; Are repetitions of this field allowed?
  (repetitions-allowed-p nil :type boolean :read-only t)
  ;; What is the Lisp type that describes the allowed length of the
  ;; field? By default, this is any non-negative integer.
  ;;
  ;; Currently unused.
  (length-type 'unsigned-byte :read-only t)
  ;; A function to validate the field. The function should take a
  ;; value as input, and return T if the value is valid, and NIL
  ;; otherwise. By default, T is always returned. (Note that this
  ;; function does not have to check if the input is a string. This
  ;; will be done at a higher level.)
  ;;
  ;; Currently un-used.
  (validator (constantly t) :type (or symbol function) :read-only t))

;;; Enterprise numbers are defined in Section 7.2.2.
(defun valid-enterprise-number-p (string)
  "Is the string STRING a valid enterprise number?"
  (check-type string string)
  (prog ((length (length string))
         (i 0))
     ;; The string must have contents.
     (when (zerop length)
       (return nil))

     ;; Fall-through to expecting a digit.

   :EXPECT-DIGIT
     (unless (digit-char-p (char string i))
       (return nil))
     (when (= i (1- length))
       (return t))
     (incf i)
     (go :EXPECT-DIGIT-OR-DOT)

   :EXPECT-DOT
     (when (or (not (char= #\. (char string i)))
               (= i (1- length)))
       (return nil))
     (incf i)
     (go :EXPECT-DIGIT)

   :EXPECT-DIGIT-OR-DOT
     (cond
       ((char= #\. (char string i))
        (go :EXPECT-DOT))
       ((digit-char-p (char string i))
        (go :EXPECT-DIGIT))
       (t
        (return nil)))))

;;; This is the SD-NAME
(defun valid-sd-name-p (string)
  (flet ((valid-char-p (char)
           (and (ascii-char-p char)
                (not (ascii-control-char-p char))
                (not (ascii-whitespace-char-p char))
                (not (member char '(#\@ #\= #\] #\"))))))
    (declare (inline valid-char-p))
    (and (<= 1 (length string) 32)
         (every #'valid-char-p string))))

(defun valid-sd-id-p (x)
  "Is X a valid structured data ID? Roughly, these are either:

    1. A bare ASCII name, in which case it's an IETF-reserved name.

    2. An ASCII name, followed by '@', followed by a number (which may have dots).

Return two values:

    1. A Boolean indicating whether it's a valid ID.

    2. A Boolean indicating whether its name conforms to an IETF-reserved name.
"
  (flet ((split (string-designator)
           (let* ((string (string string-designator))
                  (position (position #\@ string)))
             (if (null position)
                 (values string nil)
                 (values (subseq string 0 position)
                         (subseq string (1+ position)))))))
    (declare (inline split))
    ;; Is the thing even a string designator?
    (if (not (typep x 'alexandria:string-designator))
        (values nil nil)
        ;; It is. Split at the @-sign.
        (multiple-value-bind (before after) (split x)
          (cond
            ;; Is the thing before any @-sign invalid?
            ((not (valid-sd-name-p before))
             (values nil nil))
            ;; Is whatever's after an @-sign missing?
            ((null after)
             (values t t))
            ;; Is the thing after the @-sign valid?
            ((valid-enterprise-number-p after)
             (values t nil))
            ;; Nothing is right.
            (t (values nil nil)))))))

(defstruct structured-data-description
  "A description of structured data."
  ;; Section 6.3.2. SD-ID
  (id nil :type alexandria:string-designator :read-only t)
  (allow-other-params t :type boolean :read-only t)
  (fields nil :type alexandria:proper-list :read-only t))


;;; Section 6.3.3. SD-PARAM

(defun write-param-name (stream string)
  "Write out the PARAM-NAME STRING to the stream STREAM."
  (check-type string string)
  (write-string string stream)
  nil)

(defun write-param-value (stream string)
  "Write out the PARAM-VALUE STRING to the stream STREAM."
  (check-type string string)
  (loop :for c :of-type character :across string
        :do (case c
              ((#\" #\\ #\])            ; Required escape characters.
               (write-char #\\ stream)
               (write-char c stream))
              (otherwise
               (write-char c stream))))
  nil)

;;; Section 6.3.1. SD-ELEMENT
;;;
;;; We don't use any fancy data structures for the actual SD-ELEMENTs
;;; since they'll be frequently allocated and thrown away. Lisp is
;;; good at lists, so let it do its thing.
;;;
;;; These functions should be used *after* the contents have been
;;; validated by the requisite *-DESCRIPTION data structures.

(declaim (inline make-param param-name param-value))
(defun make-param (name value) (cons name value))
(defun param-name (param) (car param))
(defun param-value (param) (cdr param))
(defun valid-param-p (p)
  (and (consp p)
       (valid-sd-name-p (param-name p))
       ;; FIXME: stringp might need to be more specific. The RFC says
       ;; it should be a UTF8 string.
       (stringp (param-value p))))

(declaim (inline make-sd-element sd-element-id sd-element-params))
(defun make-sd-element (id &rest params) (list* id params))
(defun sd-element-id (elt) (first elt))
(defun sd-element-params (elt) (rest elt))
(defun valid-sd-element-p (e)
  (and (alexandria:proper-list-p e)
       (valid-sd-id-p (sd-element-id e))
       (every #'valid-param-p (sd-element-params e))))

(defun write-sd-element (stream elt)
  (write-char #\[ stream)
  (write-string (sd-element-id elt) stream)
  (dolist (param (sd-element-params elt))
    (write-char #\Space stream)
    (write-param-name stream (param-name param))
    (write-char #\= stream)
    (write-char #\" stream)
    (write-param-value stream (param-value param))
    (write-char #\" stream))
  (write-char #\] stream)
  nil)

(defun write-sd-elements (stream elts)
  (cond
    ((null elts) (write-string NILVALUE stream))
    (t (dolist (elt elts)
         (write-sd-element stream elt)))))


;;; Section 7. Structure Data IDs

(global-vars:define-global-var **structured-data-specs** (make-hash-table :test 'equal)
  "A table mapping structured data ID's (strings) to a STRUCTURED-DATA-DESCRIPTION.")

(defun find-sd-id (name)
  (check-type name alexandria:string-designator)
  (alexandria:if-let ((val (gethash (string name) **structured-data-specs**)))
    val
    (error "Couldn't find the structured data ID ~S. Perhaps you haven't ~
            defined it with DEFINE-STRUCTURED-DATA-ID?"
           (string name))))

(defun (setf find-sd-id) (new-value name)
  (check-type name alexandria:string-designator)
  (check-type new-value structured-data-description)
  (setf (gethash (string name) **structured-data-specs**) new-value))

(defmacro define-structured-data-id (id-name (&key allow-other-params standard) &body body)
  "Define a new structured data ID named ID-NAME.

ALLOW-OTHER-PARAMS is an option to allow other named parameters to be present.

STANDARD is an option to dictate that the defined message is an IETF-reserved name.

BODY specifies the fields and has the following syntax:

    <body> ::= <field>*

    <field> ::= <symbol>
              | (<symbol> [:allow-repetitions <boolean>]
                          [:length <length-type>]
                          [:validator <form>])

    <length-type> ::= an unevaluated form that is a subtype of UNSIGNED-BYTE

    <form> ::= an evaluated form producing a funcallable unary function mapping to booleans

The :ALLOW-REPETITIONS keyword allows a field to be repeated (default: nil).

The :LENGTH keyword specifies what the length of the string data must satisfy. By default it's unbounded by UNSIGNED-BYTE.

The :VALIDATOR keyword allows a validating function to be provided. By default it is (CONSTANTLY T).
"
  (check-type id-name alexandria:string-designator)
  (labels ((parse-field (field-name &key (allow-repetitions nil)
                                         (length 'unsigned-byte)
                                         (validator '(constantly t)))
             (check-type field-name alexandria:string-designator)
             (check-type allow-repetitions boolean)
             (assert (subtypep length 'unsigned-byte))
             `(make-structured-data-field-description
               :name ',(string field-name)
               :repetitions-allowed-p ',allow-repetitions
               :length-type ',length
               :validator ,validator)))
    (let ((name (string id-name)))
      (multiple-value-bind (valid? standard?) (valid-sd-id-p name)
        ;; Check that it's valid.
        (unless valid?
          (error "Invalid structured data ID: ~S" name))
        ;; Ensure that we match our stated standard naming compliance.
        (unless (eql standard standard?)
          (error "Declared that ~A ~:[is not~;is~] an IETF-reserved ~
                  (\"standard\") ID, but it was determined that ~
                  it ~:[is not~;is~]."
                 name
                 standard
                 standard?))

        `(eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (find-sd-id ',id-name)
                 (make-structured-data-description
                  :id ',name
                  :allow-other-params ',(and allow-other-params t)
                  :fields (list ,@(mapcar (lambda (field)
                                            (apply #'parse-field (alexandria:ensure-list field)))
                                          body))))
           ',id-name)))))


;;; Section 6.4. MSG
;;;
;;; This is the user-supplied message. The RFC asks for the message to
;;; be Unicode, formatted as UTF-8, but does not require it. For the
;;; purposes of this library, we are currently assuming ASCII messages
;;; only.

(defun write-msg (stream string)
  ;; TODO: Deal with the BOM for UTF8 payload.
  (write-string string stream))


;;; Back to Section 6, to bring it all together.

(defun write-rfc5424-syslog-message-unsafe (stream
                                            pri
                                            year
                                            month
                                            day
                                            hour
                                            minute
                                            second
                                            fractions-of-a-second
                                            hostname
                                            app-name
                                            procid
                                            msgid
                                            sd-elements
                                            msg)
  ;; All of this comprises a SYSLOG-MSG.

  ;; HEADER
  (write-pri stream pri)
  (write-version stream)                ; Not configurable.
  (write-char #\Space stream)
  (write-utc-timestamp stream year month day hour minute second fractions-of-a-second)
  (write-char #\Space stream)
  (write-hostname stream hostname)
  (write-char #\Space stream)
  (write-app-name stream app-name)
  (write-char #\Space stream)
  (write-procid stream procid)
  (write-char #\Space stream)
  (write-msgid stream msgid)

  ;; Done with HEADER. Back up to SYSLOG-MSG.
  (write-char #\Space stream)

  ;; STRUCTURED-DATA
  (write-sd-elements stream sd-elements)

  ;; Done with STRUCTURED-DATA. Back up to SYSLOG-MSG.
  ;;
  ;; Only write a space if we have a MSG.
  (when msg
    (write-char #\Space stream)
    ;; BOM
    ;; (write-byte #xEF stream)
    ;; (write-byte #xBB stream)
    ;; (write-byte #xBF stream)
    (write-msg stream msg))

  ;; Done. Don't return anything useful.
  nil)

(define-condition malformed-rfc5424-input (error)
  ((violated-assertion :initarg :violated-assertion
                       :reader violated-assertion))
  (:report (lambda (condition stream)
             (format stream "Malformed input for RFC 5424 syslog message. ~
                             The violated assertion was~%~S"
                     (violated-assertion condition)))))

(defmacro assert-rfc5424 (thing)
  ;; Like the ASSERT macro, but intended specifically for RFC5424
  ;; syntax violations.
  `(unless ,thing
     (error 'malformed-rfc5424-input :violated-assertion ',thing)))

(defun write-rfc5424-syslog-message (stream
                                     pri
                                     year
                                     month
                                     day
                                     hour
                                     minute
                                     second
                                     fraction-of-a-second
                                     hostname
                                     app-name
                                     procid
                                     msgid
                                     sd-elements
                                     msg)
  "Write the RFC 5424-compliant syslog message to the stream STREAM."
  (check-type stream stream)
  (assert-rfc5424 (valid-pri-p pri))
  (assert-rfc5424 (valid-year-p year))
  (assert-rfc5424 (valid-month-p month))
  (assert-rfc5424 (valid-day-p day))
  (assert-rfc5424 (valid-hour-p hour))
  (assert-rfc5424 (valid-minute-p minute))
  (assert-rfc5424 (valid-second-p second))
  (assert-rfc5424 (valid-fraction-of-a-second-p fraction-of-a-second))
  (assert-rfc5424 (valid-hostname-p hostname))
  (assert-rfc5424 (valid-app-name-p app-name))
  (assert-rfc5424 (valid-procid-p procid))
  (assert-rfc5424 (valid-msgid-p msgid))
  (assert-rfc5424 (every #'valid-sd-element-p sd-elements))
  ;; TODO: fix this. Should be either a collection of octets or a UTF8
  ;; string.
  (assert-rfc5424 (stringp msg))
  (write-rfc5424-syslog-message-unsafe stream
                                       pri
                                       year
                                       month
                                       day
                                       hour
                                       minute
                                       second
                                       fraction-of-a-second
                                       hostname
                                       app-name
                                       procid
                                       msgid
                                       sd-elements
                                      msg))


;;; Public Interface
;;;
;;; This isn't specified by RFC, but makes this file convenient to
;;; use.

(defclass rfc5424-logger ()
  ((facility :initarg :facility
             :reader logger-facility
             :documentation "The syslog facility, as a keyword. This is a required slot.")
   (maximum-priority :initarg :maximum-priority
                     :reader logger-maximum-priority
                     :documentation "The maximum priority above which log messages are not emitted to the external logging facility. The default maximum priority is :INFO. (Recall that priorities of *increasing* severity have *decreasing* priority values.)")
   (hostname :initarg :hostname
             :reader logger-hostname)
   (app-name :initarg :app-name
             :reader logger-app-name)
   (process-id :initarg :process-id
               :reader logger-process-id))
  (:default-initargs :maximum-priority ':info
                     :hostname (machine-instance)
                     :app-name nil
                     :process-id #+(and sbcl unix) (prin1-to-string (sb-posix:getpid))
                                 #-(and sbcl unix) nil)
  (:documentation "Default class representing RFC 5424-compliant logging."))

(defmethod shared-initialize :before ((logger rfc5424-logger) (slot-names t)
                                        &key facility maximum-priority
                                             hostname app-name process-id
                                        &allow-other-keys)
  ;; Do some sanity checking.
  (assert (and (keywordp facility)
               (get-facility facility)))
  (assert (and (keywordp maximum-priority)
               (get-priority maximum-priority)))
  (assert (or (null hostname)
              (stringp hostname)))
  (assert (or (null app-name)
              (stringp app-name)))
  (assert (or (null process-id)
              (stringp process-id))))

(defgeneric current-time (logger)
  (:documentation "Return values YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, FRACTION-OF-A-SECOND.")
  (:method ((logger rfc5424-logger))
    (multiple-value-bind (second minute hour day month year weekday daylight-p zone)
        (decode-universal-time (get-universal-time) 0) ; 0 for GMT
      (declare (ignore daylight-p zone weekday))
      (values year month day hour minute second nil))))

(defgeneric format-log (logger priority control &rest args)
  (:documentation "Log the simple message STRING according to the priority PRIORITY. Note that this function behaves like CL:FORMAT, so ~'s in the CONTROL string will be interpreted as such.

This should be used in the simplest of logging situations. For more complicated log messages that contain structured data, see the RFC-LOG macro.")
  (:method ((logger rfc5424-logger) priority control &rest args)
    (unless (< (get-priority (logger-maximum-priority logger)) (get-priority priority))
      (multiple-value-bind (year month day hour minute second fraction)
          (current-time logger)
        (with-output-to-string (stream)
          (write-rfc5424-syslog-message stream
                                        (logior (get-priority priority)
                                                ;; facility is already shifted by 3 bits
                                                (get-facility (logger-facility logger)))
                                        year
                                        month
                                        day
                                        hour
                                        minute
                                        second
                                        fraction
                                        (logger-hostname logger)
                                        (logger-app-name logger)
                                        (logger-process-id logger)
                                        nil ; msgid
                                        nil ; sd-elements
                                        (apply #'format nil control args)))))))

(defmacro rfc-log ((logger priority control &rest args) &body structured-data)
  "Log the message formed by CONTROL and ARGS to the logger LOGGER with priority PRIORITY. Structured data should be a list of structured data clauses of the form, where each clause has the form:
    (:MSGID <string>)

 or

    (<ID> <PARAM 1> <VALUE 1>
          <PARAM 2> <VALUE 2>
          ...)

The data <ID> and <PARAM n> are symbols, while <VALUE n> are strings. Both <ID> and <PARAM n> are *not* evaluated, while <VALUE n> are evaluated.

If :MSGID is provided, then this will be the RFC5424 msgid of the log message.

The logging will only happen of LOGGER does not exceed a specified maximum priority value."
  (assert (keywordp priority))
  ;; Change PRIORITY into its numerical value. This also provides
  ;; compile-time checking that PRIORITY is valid.
  (setf priority (get-priority priority))

  (let* ((msgid-form nil)
         (sd-forms
           ;; Parse out the structured data.
           (loop :for sd :in structured-data
                 :if (and (listp sd)
                          (= 2 (length sd))
                          (eql ':msgid (first sd)))
                   :do (setf msgid-form (second sd))
                 :else
                   :collect
                   (progn
                     ;; Check that it's a list.
                     (unless (alexandria:proper-list-p sd)
                       (error "In RFC-LOG, encountered a piece of structured data that's not a list."))
                     (unless (oddp (length sd))
                       (error "In RFC-LOG, encountered a malformed structured data specification. It ~
                                 should be a list of odd length."))
                     (destructuring-bind (id &rest params) sd
                       ;; FIND-SD-ID will error if it's invalid or unknown.
                       (with-slots (allow-other-params fields) (find-sd-id id)
                         ;; We found it, now we want to parse it all
                         ;; out. Buckle up for a wild ride.
                         (loop
                           :with seen := nil
                           :for (key val-form) :on params :by #'cddr
                           :collect (let* ((already-seen? (find key seen :test #'string=))
                                           (recognized? (find key
                                                              fields
                                                              :key #'structured-data-field-description-name
                                                              :test #'string=))
                                           ;; alias for readability.
                                           (field-descriptor recognized?))
                                      ;; Is it recognized?
                                      (unless (or recognized? allow-other-params)
                                        (error "Unrecognized field ~S in structured data ~S"
                                               key
                                               id))

                                      ;; Do we allow repeats?
                                      (when (and recognized?
                                                 already-seen?
                                                 (not (structured-data-field-description-repetitions-allowed-p field-descriptor)))
                                        (error "Found a repeated field ~S for the structured data ~S, and that's not allowed here."
                                               key
                                               id))
                                      (pushnew key seen :test #'string=)
                                      ;; Create the param constructor.
                                      `(make-param ',(string key)
                                                   ,val-form))
                             :into param-forms
                           :finally (return `(make-sd-element ',(string id) ,@param-forms)))))))))
    (alexandria:with-gensyms (year month day hour minute second fraction ; time
                                   stream)
      (alexandria:once-only (logger)
        ;; Generate the logging code. Do *NOT* evaluate anything or do
        ;; anything costly if our priority isn't correct.
        `(unless (< (get-priority (logger-maximum-priority ,logger)) ',priority)
           (multiple-value-bind (,year ,month ,day ,hour ,minute ,second ,fraction)
               (current-time ,logger)
             (with-output-to-string (,stream)
               (write-rfc5424-syslog-message ,stream
                                             (logior ',priority
                                                     ;; facility is already shifted by 3 bits
                                                     (get-facility (logger-facility ,logger)))
                                             ,year
                                             ,month
                                             ,day
                                             ,hour
                                             ,minute
                                             ,second
                                             ,fraction
                                             (logger-hostname ,logger)
                                             (logger-app-name ,logger)
                                             (logger-process-id ,logger)
                                             ,msgid-form
                                             (list ,@sd-forms)
                                             (format nil ,control ,@args)))))))))

