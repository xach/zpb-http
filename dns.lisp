;;;; dns.lisp
;;;; $Id: dns.lisp,v 1.5 2007/11/30 16:43:14 xach Exp $

(defpackage :dns
  (:use :cl)
  (:export :message
           :query
           :response
           :id
           :responsep
           :query-code
           :authoritativep
           :truncatedp
           :recursion-desired-p
           :recursion-available-p
           :response-code
           :question-records
           :add-question
           :answer-records
           :add-answer
           :authority-records
           :add-authority
           :additional-records
           :add-additional
           :question-count
           :answer-count
           :authority-count
           :additional-count
           :message-vector
           :data-record
           :question-record
           :resource-record
           :name
           :type-id
           :type-id-designator
           :class-id
           :class-id-designator
           :ttl
           :data

           :canonical-name

           :address

           :name-server

           :mail-exchange
           :preference
           :exchange

           :pointer
           :pointer-name

           :start-of-authority
           :main-name
           :responsible-name
           :serial
           :refresh
           :retry
           :expire
           :minimum

           :get-message
           :query-response-p
           :string-labels
           :labels-string
           :in-addr.arpa-labels
           :type-id-name
           :type-name-id
           :class-id-name
           :class-name-id
           :labels-equal

           :make-query
           :make-pointer-query))

(in-package :dns)

(defclass message ()
  ((id
    :initarg :id
    :initform (random #xFFFF)
    :accessor id)
   (truncatedp
    :documentation "The TC header flag."
    :initform nil
    :initarg :truncatedp
    :accessor truncatedp)
   (responsep
    :documentation "The QR header flag."
    :reader responsep
    :initform nil)
   (query-code
    :documentation "The OPCODE header value."
    :initform 0
    :initarg :query-code
    :accessor query-code)
   (recursion-desired-p
    :documentation "The RD header flag."
    :initform nil)
   (response-code
    :documentation "The RCODE header flag."
    :initform 0)
   (authoritativep
    :documentation "The RD header flag."
    :initform nil)
   (recursion-available-p
    :documentation "The RA header flag."
    :initform nil)
   (question-records
    :initform (make-array 1 :adjustable t :fill-pointer 0)
    :accessor question-records)
   (answer-records
    :initform nil)
   (authority-records
    :initform nil)
   (additional-records
    :initform nil)))

(defmethod print-object ((object message) stream)
  (print-unreadable-object (object stream :type t)
    (princ (id object) stream)))

(defclass query (message)
  ((responsep
    :initform nil)
   (recursion-desired-p
    :initform t
    :initarg :recursion-desired-p
    :accessor recursion-desired-p)))

(defclass response (message)
  ((responsep
    :initform t)
   (response-code
    :initarg :response-code
    :accessor response-code)
   (recursion-available-p
    :initarg :recursion-available-p
    :accessor recursion-available-p)
   (recursion-desired-p
    :initform nil
    :initarg :recursion-desired-p
    :accessor recursion-desired-p)
   (authoritativep
    :initarg :authoritativep
    :accessor authoritativep)
   (answer-records
    :initform (make-array 1 :adjustable t :fill-pointer 0)
    :accessor answer-records)
   (authority-records
    :initform (make-array 1 :adjustable t :fill-pointer 0)
    :accessor authority-records)
   (additional-records
    :initform (make-array 1 :adjustable t :fill-pointer 0)
    :accessor additional-records)))

(defgeneric question-count (message)
  (:method (message)
    (length (question-records message))))

(defgeneric answer-count (message)
  (:method (message)
    (length (answer-records message))))

(defgeneric authority-count (message)
  (:method (message)
    (length (authority-records message))))

(defgeneric additional-count (message)
  (:method (message)
    (length (additional-records message))))

(defclass data-record ()
  ((name
    :initarg :name
    :accessor name)
   (type-id
    :initarg :type-id
    :accessor type-id)
   (class-id
    :initarg :class-id
    :accessor class-id)))

(defclass question-record (data-record) ())

(defclass resource-record (data-record)
  ((ttl
    :initform 0
    :initarg :ttl
    :accessor ttl)
   (data
    :documentation "The RDATA field of a resource record."
    :initarg :data
    :accessor data)))

;;;
;;; Specific resource-record types
;;;

;;; First, plain functions that assist in getting from or putting to
;;; the RDATA octet vector. Each GET-* returns multiple values: the
;;; actual value requested and the offset after the size of the data
;;; fetched. The PUT-* functions return just the offset after.

(defun get-u32 (data offset)
  (values (logior (ash (aref data (+ offset 0)) 24)
                  (ash (aref data (+ offset 1)) 16)
                  (ash (aref data (+ offset 2))  8)
                  (ash (aref data (+ offset 3))  0))
          (+ offset 4)))

(defun put-u32 (data offset value)
  (setf (aref data (+ offset 0)) (ldb (byte 8 24) value)
        (aref data (+ offset 1)) (ldb (byte 8 16) value)
        (aref data (+ offset 2)) (ldb (byte 8  8) value)
        (aref data (+ offset 3)) (ldb (byte 8  0) value))
  (+ offset 4))

(defun get-s32 (data offset)
  (values (dpb (get-u32 data offset) (byte 32 0) -1)
          (+ offset 4)))

(defun put-s32 (data offset value)
  (put-u32 data offset value))

(defun get-u16 (data offset)
  (values (logior (ash (aref data (+ offset 0)) 8)
                  (ash (aref data (+ offset 1)) 0))
          (+ offset 2)))

(defun put-u16 (data offset value)
  (setf (aref data (+ offset 0)) (ldb (byte 8 8) value)
        (aref data (+ offset 1)) (ldb (byte 8 0) value))
  (+ offset 2))

(defun get-labels (data offset)
  (let ((labels '())
        (end nil))
    (loop
     (let ((length (aref data offset)))
       (when (zerop length)
         (return (values (nreverse labels) (1+ (or end offset)))))
       (if (>= length #b11000000)
           (setf end (or end (1+ offset))
                 offset (logandc2 (get-u16 data offset) #xC000))
           (progn
             (incf offset)
             (push (subseq data offset (+ offset length)) labels)
             (incf offset length)))))))

(defun put-labels (data offset labels)
  (dolist (label labels)
    (setf (aref data offset) (length label))
    (incf offset)
    (replace data label :start1 offset)
    (incf offset (length label)))
  (setf (aref data offset) 0)
  (1+ offset))

(defgeneric upgraded-resource-record-class (type-id)
  (:method (type-id)
    nil))

(defgeneric initialize-slots-from-data (resource-record data offset)
  (:method (resource-record data offset)
    nil))
(defgeneric initialize-data-from-slots (resource-record))
(defgeneric get-data-value (type data offset))
(defgeneric put-data-value (type data offset value))

(defmacro define-data-value (name &body options)
  (let ((result-form '()))
    (dolist (option options (cons 'progn result-form))
      (destructuring-bind (type lambda-list &body body)
          option
        (ecase type
          (:reader
           (push
            (destructuring-bind (data offset) lambda-list
              `(defmethod get-data-value ((type (eql ',name)) ,data ,offset)
                ,@body))
            result-form))
          (:writer
           (push
            (destructuring-bind (data offset value) lambda-list
              `(defmethod put-data-value ((type (eql ',name))
                                          ,data ,offset ,value)
                ,@body))
            result-form)))))))

(define-data-value ipv4-address
  (:reader (data offset)
     (get-u32 data offset))
  (:writer (data offset value)
     (put-u32 data offset value)))

(define-data-value domain-name
  (:reader (data offset)
     (get-labels data offset))
  (:writer (data offset value)
     (put-labels data offset value)))
    
(define-data-value u16
  (:reader (data offset)
     (get-u16 data offset))
  (:writer (data offset value)
     (put-u16 data offset value)))

(define-data-value u32
  (:reader (data offset)
     (get-u32 data offset))
  (:writer (data offset value)
     (put-u32 data offset value)))


(defmacro define-resource-record-class ((name &key type-id (class-id 1))
                                        slot-definitions)
  (declare (ignore class-id))
  (labels ((keywordify (symbol)
             (intern (symbol-name symbol) :keyword))
           (slot-spec (symbol)
             `(,symbol
               :initform nil
               :initarg ,(keywordify symbol)
               :accessor ,symbol))
           (slot-initializer (slot type data offset)
             `(setf (values (,slot resource-record) ,offset)
               (get-data-value ',type ,data ,offset))))
    (let ((slots (mapcar #'first slot-definitions))
          (types (mapcar #'second slot-definitions)))
      `(progn
        (eval-when (:compile-toplevel :load-toplevel :execute)
          (defclass ,name (resource-record)
            ,(mapcar #'slot-spec slots)))
        (defmethod upgraded-resource-record-class ((type-id (eql ,type-id)))
          ',name)
        (defmethod initialize-slots-from-data ((resource-record ,name) data offset)
          ,@(loop for slot in slots
                  for type in types
                  collect (slot-initializer slot type 'data 'offset)))))))

(define-resource-record-class (address :type-id 1)
  ((address ipv4-address)))

(define-resource-record-class (name-server :type-id 2)
  ((name-server domain-name)))

(define-resource-record-class (canonical-name :type-id 5)
  ((canonical-name domain-name)))

(define-resource-record-class (start-of-authority :type-id 6)
  ((main-name domain-name)
   (responsible-name domain-name)
   (serial u32)
   (refresh u32)
   (retry u32)
   (expire u32)
   (minimum u32)))

(define-resource-record-class (pointer :type-id 12)
  ((pointer-name domain-name)))

(define-resource-record-class (mail-exchange :type-id 15)
  ((preference u16)
   (exchange domain-name)))

(defgeneric add-question (record message))
(defgeneric add-answer (record message))
(defgeneric add-authority (record message))
(defgeneric add-additional (record message))

(defmethod add-question ((record question-record) message)
  (vector-push-extend record (question-records message)))

(defmethod add-answer ((record resource-record) (message response))
  (vector-push-extend record (answer-records message)))

(defmethod add-authority ((record name-server) (message response))
  (vector-push-extend record (authority-records message)))

(defmethod add-authority ((record start-of-authority) (message response))
  (vector-push-extend record (authority-records message)))

(defmethod add-additional ((record resource-record) (message response))
  (vector-push-extend record (additional-records message)))


(defgeneric get-resource-record (data offset))

(defmethod get-resource-record (data offset)
  (let (name type class ttl rdlength rdata)
    (setf (values name offset) (get-labels data offset)
          (values type offset) (get-u16 data offset)
          (values class offset) (get-u16 data offset)
          (values ttl offset) (get-u32 data offset)
          (values rdlength offset) (get-u16 data offset)
          rdata (subseq data offset (+ offset rdlength)))
    (let ((rr
           (make-instance (or (upgraded-resource-record-class type)
                              'resource-record)
                          :name name
                          :type-id type
                          :class-id class
                          :ttl ttl
                          :data rdata)))
      (initialize-slots-from-data rr data offset)
      (values rr (+ offset rdlength)))))

(defgeneric get-question-record (data offset))

(defmethod get-question-record (data offset)
  (let (name type class)
    (setf (values name offset) (get-labels data offset)
          (values type offset) (get-u16 data offset)
          (values class offset) (get-u16 data offset))
    (values (make-instance 'question-record
                           :name name
                           :type-id type
                           :class-id class)
            offset)))

;;; Parse a packet and produce a complete message of the appropriate
;;; class

(defconstant +header-data-length+ 12)

(defgeneric get-message (data))

(defmethod get-message (data)
  (let ((id (get-u16 data 0))
        (flags (get-u16 data 2))
        (qdcount (get-u16 data 4))
        (ancount (get-u16 data 6))
        (nscount (get-u16 data 8))
        (arcount (get-u16 data 10))
        qr opcode aa tc rd ra rcode message)
    (setf (values qr opcode aa tc rd ra rcode)
          (values (ldb (byte 1 15) flags)
                  (ldb (byte 4 11) flags)
                  (ldb (byte 1 10) flags)
                  (ldb (byte 1  9) flags)
                  (ldb (byte 1  8) flags)
                  (ldb (byte 1  7) flags)
                  (ldb (byte 4  0) flags)))
    (if (zerop qr)
        (setf message (make-instance 'query
                                     :id id
                                     :query-code opcode
                                     :truncatedp (plusp tc)
                                     :recursion-desired-p (plusp rd)))
        (setf message (make-instance 'response
                                     :id id
                                     :query-code opcode
                                     :authoritativep (plusp aa)
                                     :truncatedp (plusp tc)
                                     :recursion-available-p (plusp ra)
                                     :recursion-desired-p (plusp rd)
                                     :response-code rcode)))
    (let ((offset +header-data-length+)
          record)
      (dotimes (i qdcount)
        (setf (values record offset) (get-question-record data offset))
        (add-question record message))
      (dotimes (i ancount)
        (setf (values record offset) (get-resource-record data offset))
        (add-answer record message))
      (dotimes (i nscount)
        (setf (values record offset) (get-resource-record data offset))
        (add-authority record message))
      (dotimes (i arcount message)
        (setf (values record offset) (get-resource-record data offset))
        (add-additional record message)))))
  
;;;
;;; Creating an octet vector from from a query object
;;;

(defun labels-data-length (labels)
  (1+ (loop for label in labels summing (1+ (length label)))))

(defgeneric data-length (message))

(defmethod data-length ((message query))
  (+ +header-data-length+
     (loop for record across (question-records message)
           summing (+ (labels-data-length (name record))
                      ;; type-id size
                      2
                      ;; class-id size
                      2))))

(defgeneric message-vector (message))

(defgeneric message-flags (message))

(defgeneric put-record (data offset record))

(defmethod put-record (data offset (record question-record))
  (setf offset (put-labels data offset (name record)))
  (setf offset (put-u16 data offset (type-id record)))
  (setf offset (put-u16 data offset (class-id record)))
  offset)

(defmethod message-vector ((message query))
  (flet ((flag-value (flag)
           (if flag 1 0)))
    (let ((vector (make-array (data-length message)
                              :initial-element 0
                              :element-type '(unsigned-byte 8)))
          (qr 0)
          (opcode (query-code message))
          (aa 0)
          ;; FIXME: this should be done by checking DATA-LENGTH
          (tc (flag-value (truncatedp message)))
          (rd (flag-value (recursion-desired-p message)))
          (ra 0)
          (rcode 0))
      (put-u16 vector 0 (id message))
      (put-u16 vector 2 (logior (dpb qr     (byte 1 15) 0)
                                (dpb opcode (byte 4 11) 0)
                                (dpb aa     (byte 1 10) 0)
                                (dpb tc     (byte 1  9) 0)
                                (dpb rd     (byte 1  8) 0)
                                (dpb ra     (byte 1  7) 0)
                                (dpb rcode  (byte 4  0) 0)))
      (put-u16 vector 4 (question-count message))
      (put-u16 vector 6 0)
      (put-u16 vector 8 0)
      (put-u16 vector 10 0)
      (let ((offset +header-data-length+)
            (questions (question-records message)))
        (dotimes (i (question-count message))
          (setf offset (put-record vector offset (aref questions i)))))
      vector)))


;;;
;;; Matching up a response to a request
;;;

(defgeneric query-response-p (query response))

(defmethod query-response-p ((query query) (response response))
  (and (= (id query)
          (id response))
       (= (question-count query)
          (question-count response))))


;;;
;;; Convenience
;;;

(defun string-labels (name)
  "Convert a dotted string to a list of labels."
  (let ((labels '())
        (label (make-array (length name) :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (flet ((save ()
             (when (plusp (fill-pointer label))
               (push (copy-seq label) labels)
               (setf (fill-pointer label) 0))))
      (dotimes (i (length name))
        (let* ((char (char name i))
               (code (char-code char)))
          (if (char= char #\.) 
              (save) 
              (vector-push code label))))
      (save)
      (nreverse labels))))

(defun labels-string (labels)
  (with-output-to-string (stream)
    (dolist (label labels)
      (write-string (sb-ext:octets-to-string label :external-format :utf-8)
                    stream)
      (write-char #\. stream))))

(defun in-addr.arpa-labels (ip-string)
  (let ((labels (string-labels ip-string))
        (suffix (string-labels "in-addr.arpa")))
    (nconc (nreverse labels) suffix)))

(defvar *types*
  '((:A . 1)
    (:NS . 2)
    (:MD . 3)
    (:MF . 4)
    (:CNAME . 5)
    (:SOA . 6)
    (:MB . 7)
    (:MG . 8)
    (:MR . 9)
    (:NULL . 10)
    (:WKS . 11)
    (:PTR . 12)
    (:HINFO . 13)
    (:MINFO . 14)
    (:MX . 15)
    (:TXT . 16)
    (:AXFR . 252)
    (:MAILB . 253)
    (:MAILA . 254)
    (:ANY . 255)))

(defvar *classes*
  '((:IN . 1)
    (:CS . 2)
    (:CH . 3)
    (:HS . 4)
    (:ANY . 255)))

(defun type-id-name (type-id)
  (car (rassoc type-id *types*)))

(defun type-name-id (type-name)
  (cdr (assoc type-name *types* :test #'string-equal)))

(defun type-id-designator (designator)
  (etypecase designator
    (integer designator)
    ((or symbol string) (type-name-id designator))))

(defun class-id-name (class-id)
  (car (rassoc class-id *classes*)))

(defun class-name-id (class-name)
  (cdr (assoc class-name *classes* :test #'string-equal)))

(defun class-id-designator (designator)
  (etypecase designator
    (integer designator)
    ((or symbol string) (class-name-id designator))))


(defun labels-equal (a b)
  (flet ((ascii-code-equal (a b)
           (= (+ a (if (<= a 65 90) #x20 0))
              (+ b (if (<= b 65 90) #x20 0)))))
    (loop
     (let ((label-a (pop a))
           (label-b (pop b)))
       (cond ((not (or label-a label-b))
              (return t))
             ((or (not label-a) (not label-b))
              (return nil))
             ((not (mismatch label-a label-b :test #'ascii-code-equal)))
             (t
              (return nil)))))))

;;;
;;; Creating a query
;;;

(defun make-query (name &key (type :a) (class :in))
  (let ((query (make-instance 'query))
        (question (make-instance 'question-record
                                 :name (string-labels name)
                                 :class-id (class-id-designator class)
                                 :type-id (type-id-designator type))))
    (add-question question query)
    query))

(defun make-pointer-query (ip-address)
  (let ((query (make-instance 'query))
        (question (make-instance 'question-record
                                 :name (in-addr.arpa-labels ip-address)
                                 :type-id (type-id-designator :ptr)
                                 :class-id (class-id-designator :in))))
    (add-question question query)
    query))
