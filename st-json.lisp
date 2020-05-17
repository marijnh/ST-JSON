(defpackage :st-json
  (:use :common-lisp)
  (:export #:read-json #:read-json-as-type #:read-json-from-string
           #:write-json #:write-json-to-string #:write-json-element
           #:as-json-bool #:from-json-bool
           #:json-bool #:json-null
           #:jso #:getjso #:getjso* #:mapjso
           #:json-error #:json-type-error #:json-parse-error
           #:json-eof-error
           #:*decode-objects-as*
           #:*allow-comments*
           #:*script-tag-hack*
           #:*output-literal-unicode*))

(in-package :st-json)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize*
    '(optimize (speed 3) (safety 0) (space 1) (debug 1) (compilation-speed 0))))

;; Boolean types. It is hard to see what is meant by NIL when encoding
;; a lisp value -- false or [] -- so :false and :true are used instead
;; of T and NIL.
(defun as-json-bool (value)
  "Convert a generalised boolean to a :true/:false keyword."
  (if value :true :false))
(defun from-json-bool (value)
  "Convert :true or :false to its boolean equivalent."
  (ecase value (:true t) (:false nil)))

;; Types that might be useful when checking the type of input.
(deftype json-bool () '(member :true :false))
(deftype json-null () '(eql :null))

;; These are used to represent JS objects on the Lisp side -- hash
;; tables are too heavyweight on some implementations.
(defstruct jso alist)
(defun jso (&rest fields)
  "Create a JS object. Arguments should be alternating labels and values."
  (make-jso :alist (loop :for (key val) :on fields :by #'cddr
                           :collect (cons key val))))

;; A hash-table-like interface for JS objects.
(defun getjso (key map)
  "Fetch a value from a JS object. Returns a second value like
gethash."
  (let ((pair (assoc key (jso-alist map) :test #'string=)))
    (values (cdr pair) (and pair t))))
(defun (setf getjso) (val key map)
  "Store a value in a JS object."
  (let ((pair (assoc key (jso-alist map) :test #'string=)))
    (if pair
        (setf (cdr pair) val)
        (prog1 val (push (cons key val) (jso-alist map))))))
(defun mapjso (func map)
  "Iterate over the key/value pairs in a JS object."
  (loop :for (key . val) :in (jso-alist map)
        :do (funcall func key val)))

(defmacro getjso* (keys jso)
  (let ((last (position #\. keys :from-end t)))
    (if last
        `(getjso ,(subseq keys (1+ last))
                 (getjso* ,(subseq keys 0 last) ,jso))
        `(getjso ,keys ,jso))))

;; Reader

(defparameter *decode-objects-as* :jso
  "Valid values: :jso :hashtable
  Controls how js objects should be decoded. :jso means decode to internal struct which
  can be processed by getjso, mapjso etc. :hashtable means decode as hash tables.")

(defparameter *allow-comments* nil
  "Non-nil means ignore comments when parsing.")

(define-condition json-error (simple-error) ())
(define-condition json-parse-error (json-error) ())
(define-condition json-eof-error (json-parse-error) ())
(define-condition json-write-error (json-error) ())
(define-condition json-type-error (json-error) ())
(defun raise (type format &rest args)
  (error type :format-control format :format-arguments args))

(defvar *reading-slot-name* nil)

(defun is-whitespace (char)
  (member char '(#\space #\newline #\return #\tab)))

(defun ends-atom (char)
  (or (is-whitespace char) (member char '(#\) #\] #\} #\, #\:))))

(defun skip-cpp-comment (stream)
  (declare #.*optimize*)
  ;; Skip the first slash.
  (read-char stream)
  (unless (char= #\/ (peek-char nil stream nil))
    (raise 'json-parse-error "Unexpected input '/~A'." (read-char stream)))
  ;; Skip the second slash.
  (read-char stream)
  (loop :while (not (member (peek-char nil stream nil)
                            '(#\newline #\return)))
        :do (read-char stream))
  (skip-whitespace stream))

(defun skip-whitespace (stream)
  (declare #.*optimize*)
  (loop :for char := (peek-char nil stream nil)
        :while (cond
                 ((is-whitespace char) (read-char stream))
                 ((and *allow-comments* (char= #\/ char))
                  (skip-cpp-comment stream)))))

(defun at-eof (stream)
  (eql (peek-char nil stream nil :eof) :eof))

(defgeneric read-json (in &optional junk-allowed-p)
  (:documentation "Read a JSON-encoded value from a stream or a string."))

(defmethod read-json ((in stream) &optional (junk-allowed-p t))
  (let ((value (read-json-element in)))
    (skip-whitespace in)
    (unless (or junk-allowed-p (at-eof in))
      (raise 'json-parse-error "Unused characters at end of input."))
    value))

(defmethod read-json ((in string) &optional (junk-allowed-p nil))
  (with-input-from-string (stream in)
    (read-json stream junk-allowed-p)))

(defun read-json-from-string (string &key (start 0) end junk-allowed-p)
  (let (index value)
    (with-input-from-string (stream string :index index :start start :end end)
      (setf value (read-json stream junk-allowed-p)))
    (values value index)))

(defun read-json-as-type (source type)
  "Read a JSON value and assert the result to be of a given type.
Raises a json-type-error when the type is wrong."
  (let ((val (read-json source)))
    (if (typep val type)
        val
        (raise 'json-type-error "JSON input '~A' is not of expected type ~A." source type))))

(defun read-json-element (stream)
  (declare #.*optimize*)
  (skip-whitespace stream)
  (case (peek-char nil stream nil :eof)
    (:eof (raise 'json-eof-error "Unexpected end of input."))
    ((#\" #\') (read-json-string stream))
    (#\[ (read-json-list stream))
    (#\{ (read-json-object stream))
    (t (read-json-atom stream))))

(defun read-json-string (stream)
  (declare #.*optimize*)
  (labels ((interpret (char)
             (if (eql char #\\)
                 (let ((escaped (read-char stream)))
                   (case escaped
                     (#\u (read-unicode))
                     (#\b #\backspace) (#\n #\newline) (#\r #\return)
                     (#\t #\tab) (#\f #\page) (t escaped)))
                 char))
           (read-unicode ()
             ;; refer to ECMA-404, strings.
             (flet ((read-code-point ()
                      (the fixnum
                           (loop :for pos :from 0 :below 4
                                 :for weight :of-type fixnum := #.(expt 16 3) :then (ash weight -4)
                                 :for digit := (digit-char-p (read-char stream) 16)
                                 :do (unless digit (raise 'json-parse-error "Invalid unicode constant in string."))
                                 :sum (* digit weight))))
                    (expect-char (char)
                      (let ((c (read-char stream)))
                        (assert (char= char c) (c)
                                "Expecting ~c, found ~c instead." char c))))
               (let ((code-point (read-code-point)))
                 (code-char
                  (if (<= #xD800 code-point #xDBFF)
                      (let ((utf-16-high-surrogate-pair code-point))
                        (expect-char #\\)
                        (expect-char #\u)
                        (let ((utf-16-low-surrogate-pair (read-code-point)))
                          (declare (type fixnum utf-16-low-surrogate-pair))
                          (assert (<= #xDC00 utf-16-low-surrogate-pair #xDFFF)
                                  (utf-16-low-surrogate-pair)
                                  "Unexpected UTF-16 surrogate pair: ~a and ~a."
                                  utf-16-high-surrogate-pair
                                  utf-16-low-surrogate-pair)
                          (+ #x010000
                             (ash (- utf-16-high-surrogate-pair #xD800) 10)
                             (- utf-16-low-surrogate-pair #xDC00))))
                      code-point))))))
    (with-output-to-string (out)
      (handler-case
          (loop :with quote :of-type character := (read-char stream)
                :for next :of-type character := (read-char stream)
                :until (eql next quote)
                :do (write-char (interpret next) out))
        (end-of-file () (raise 'json-eof-error "Encountered end of input inside string constant."))))))

;;; (st-json:read-json-from-string "\"In JSON, 𝄞 can be encoded/escaped like this: \\uD834\\uDD1E.\"")

(defun gather-comma-separated (stream end-char obj-name gather-func)
  (declare #.*optimize*)
  (declare (type character end-char))
  (declare (type function gather-func))
  ;; Throw away opening char
  (read-char stream)
  (let ((finished nil))
    (loop
     (skip-whitespace stream)
     (let ((next (peek-char nil stream nil #\nul)))
       (declare (type character next))
       (when (eql next #\nul)
         (raise 'json-eof-error "Encountered end of input inside ~A." obj-name))
       (when (eql next end-char)
         (read-char stream)
         (return))
       (when finished
         (raise 'json-parse-error "Comma or end of ~A expected, found '~A'" obj-name next)))
     (funcall gather-func)
     (skip-whitespace stream)
     (if (eql (peek-char nil stream nil) #\,)
         (read-char stream)
         (setf finished t)))))

(defun read-json-list (stream)
  (declare #.*optimize*)
  (let ((accum ()))
    (gather-comma-separated
     stream #\] "list"
     (lambda ()
       (push (read-json-element stream) accum)))
    (nreverse accum)))

(defun read-json-object (stream)
  (declare #.*optimize*)
  (ecase *decode-objects-as*
    ((:jso :alist)
     (let ((accum ()))
       (gather-comma-separated
        stream #\} "object literal"
        (lambda ()
          (let ((slot-name (let ((*reading-slot-name* t)) (read-json-element stream))))
            (unless (or (typep slot-name 'string) (typep slot-name 'number))
              (raise 'json-parse-error "Invalid slot name in object literal: ~A" slot-name))
            (skip-whitespace stream)
            (when (not (eql (read-char stream nil) #\:))
              (raise 'json-parse-error "Colon expected after '~a'." slot-name))
            (push (cons slot-name (read-json-element stream)) accum))))
       (if (eq *decode-objects-as* :jso)
           (make-jso :alist (nreverse accum))
           (nreverse accum))))
     (:hashtable
      (let ((accum (make-hash-table :test #'equal)))
        (gather-comma-separated
         stream #\} "object literal"
         (lambda ()
           (let ((slot-name (let ((*reading-slot-name* t)) (read-json-element stream))))
             (unless (or (typep slot-name 'string) (typep slot-name 'number))
               (raise 'json-parse-error "Invalid slot name in object literal: ~A" slot-name))
             (skip-whitespace stream)
             (when (not (eql (read-char stream nil) #\:))
               (raise 'json-parse-error "Colon expected after '~a'." slot-name))
             (setf (gethash slot-name accum) (read-json-element stream)))))
        accum))))

(defun looks-like-a-number (string)
  (declare #.*optimize*)
  (let ((string (coerce string 'simple-string)))
    (every (lambda (char)
             (or (digit-char-p char)
                 (member char '(#\e #\E #\. #\- #\+))))
           string)))

(defun read-json-atom (stream)
  (declare #.*optimize*)
  (let ((accum (make-array 0 :element-type 'character :fill-pointer 0 :adjustable t)))
    (loop
     (let ((next (peek-char nil stream nil :eof)))
       (when (or (ends-atom next) (eql next :eof))
         (return))
       (vector-push-extend next accum)
       (read-char stream)))
    (let ((number-val (and (looks-like-a-number accum)
                           (ignore-errors (read-from-string accum)))))
      (cond ((numberp number-val) number-val)
            ((string= accum "false") :false)
            ((string= accum "true") :true)
            ((string= accum "null") :null)
            ((string= accum "undefined") :null)
            ((and *reading-slot-name*
                  (every (lambda (c)
                           (declare (type character c))
                           (or (alphanumericp c) (eql c #\_) (eql c #\$)))
                         accum))
             accum)
            (t (raise 'json-parse-error "Unrecognized value in JSON data: ~A" accum))))))

;; Writer

(defparameter *script-tag-hack* nil
  "Bind this to T when writing JSON that will be written to an HTML
  document. It prevents '</script>' from occurring in strings by
  escaping any slash following a '<' character.")

(defparameter *output-literal-unicode* nil
  "Bind this to T in order to reduce the use of \uXXXX Unicode escapes,
  by emitting literal characters (encoded in UTF-8). This may help
  reduce the parsing effort for any recipients of the JSON output, if
  they can already read UTF-8, or else, they'll need to implement
  complex unicode (eg UTF-16 surrogate pairs) escape parsers.")

(defun write-json-to-string (element)
  "Write a value's JSON representation to a string."
  (with-output-to-string (out)
    (write-json element out)))

(defun write-json (element stream)
  "Write a value's JSON representation to a stream."
  (let ((*print-pretty* nil))
    (write-json-element element stream)
    (values)))

(defgeneric write-json-element (element stream)
  (:method (element stream)
    (declare (ignore stream))
    (raise 'json-write-error "Can not write object of type ~A as JSON." (type-of element)))
  (:documentation "Method used for writing values of a specific type.
  You can specialise this for your own types."))

(defmethod write-json-element ((element symbol) stream)
  (declare #.*optimize*)
  (ecase element
    ((nil) (write-string "[]" stream))
    ((t :true) (write-string "true" stream))
    (:false (write-string "false" stream))
    ((:null :undefined) (write-string "null" stream))))

(defmethod write-json-element ((element string) stream)
  (declare #.*optimize* (stream stream))
  (let ((element (coerce element 'simple-string)))
    (write-char #\" stream)
    (loop :for prev := nil :then ch
       :for ch :of-type character :across element :do
       (let ((code (char-code ch)))
         (declare (fixnum code))
         (if (or (<= 0 code #x1f)
                 (<= #x7f code #x9f))
             (case code
               (#.(char-code #\backspace) (write-string "\\b" stream))
               (#.(char-code #\newline)   (write-string "\\n" stream))
               (#.(char-code #\return)    (write-string "\\r" stream))
               (#.(char-code #\page)      (write-string "\\f" stream))
               (#.(char-code #\tab)       (write-string "\\t" stream))
               (t                         (format stream "\\u~4,'0x" code)))
             (case code
               (#.(char-code #\/)  (when (and (eql prev #\<) *script-tag-hack*)
                                     (write-char #\\ stream))
                                   (write-char ch stream))
               (#.(char-code #\\)  (write-string "\\\\" stream))
               (#.(char-code #\")  (write-string "\\\"" stream))
               (t                  (cond ((< #x1F code #x7F)
                                          (write-char ch stream))
                                         ((and (< #x9F code #x10000)
                                               (not *output-literal-unicode*))
                                          (format stream "\\u~4,'0x" code))
                                         ((and (< #x10000 code #x1FFFF)
                                               (not *output-literal-unicode*))
                                          (let ((c (- code #x10000)))
                                            (format stream "\\u~4,'0x\\u~4,'0x"
                                                    (logior #xD800 (ash c -10))
                                                    (logior #xDC00 (logand c #x3FF)))))
                                         (t
                                          (write-char ch stream))))))))
    (write-char #\" stream)))

#+nil
(let ((st-json:*script-tag-hack* t))
  (st-json:write-json-to-string "Test 𝄞 ⇓ 	<tag>
</tag>"))
;; ==> "\"Test \\uD834\\uDD1E \\u21D3 \\t<tag>\\n<\\/tag>\""

(defmethod write-json-element ((element integer) stream)
  (write element :stream stream))

(defmethod write-json-element ((element real) stream)
  (format stream "~,,,,,,'eE" element))

(defmethod write-json-element ((element hash-table) stream)
  (declare #.*optimize*)
  (write-json-element
   (make-jso :alist (loop :for key :being :the :hash-keys :of element :using (hash-value val)
                          :collect (cons key val)))
   stream))

(defmethod write-json-element ((element jso) stream)
  (declare #.*optimize*)
  (write-char #\{ stream)
  (loop :for (key . val) :in (jso-alist element)
        :for first := t :then nil
        :unless first :do (write-char #\, stream)
        :do (write-json-element key stream)
        :do (write-char #\: stream)
        :do (write-json-element val stream))
  (write-char #\} stream))

(defmethod write-json-element ((element list) stream)
  (declare #.*optimize*)
  (write-char #\[ stream)
  (let ((first t))
    (dolist (part element)
     (if first
         (setf first nil)
         (write-char #\, stream))
     (write-json-element part stream)))
  (write-char #\] stream))
