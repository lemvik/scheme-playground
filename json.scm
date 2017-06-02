; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; JSON Scheme library specification.
;;;;

(library (json (0 1 0))
  (export make-json-number
          make-json-string
          make-json-bool
          make-json-array
          make-json-object
          json-number-value
          json-string-value
          json-bool-value
          json-object-get
          json-object-set!
          json-array-get
          json-array-set!
          json-array-push!
          json-array-pop!
          json->string)
  (import (rnrs)
          (rvector))

  ;; Enumeration of possible json values. Constructor json-type-union is not exposed as
  ;; there is no sense in having value with set of types.
  (define-enumeration json-type
    (json-null json-bool json-number json-string json-object json-array)
    make-json-type)

  ;; Condition for types mismatch when invoking various methods on json values.
  (define-condition-type &json-type-mismatch &condition make-type-mismatch json-type-mismatch?
    (actual-type json-type-actual)
    (expected-type json-type-expected))

  ;; The type of JSON values - contains a tag for type checks and actual value.
  (define-record-type json-value
    (nongenerative)
    (fields (immutable type)
            (mutable value)))

  ;; Returns true if given value is of given type
  (define (is-of-type val type)
    (equal? (json-value-type val) type))

  ;; Defines a simple json constructor
  (define-syntax define-json-constructor
    (lambda (f)
      (syntax-case f ()
        [(d type)
         (let* ([type-symbol (symbol->string (syntax->datum #'type))]
                [constructor-name (string->symbol (string-append "make-" type-symbol))])
           (with-syntax ([name (datum->syntax #'d constructor-name)])
             #`(define (#,#'name val)
                 (make-json-value (json-type #,#'type) val))))])))


  ;; Constructs a JSON number.
  (define-json-constructor json-number)
  ;; Constructs a JSON string.
  (define-json-constructor json-string)
  ;; Constructs a JSON boolean.
  (define-json-constructor json-bool)
  ;; Constructs a JSON null value.
  (define (make-json-null)
    (make-json-value (json-type json-null) #f))

  ;; Constructs a JSON object value.
  (define (make-json-object)
    (make-json-value (json-type json-object)
                     (make-hashtable string-hash string=?)))

  ;; Determines initial array capacity.
  (define +default-array-capacity+ 5)

  ;; Constructs a JSON array value.
  (define (make-json-array)
    (make-json-value (json-type json-array)
                     (make-rvector +default-array-capacity+)))

  ;; Converts given object to JSON value. Only works for booleans, strings and numbers and empty lists
  ;; (empty list is converted to null).
  (define (object->json-value obj)
    (cond [(json-value? obj) obj]
          [(string? obj) (make-json-string obj)]
          [(number? obj) (make-json-number obj)]
          [(boolean? obj) (make-json-bool obj)]
          [(equal? obj '()) (make-json-null)]
          [else (raise (error 'object->json-value "Cannot convert value to JSON" obj))]))


  ;; Defines a predicate for type checking.
  (define-syntax define-type-predicate
    (lambda (f)
      (syntax-case f ()
        [(d type)
         (let* ([type-symbol (symbol->string (syntax->datum #'type))]
                [predicate-name (string->symbol (string-append "json-value-" type-symbol "?"))]
                [type-name (string->symbol (string-append "json-" type-symbol))])
           (with-syntax ([name (datum->syntax #'d predicate-name)]
                         [tname (datum->syntax #'d type-name)])
             #`(define (#,#'name val)
                 (and (json-value? val) (equal? (json-value-type val) (json-type #,#'tname))))))])))

  ;; Predicate to check if value is number.
  (define-type-predicate number)
  ;; Predicate to check if value is string.
  (define-type-predicate string)
  ;; Predicate to check if value is null.
  (define-type-predicate null)
  ;; Predicate to check if value is boolean.
  (define-type-predicate bool)
  ;; Predicate to check if value is object.
  (define-type-predicate object)
  ;; Predicate to check if value is array.
  (define-type-predicate array)

  ;; Checks that given value is json value and have appropriate type.
  ;; It's a syntax to avoid quiting the type.
  (define-syntax ensure-json-value-of-type
    (syntax-rules ()
      [(_ val type)
       (begin 
         (unless (json-value? val)
           (raise (error 'name "Expected JSON value" val)))
         (unless (is-of-type val (json-type type))
           (raise (make-type-mismatch (json-value-type val) (json-type type)))))]))

  ;; Syntax shortcut for JSON getters definition.
  (define-syntax define-json-getter
    (syntax-rules ()
      [(_ name type)
       (define (name val)
         (ensure-json-value-of-type val type)
         (json-value-value val))]))

  ;; Retrieves numeric value from JSON value
  (define-json-getter json-number-value json-number)
  ;; Retrieves string value from JSON value
  (define-json-getter json-string-value json-string)
  ;; Retrieves boolean value from JSON value
  (define-json-getter json-bool-value json-bool)

  ;; Retrieves value from JSON object. Returns #f if there is no value.
  (define (json-object-get object key)
    (ensure-json-value-of-type object json-object)
    (hashtable-ref (json-value-value object) key #f))

  ;; Sets a value in JSON object.
  (define (json-object-set! object key value)
    (ensure-json-value-of-type object json-object)
    (hashtable-set! (json-value-value object) key (object->json-value value)))

  ;; Retrieves value stored in a JSON array at given index.
  (define (json-array-get array index)
    (ensure-json-value-of-type array json-array)
    (rvector-ref (json-value-value array) index))

  ;; Sets a value in JSON array.
  (define (json-array-set! array index element)
    (ensure-json-value-of-type array json-array)
    (rvector-set! (json-value-value array) index (object->json-value element)))

  ;; Pushes a new value to JSON array.
  (define (json-array-push! array element)
    (ensure-json-value-of-type array json-array)
    (rvector-push! (json-value-value array) (object->json-value element)))

  ;; Pops a value off JSON array.
  (define (json-array-pop! array)
    (ensure-json-value-of-type array json-array)
    (rvector-pop! (json-value-value array)))

  ;; Escape JSON string.
  (define (json-escape-string str)
    str)

  ;; Converts JSON value to a string.
  (define (json->string val)
    (define (json-number->string n out)
      (put-string out (number->string (json-value-value n))))
    (define (json-string->string str out)
      (put-char out #\")
      (put-string out (json-escape-string (json-value-value str)))
      (put-char out #\"))
    (define (json-boolean->string b out)
      (put-string out (if (json-value-value b) "true" "false")))
    (define (json-null->string out)
      (put-string out "null"))
    (define (json-array->string arr out)
      (let ([separator ""])
        (put-char out #\[)
        (rvector-for-each (json-value-value arr)
                          (lambda (el)
                            (put-string out separator)
                            (json-value->string el out)
                            (set! separator ",")))
        (put-char out #\])))
    (define (json-object->string obj out)
      (let ([separator ""])
        (put-char out #\{)
        (let-values ([(keys values) (hashtable-entries (json-value-value obj))])
          (vector-for-each (lambda (k v)
                      (put-string out separator)
                      (put-char out #\")
                      (put-string out (json-escape-string k))
                      (put-char out #\")
                      (put-char out #\:)
                      (json-value->string v out)
                      (set! separator ","))
                    keys
                    values))
        (put-char out #\})))
    (define (json-value->string val out)
      (cond [(json-value-number? val) (json-number->string val out)]
            [(json-value-bool? val) (json-boolean->string val out)]
            [(json-value-string? val) (json-string->string val out)]
            [(json-value-null? val) (json-null->string out)]
            [(json-value-array? val) (json-array->string val out)]
            [(json-value-object? val) (json-object->string val out)]
            [else (raise (error 'json-value->string "Unknown JSON value type detected." val))]))
    (let-values ([(out done) (open-string-output-port)])
      (json-value->string val out)
      (done))))


  
