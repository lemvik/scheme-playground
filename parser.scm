; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; Simple stream parser for JSON files.
;;;;

(library (json-parser (0 1 0))
  (export parse-json)
  (import (rnrs)
          (format)
          (stack)
          (json)
          (json-tokenizer))

  ;; Condition that fires up when there is a parsing error.
  (define-condition-type &json-parse-error &condition make-parse-error parse-error?
    (expected parse-error-expected)
    (actual parse-error-actual)
    (invariant-violated json-invariant-violated))

  ;; Enumeration describing all possible parse states.
  (define-enumeration parse-state
    (state-value state-comma state-colon state-key)
    make-parse-state)

  ;; Parses JSON coming out of some source
  (define (parse-json in)
    (let ([value #f]                                      ; Root value that will be eventually returned
          [being-built (make-stack)]                      ; Stack of containers (array/object) being built, part of state machine
          [state (make-stack (parse-state state-value))]  ; Stack representing states JSON document reader state machine 
          [keys (make-stack)])                            ; Stack for object keys being built (stack because we can have nested objects
                                                          ; level in this stack == level of nesting)
      (letrec-syntax ([expecting (syntax-rules ()
                                   [(_ (t1 ...) e1 ...)
                                    (let ([actual (stack-top state)])
                                      (if (or (equal? (parse-state t1) actual) ...)
                                          (begin e1 ...)
                                          (raise (make-parse-error (list (parse-state t1) ...) actual "States mismatch"))))]
                                   [(_ t e1 ...)
                                    (expecting (t) e1 ...)])]
                      [state-case (syntax-rules ()
                                    [(_ (s1 e1 ...) ...)
                                     (let ([st (current-state)])
                                       (case st 
                                         [(parse-state s1) e1 ...]
                                         ...
                                         [else (raise (make-parse-error (list (parse-state s1) ...) st "States mismatch"))]))])])
        ; Pop one node off states state machine.
        (define (state-pop!)
          (stack-pop! state))
        ; Push state onto states state machine.
        (define (state-push! st)
          (stack-push! state st))
        ; Returns current state of the machine.
        (define (current-state)
          (stack-top state))

        ; Returns reference to object being currently built
        (define (being-built-ref)
          (stack-top being-built))
        ; Checks if currently built object matches given predicate
        (define (building? pred?)
          (and (not (stack-empty? being-built))
               (pred? (being-built-ref))))
        ; Checks if we are building an array 
        (define (building-array?)
          (building? json-value-array?))
        ; Checks if we are building an object
        (define (building-object?)
          (building? json-value-object?))

        ; When we've attached a value, we need to update
        ; the state machine - if we are in an array or an object - we expect to see comma next.
        (define (value-attached!)
          (when (or (building-array?) (building-object?))
            (state-push! (parse-state state-comma))))
        
        ; Attaches value to current attachment point:
        ; 1. If we have no value at all - this value becomes the root
        ; 2. If we are building an object or an array - attach to that
        ;    using appropriate method
        (define (attach! v)
          (cond [(not value)
                 (set! value v)]
                [(building-array?)
                 (json-array-push! (being-built-ref) v)]
                [(building-object?)
                 (json-object-set! (being-built-ref) (stack-pop! keys) v)]
                [else (assert #f)]))

        ; If we encounter a primitive value - we check that we are
        ; expecting it, if so - attach it to the container being built
        (define (on-primitive! v)
          (expecting state-value
            (attach! v)
            (state-pop!)
            (value-attached!)))

        ; Null is a primitive
        (define (on-null!)
          (on-primitive! (make-json-null)))
        ; Number is a primitive
        (define (on-number! n)
          (on-primitive! (make-json-number n)))
        ; Boolean is a primitive
        (define (on-boolean! b)
          (on-primitive! (make-json-bool b)))
        ; Although string is a primitive, it can act as a key in object - check if
        ; we are in an object and expect key - the branch accordingly
        (define (on-string! str)
          (let ([expected (stack-top state)])
            (state-pop!)
            (cond [(equal? expected (parse-state state-key))
                   (stack-push! keys str)
                   (state-push! (parse-state state-colon))]
                  [(equal? expected (parse-state state-value))
                   (attach! (make-json-string str))
                   (value-attached!)]
                  [else (assert #f)])))

        ; Array is a container - we push it as acurrent attachment point and expect value to arrive.
        (define (on-array-start!)
          (expecting state-value
            (let ([arr (make-json-array)])
              (attach! arr)
              (state-push! (parse-state state-value))
              (stack-push! being-built arr))))
        ; If array is empty - we were expecting a value, if not - a comma 
        (define (on-array-end!)
          (assert (building-array?))
          (state-case
           (state-value
            (unless (json-array-empty? (being-built-ref))
              (raise (make-parse-error (parse-state state-value) (current-state) "Trailing comma is not allowed in arrays."))))
           (state-comma #t)) 
          (state-pop!)
          (state-pop!)
          (stack-pop! being-built)
          (value-attached!))
        (define (on-object-start!)
          (expecting state-value
            (let ([obj (make-json-object)])
              (attach! obj)
              (state-push! (parse-state state-key))
              (stack-push! being-built obj))))
        (define (on-object-end!)
          (assert (building-object?))
          (state-case
           (state-key
            (unless (json-object-empty? (being-built-ref))
              (raise (make-parse-error (parse-state state-value) (current-state) "Trailing comma is not allowed in objects."))))
           (state-comma #t)) 
          (state-pop!)
          (state-pop!)
          (stack-pop! being-built)
          (value-attached!))
        (define (on-comma!)
          (expecting state-comma
                     (state-pop!)
                     (cond [(building-array?) (state-push! (parse-state state-value))]
                           [(building-object?) (state-push! (parse-state state-key))]
                           [else (assert #f)])))
        (define (on-colon!)
          (expecting state-colon
                     (assert (building-object?))
                     (state-pop!)
                     (state-push! (parse-state state-value))))
        (define (need-more?)
          (not (stack-empty? state)))
        (tokenize in
                  on-null!
                  on-number!
                  on-boolean!
                  on-string!
                  on-array-start!
                  on-array-end!
                  on-object-start!
                  on-object-end!
                  on-comma!
                  on-colon!
                  need-more?)
        value))))
