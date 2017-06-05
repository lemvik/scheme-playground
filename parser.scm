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
    (actual parse-error-actual))

  ;; Enumeration describing all possible parse states.
  (define-enumeration parse-state
    (state-value state-comma state-colon state-key)
    make-parse-state)

  ;; Parses JSON coming out of some source
  (define (parse-json in)
    (let ([value #f]
          [being-built (make-stack)]
          [state (make-stack (parse-state state-value))]
          [keys (make-stack)])
      (let-syntax ([expecting (syntax-rules ()
                                [(_ (t1 ...) e1 ...)
                                 (let ([actual (stack-top state)])
                                   (if (exists (lambda (s) (equal? s actual)) (list (parse-state t1) ...))
                                       (begin e1 ...)
                                       (raise (make-parse-error (list (parse-state t1) ...) actual))))]
                                [(_ t e1 ...)
                                 (let ([actual (stack-top state)])
                                   (if (equal? (parse-state t) actual)
                                       (begin e1 ...)
                                       (raise (make-parse-error (parse-state t) actual))))])])
        (define (state-pop!)
          (stack-pop! state))
        (define (state-push! st)
          (stack-push! state st))
        (define (current-state)
          (stack-top state))
        (define (being-built-ref)
          (stack-top being-built))
        (define (building-array?)
          (and (not (stack-empty? being-built))
               (json-value-array? (stack-top being-built))))
        (define (building-object?)
          (and (not (stack-empty? being-built))
               (json-value-object? (stack-top being-built))))
        (define (value-attached!)
          (when (or (building-array?) (building-object?))
            (state-push! (parse-state state-comma))))
        (define (attach! v)
          (cond [(not value)
                 (set! value v)]
                [(building-array?)
                 (json-array-push! (being-built-ref) v)]
                [(building-object?)
                 (json-object-set! (being-built-ref) (stack-pop! keys) v)]
                [else (assert #f)]))
        (define (on-null)
          (expecting state-value (attach! (make-json-null)))
          (state-pop!)
          (value-attached!))
        (define (on-number n)
          (expecting state-value (attach! (make-json-number n)))
          (state-pop!)
          (value-attached!))
        (define (on-boolean b)
          (expecting state-value (attach! (make-json-bool b)))
          (state-pop!)
          (value-attached!))
        (define (on-string str)
          (let ([expected (stack-top state)])
            (state-pop!)
            (cond [(equal? expected (parse-state state-key))
                   (stack-push! keys str)
                   (state-push! (parse-state state-colon))]
                  [(equal? expected (parse-state state-value))
                   (attach! (make-json-string str))
                   (value-attached!)]
                  [else (assert #f)])))
        (define (on-array-start)
          (expecting state-value
            (let ([arr (make-json-array)])
              (attach! arr)
              (state-push! (parse-state state-value))
              (stack-push! being-built arr))))
        (define (on-array-end)
          (expecting (state-value state-comma)
                     (state-pop!)
                     (state-pop!)
                     (stack-pop! being-built)
                     (value-attached!)))
        (define (on-object-start)
          (expecting state-value
            (let ([obj (make-json-object)])
              (attach! obj)
              (state-push! (parse-state state-key))
              (stack-push! being-built obj))))
        (define (on-object-end)
          (expecting (state-key state-comma)
                     (state-pop!)
                     (state-pop!)
                     (stack-pop! being-built)
                     (value-attached!)))
        (define (on-comma)
          (expecting state-comma
                     (state-pop!)
                     (cond [(building-array?) (state-push! (parse-state state-value))]
                           [(building-object?) (state-push! (parse-state state-key))]
                           [else (assert #f)])))
        (define (on-colon)
          (expecting state-colon
                     (assert (building-object?))
                     (state-pop!)
                     (state-push! (parse-state state-value))))
        (define (need-more?)
          (not (stack-empty? state)))
        (tokenize in
                  on-null
                  on-number
                  on-boolean
                  on-string
                  on-array-start
                  on-array-end
                  on-object-start
                  on-object-end
                  on-comma
                  on-colon
                  need-more?)
        value))))
