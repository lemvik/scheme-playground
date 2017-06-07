; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; Json tokenizer library.
;;;;

(library (json tokenizer)
  (export tokenize)
  (import (rnrs))

  ;; Condition to signal if something goes wrong during tokenization
  (define-condition-type &tokenizer-error &condition make-tokenizer-error tokenizer-error?
    (invalid-token tokenizer-error-token))

  ;; Reads a "-delimited literal from given source.
  (define (read-json-string source)
    (get-char source) ; Drop the opening "
    (let-values ([(out done) (open-string-output-port)])
      (let loop ([escaped #f])
        (let ([char (get-char source)])
          (cond [(and (char=? char #\") (not escaped)) (done)]
                [(and (char=? char #\\) (not escaped)) (put-char out char) (loop #t)]
                [else (put-char out char) (loop #f)])))))

  ;; Consumes enough chars from source to read the literal. If some of consumed
  ;; characters do not match literal's - raises tokenizer-error.
  (define (read-literal literal source)
    (string-for-each
     (lambda (c)
       (let ([ch (get-char source)])
         (when (not (char=? c ch))
           (raise (make-tokenizer-error ch)))))
     literal))

  ;; Reads a number from given source
  (define (read-number source)
    (let-values ([(out done) (open-string-output-port)])
      (put-char out (get-char source)) ; This character is already validated by calling code.
      (let loop ([has-dot #f])
        (let ([char (peek-char source)])
          (cond [(eof-object? char) (string->number (done) 10)]
                [(char-numeric? char) (get-char source) (put-char out char) (loop has-dot)]
                [(and (char=? char #\.) (not has-dot)) (get-char source) (put-char out char) (loop #t)]
                [else (string->number (done) 10)])))))

  ;; Tokenizes given input source via given callbacks
  (define (tokenize source
                    on-null
                    on-number
                    on-bool
                    on-string
                    on-array-start
                    on-array-end
                    on-object-start
                    on-object-end
                    on-comma
                    on-colon
                    need-more?)
    (let loop ()
      (let ([char (peek-char source)])
        (cond [(not (need-more?)) #t]
              [(eof-object? char) #f]
              [(char-whitespace? char) (get-char source) (loop)]
              [(char? char)
               (case char
                 [(#\{) (on-object-start) (get-char source) (loop)]
                 [(#\}) (on-object-end) (get-char source) (loop)]
                 [(#\[) (on-array-start) (get-char source) (loop)]
                 [(#\]) (on-array-end) (get-char source) (loop)]
                 [(#\,) (on-comma) (get-char source) (loop)]
                 [(#\:) (on-colon) (get-char source) (loop)]
                 [(#\") (on-string (read-json-string source)) (loop)]
                 [(#\t) (read-literal "true" source) (on-bool #t) (loop)]
                 [(#\f) (read-literal "false" source) (on-bool #f) (loop)]
                 [(#\n) (read-literal "null" source) (on-null) (loop)]
                 [(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\-)
                  (on-number (read-number source)) (loop)]
                 [else (raise (make-tokenizer-error char))])])))))
