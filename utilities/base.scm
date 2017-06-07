; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; Various utilities
;;;;

(library (utilities base)
  (export string-join
          hashtable-require
          curry)
  (import (rnrs)
          (portability base))

  ;; Joins a string on given separator.
  (define (string-join sep . strings)
    (let-values ([(out done) (open-string-output-port)])
      (let ([the-sep ""])
        (for-each (lambda (s)
                    (put-string out the-sep)
                    (put-string out s)
                    (set! the-sep sep))
                  strings)
        (done))))

  ;; Curries a function to given args.
  (define-syntax curry
    (syntax-rules ()
      [(_ fn arg1 ...)
       (lambda rest
         (apply fn arg1 ... rest))]))

  ;; Require value to be present in the hashtable, if it's missing, raises the provided error.
  ;; Defined as macros to avoid instantiation of error if everything goes proper.
  (define-syntax hashtable-require 
    (syntax-rules ()
      ([_ h k err]
       (let* ([missing-token (gensym "missing-token")]
              [val (hashtable-ref h k missing-token)])
         (when (equal? val missing-token)
           (raise err))
         val)))))
