; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; Some FFI helper functions library.
;;;;

(library (ffi base)
  (export with-indirection
          with-string-pointer
          with-pointer-to-string-pointer)
  (import (chezscheme))

  ;; Calls function f with a pointer to given pointer. Useful to create pointers to pointers.
  (define (with-indirection ptr f)
    (let ([ptr->ptr #f])
      (dynamic-wind
        (lambda ()
          (set! ptr->ptr (foreign-alloc (foreign-sizeof 'uptr)))
          (foreign-set! 'uptr ptr->ptr 0 ptr))
        (lambda ()
          (f ptr->ptr))
        (lambda ()
          (when ptr->ptr
            (foreign-free ptr->ptr)
            (set! ptr->ptr #f))))))

  ;; Calls f with pointer to a temporarily allocated storage with copy of given string.
  ;; This is mostly used to pass pointers manually bypassing usual Chez conversion - for example,
  ;; when we need to pass pointer to pointer to string.
  ;; Quite inefficient function.
  (define (with-string-pointer str f)
    (let* ([utf8 (string->utf8 str)]
           [len (bytevector-length utf8)]
           [pointer #f])
      (dynamic-wind
        (lambda ()
          (set! pointer (foreign-alloc (+ 1 len)))
          (do ([i 0 (+ i 1)])
              ((>= i len) (begin
                            (foreign-set! 'unsigned-8 pointer i 0)
                            pointer))
            (foreign-set! 'unsigned-8 pointer i (bytevector-u8-ref utf8 i))))
        (lambda ()
          (f pointer))
        (lambda ()
          (when pointer
            (foreign-free pointer)
            (set! pointer #f))))))

  ;; Invokes procedure f with double pointer to a given string literal. Performs various copies so
  ;; is pretty inefficient.
  (define (with-pointer-to-string-pointer str f)
    (with-string-pointer str
      (lambda (p)
        (with-indirection p f)))))
