; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; Utils for JSON library.
;;;;

(library (containers rvector)
  (export make-rvector
          rvector?
          rvector-ref
          rvector-set!
          rvector-empty?
          rvector-length
          rvector-capacity
          rvector-ensure-capacity!
          rvector-push!
          rvector-top
          rvector-pop!
          rvector-for-each
          rvector-display
          rvector->list)
  (import (rnrs)
          (portability base))

  ;; Copies contents of the source vector delimited by source-start and source-end into
  ;; target vector from target-start. Raises a condition if such copy is impossible.
  ;; Returns the filled target vector.
  (define (vector->vector source target source-start source-end target-start)
    (assert (and (>= source-end source-start) (>= source-start 0) (>= (vector-length source) source-end)))
    (assert (and (>= target-start 0) (>= (vector-length target) (+ target-start (- source-end source-start)))))
    (do ([i source-start (+ i 1)]
         [j target-start (+ j 1)])
        ([> i source-end] target)
      (let ([value (vector-ref source i)])
        (vector-set! target i value))))

  ;; A resizable vector that works much like it's CL counterpart with fill-pointer
  (define-record-type (rvector allocate-rvector rvector?)
    (nongenerative)
    (fields (mutable fill-pointer)
            (mutable contents)))

  ;; Custom constructor for the rvector to hide internals.
  (define (make-rvector initial-capacity)
    (allocate-rvector -1 (make-vector initial-capacity)))

  ;; Returns a reference into a resizable vector.
  (define (rvector-ref rvec index)
    (assert (and (>= index 0) (<= index (rvector-fill-pointer rvec))))
    (vector-ref (rvector-contents rvec) index))

  ;; Sets a value in the resizable vector.
  (define (rvector-set! rvec index element)
    (assert (and (>= index 0) (<= index (rvector-fill-pointer rvec))))
    (vector-set! (rvector-contents rvec) index element))

  ;; Ensures that rvector has at least given size
  (define (rvector-ensure-capacity! rvec size)
    (unless (>= (vector-length (rvector-contents rvec)) size)
      (let ([new-contents (make-vector size)])
        (rvector-contents-set! rvec
                               (vector->vector (rvector-contents rvec)
                                               new-contents
                                               0
                                               (rvector-fill-pointer rvec)
                                               0)))))

  ;; Extends the resizable vector capacity with given factor
  (define rvector-extend!
    (case-lambda
      [(rvec) (rvector-extend! rvec 1.5)]
      [(rvec factor)
       (assert (> factor 1))
       (rvector-ensure-capacity! rvec (exact (ceiling (* (rvector-capacity rvec) factor))))]))

  (define (rvector-resize! rvec new-size)
    (assert (>= (rvector-capacity rvec) new-size))
    (rvector-fill-pointer-set! rvec new-size))

  ;; Returns current vector size of the resizable vector.
  (define (rvector-length rvec)
    (+ 1 (rvector-fill-pointer rvec)))

  ;; Returns the capacity of the resizable vector.
  (define (rvector-capacity rvec)
    (vector-length (rvector-contents rvec)))

  ;; Returns true if resizable vector has enough space without need for reallocation.
  (define (has-space? rvec)
    (> (rvector-capacity rvec) (rvector-length rvec)))

  ;; Resizable vector is considered empty if fill pointer points before the first element.
  (define (rvector-empty? rvec)
    (= (rvector-fill-pointer rvec) -1))

  ;; Pushes a new element onto the vector.
  (define (rvector-push! rvec element)
    (when (not (has-space? rvec))
      (rvector-extend! rvec))
    (let ([new-pointer (+ 1 (rvector-fill-pointer rvec))])
      (rvector-fill-pointer-set! rvec new-pointer)
      (rvector-set! rvec new-pointer element)))

  ;; Returns the top element for the resizable vector.
  (define (rvector-top rvec)
    (assert (not (rvector-empty? rvec)))
    (rvector-ref rvec (rvector-fill-pointer rvec)))

  ;; Pops a top element from the vector.
  (define (rvector-pop! rvec)
    (assert (not (rvector-empty? rvec)))
    (let ([element (rvector-top rvec)])
      (rvector-fill-pointer-set! rvec (- (rvector-fill-pointer rvec) 1))
      element))

  ;; Calls function f for each element of the vector.
  (define (rvector-for-each rvec f)
    (do [(len (rvector-length rvec))
         (i 0 (+ 1 i))]
        [(>= i len) #f]
      (let ([el (rvector-ref rvec i)])
        (f el))))

  ;; Displays rvector in a more appropriate manner.
  (define (rvector-display rvec)
    (let-values ([(out done) (open-string-output-port)])
      (format out "#r(~a:~a){" (rvector-length rvec) (rvector-capacity rvec))
      (let ([sep ""])
        (rvector-for-each rvec (lambda (el)
                                 (format out "~a~a" sep el)
                                 (set! sep ","))))
      (format out "}")
      (done)))

  ;; Converts given resizable vector into a list.
  ;; Second form applies mapping function to each element of the vector during conversion.
  (define rvector->list
    (case-lambda
      [(rvec)
       (rvector->list rvec (lambda (x) x))]
      [(rvec f)
       (let ([result (list)])
         (rvector-for-each rvec (lambda (e) (set! result (cons (f e) result))))
         (reverse result))])))
       
