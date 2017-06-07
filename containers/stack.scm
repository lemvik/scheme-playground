; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; Scheme stack implementation.
;;;;

(library (containers stack)
  (export make-stack
          stack-push!
          stack-pop!
          stack-top
          stack-empty?
          stack-display)
  (import (rnrs)
          (portability base)
          (containers rvector))

  ;; Default capacity for the stack.
  (define default-stack-capacity 10)

  ;; Creates an empty stack.
  (define make-stack
    (case-lambda
      [() (make-rvector default-stack-capacity)]
      [rest (let ([s (make-stack)])
              (for-each (lambda (e) (stack-push! s e))
                        rest)
              s)]))

  ;; Pushed element onto the stack.
  (define (stack-push! stack element)
    (rvector-push! stack element))

  ;; Pops element off the stack.
  (define (stack-pop! stack)
    (rvector-pop! stack))

  ;; Returns reference to the top element of the stack.
  (define (stack-top stack)
    (rvector-top stack))

  ;; Returns true if given stack is empty.
  (define (stack-empty? stack)
    (rvector-empty? stack))

  ;; Returns the height of the stack.
  (define (stack-size stack)
    (rvector-length stack))

  ;; Displays the stack in a convinient manner.
  (define (stack-display stack)
    (let-values ([(out done) (open-string-output-port)])
      (format out "#s(~a)" (stack-size stack))
      (rvector-for-each stack (lambda (el)
                                (format out ":~a" el)))
      (format out ":@")
      (done))))
