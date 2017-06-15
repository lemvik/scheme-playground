; -*- geiser-scheme-implementation: chez -*-
#!chezscheme

;;;;
;;;; GLFW bindings for OpenGl initialization.
;;;;

(library (opengl glfw)
  (export with-glfw-window
          in-event-loop)
  (import (chezscheme)
          (ffi base)
          (opengl os)
          (opengl raw))

  ;; Condition type for GLFW errors.
  (define-condition-type &glfw-error &condition make-glfw-error glfw-error?
    (message message))

  ;; Holder for glfw-window reference (which is a pointer from Scheme's perspective).
  (define-record-type glfw-window
    (nongenerative)
    (fields (immutable window)))

  (define +glfw-context-version-major+ #x00022002)
  (define +glfw-context-version-minor+ #x00022003)

  ;; Parameter that holds the glfw window reference (unwrapped).
  (define current-glfw-window 
    (make-parameter #f
                    (lambda (ref)
                      (assert (or (not ref)
                                  (glfw-window? ref)))
                      (if (not ref)
                          ref
                          (glfw-window-window ref)))))

  (define glfw-init                 (lazy-foreign-procedure "glfwInit" () boolean))
  (define glfw-window-hint          (lazy-foreign-procedure "glfwWindowHint" (int int) void))
  (define glfw-create-window        (lazy-foreign-procedure "glfwCreateWindow" (int int string uptr uptr) uptr))
  (define glfw-destroy-window       (lazy-foreign-procedure "glfwDestroyWindow" (uptr) void))
  (define glfw-terminate            (lazy-foreign-procedure "glfwTerminate" () void))
  (define glfw-make-context-current (lazy-foreign-procedure "glfwMakeContextCurrent" (uptr) void))
  (define glfw-window-should-close? (lazy-foreign-procedure "glfwWindowShouldClose" (uptr) boolean))
  (define glfw-swap-buffers         (lazy-foreign-procedure "glfwSwapBuffers" (uptr) void))
  (define glfw-poll-events          (lazy-foreign-procedure "glfwPollEvents" () void))

  ;; Creates a GLFW window with given title and dimensions and runs given operations inside it
  ;; in a glfw-managed loop.
  (define-syntax with-glfw-window
    (syntax-rules ()
      ([_ (dimX dimY title) e1 ...]
       (dynamic-wind
         (lambda () #t)
         (lambda ()
           (unless (glfw-init)
             (raise (make-glfw-error "Failed to initialize glfw.")))
           (glfw-window-hint +glfw-context-version-major+ 4)
           (glfw-window-hint +glfw-context-version-minor+ 0)
           (let* ([window (glfw-create-window dimX dimY title 0 0)]
                  [window-ref (make-glfw-window window)])
             (with-exception-handler
                 (lambda (x)
                   (glfw-destroy-window window)
                   (raise x))
               (lambda ()
                 (glfw-make-context-current window)
                 (parameterize ([current-glfw-window window-ref])
                   e1 ...)))))
         glfw-terminate))))

  ;; Runs given forms in an event loop associated with current GLFW window.
  (define-syntax in-event-loop
    (syntax-rules ()
      ([_ body ...]
       (let ([window (current-glfw-window)])
         (unless window
           (raise (make-glfw-error "No current window to operate in")))
         (do ()
             ((glfw-window-should-close? window) #t)
           body
           ...
           (glfw-swap-buffers window)
           (glfw-poll-events)))))))
