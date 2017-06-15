; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; OpenGL functions.
;;;; All functions in this package assume that proper OpenGL context was set up.
;;;;

(library (opengl lib)
  (export install-opengl-debug-callback!
          opengl-state-shaders
          application-opengl-state
          set-buffer-data
          allocate-buffers
          allocate-vertex-arrays)
  (import (chezscheme)
          (containers rvector)
          (opengl raw))

  ;; Error to raise in case of OpenGL errors.
  (define-condition-type &opengl-error &condition make-opengl-error opengl-error?
    (message message))

  ;; Parameter holding current opengl-debug-callback.
  (define debug-callback #f)
    
  ;; Installs/uninstalls OpenGL debugging callback using given procedure.
  ;; Procedure must accept following parameters:
  ;; source, type, id, severity, size, message and param
    (define (install-opengl-debug-callback! callback)
      (set! debug-callback (foreign-callable callback (int int unsigned-int int int string uptr) void))
      (lock-object debug-callback)
      (gl-debug-message-callback (foreign-callable-entry-point debug-callback) 0))
    (define (uninstall-opengl-debug-callback!)
      (gl-debug-message-callback 0 0)
      (unlock-object debug-callback)
      (set! debug-callback #f))

  ;; Retains currently active and defined buffers.
  (define-record-type opengl-state
    (nongenerative)
    (fields (immutable buffer-mapping)
            (immutable vertex-arrays)
            (immutable shaders)))

  ;; Allocates and initializes buffers-state structure.
  (define (allocate-opengl-state)
    (make-opengl-state (make-eqv-hashtable)
                        (make-eqv-hashtable)
                        (make-eqv-hashtable)))

  ;; Global parameter for managing buffers state.
  (define application-opengl-state
    (make-parameter
     (allocate-opengl-state)
     (lambda (st)
       (unless (opengl-state? st)
         (raise (make-opengl-error "Cannot assign non-opengl-state to application-opengl-state parameter.")))
       st)))

  ;; Marker for unbound buffer in buffers state.
  (define unbound-buffer-marker (gensym "unbound-buffer-marker"))

  ;; Allocates a number of some OpenGL entities via allocator and feeds them into consumer.
  ;; Allocator is a function of number X bytevector that allocates entities into given bytevector
  ;; Consumer is a function of one argument that gets the read id.
  (define (opengl-allocate number allocator consumer)
    (let* ([size (foreign-sizeof 'unsigned-int)]
           [storage (make-bytevector (* number size))]
           [result (make-vector number)])
      (allocator number storage)
      (do ([i 0 (+ 1 i)])
          ((>= i number) result)
        (let ([id (bytevector-u32-native-ref storage (* i size))])
          (consumer id)
          (vector-set! result i id)))))

  ;; Allocates buffers and returns a vector of allocated buffers.
  (define (allocate-buffers number)
    (opengl-allocate number
                     gl-gen-buffers
                     (lambda (id)
                       (hashtable-set! (opengl-state-buffer-mapping (application-opengl-state))
                                       id
                                       unbound-buffer-marker))))

  ;; Allocates vertex arrays and returns a vector of them.
  (define (allocate-vertex-arrays number)
    (opengl-allocate number
                     gl-gen-vertex-arrays
                     (lambda (id)
                       (hashtable-set! (opengl-state-vertex-arrays (application-opengl-state))
                                       id
                                       unbound-buffer-marker))))

  ;; Binds given buffer to a given target.
  (define (bind-buffer buffer-id buffer-type)
    (assert (buffer-exists? buffer-id))
    (gl-bind-buffer buffer-type buffer-id)
    (hashtable-set! (opengl-state-buffer-mapping (application-opengl-state))
                    buffer-id
                    buffer-type))

  ;; Binds given vertex array.
  (define (bind-vertex-array vertex-array-id)
    (assert (vertex-array-exists? vertex-array-id))
    (gl-bind-vertex-array vertex-array-id))

  ;; Checks if given buffer exists in the buffers-state
  (define (buffer-exists? buffer-id)
    (hashtable-contains? (opengl-state-buffer-mapping (application-opengl-state))
                         buffer-id))

  ;; Checks if given vertex array exists.
  (define (vertex-array-exists? vertex-array-id)
    (hashtable-contains? (opengl-state-vertex-arrays (application-opengl-state))
                         vertex-array-id))

  ;; Allocates array buffer of appropriate size, sets it's data and draw-type and returns id of the buffer.
  (define (set-buffer-data buffer-id buffer-type source-buffer draw-type)
    (bind-buffer buffer-id buffer-type)
    (gl-buffer-data buffer-type (bytevector-length source-buffer) source-buffer draw-type)
    buffer-id)

  ;; Unsets the array buffer data (effectively by assigning an empty array to that).
  (define (unset-buffer-data buffer-id buffer-type)
    (set-buffer-data buffer-id buffer-type #vu8() :gl-static-draw)))
