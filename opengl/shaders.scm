; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; OpenGL shaders-related stuff.
;;;;

(library (opengl shaders)
  (export create-shader
          use-shader)
  (import (chezscheme)
          (opengl raw)
          (opengl lib)
          (ffi base))

  ;; Creates shader consisting of vertex and fragment parts.
  (define (create-shader vertex-shader-source fragment-shader-source)
    (let ([v-shader-id (gl-create-shader :gl-vertex-shader)]
          [f-shader-id (gl-create-shader :gl-fragment-shader)]
          [program-id (gl-create-program)])
      (define (load-shader shader-id src)
        (with-pointer-to-string-pointer src
          (lambda (shdr-ptr)
            (gl-shader-source shader-id 1 shdr-ptr 0)
            (gl-compile-shader shader-id))))
      (load-shader v-shader-id vertex-shader-source)
      (load-shader f-shader-id fragment-shader-source)
      (gl-attach-shader program-id v-shader-id)
      (gl-attach-shader program-id f-shader-id)
      (gl-link-program program-id)
      (gl-delete-shader v-shader-id)
      (gl-delete-shader f-shader-id)
      (hashtable-set! (opengl-state-shaders (application-opengl-state))
                      program-id
                      #t)
      program-id))

  ;; Uses shader for consequent rendenring.
  (define (use-shader shader-id)
    (assert (hashtable-contains? (opengl-state-shaders (application-opengl-state)) shader-id))
    (gl-use-program shader-id)))
