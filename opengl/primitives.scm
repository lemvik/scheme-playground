; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; OpenGL functions.
;;;; All functions in this package assume that proper OpenGL context was set up.
;;;;

(library (opengl primitives)
  (export load-static-polygon
          load-static-model)
  (import (chezscheme)
          (opengl raw)
          (opengl lib)
          (opengl shaders))

  ;; Converts floating point list into a 'float vector that can be consumed by OpenGL.
  (define (vector->opengl-float-vector points)
    (let* ([len (length points)]
           [size (foreign-sizeof 'float)]
           [bvector (make-bytevector (* len size) 0)])
      (for-each (lambda (i p)
                  (bytevector-ieee-single-set! bvector (* i size) p 'little))
                (iota len)
                points)
      bvector))

  ;; Loads static model with given vertices and vertex/fragment shaders and returns function
  ;; that can be invoked to render the model.
  (define (load-static-model vertices vertex-shader fragment-shader)
    (let ([program (create-shader vertex-shader fragment-shader)]
          [model-render (load-static-polygon (vector->opengl-float-vector vertices))])
      (lambda ()
        (use-shader program)
        (model-render))))

  ;; Loads polygon into OpenGL context and returns procedure that can be used to draw it.
  (define (load-static-polygon polygon-data)
    (let ([buffer-id (vector-ref (allocate-buffers 1) 0)]
          [array-id (vector-ref (allocate-vertex-arrays 1) 0)])
      (gl-bind-vertex-array array-id)
      (set-buffer-data buffer-id :gl-array-buffer polygon-data :gl-static-draw)
      (gl-vertex-attrib-pointer 0 3 :gl-float :gl-false 0 0)
      (gl-enable-vertex-attrib-array 0)
      (lambda ()
        (gl-bind-vertex-array array-id)
        (gl-draw-arrays :gl-triangles 0 3)))))
