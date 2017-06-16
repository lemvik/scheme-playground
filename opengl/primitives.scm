; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; OpenGL functions.
;;;; All functions in this package assume that proper OpenGL context was set up.
;;;;

(library (opengl primitives)
  (export load-static-polygon
          load-static-model
          create-polygon-renderer)
  (import (chezscheme)
          (opengl raw)
          (opengl lib)
          (opengl shaders))

  ;; Helper function to define conversion functions from Scheme to OpenGL.
  (define (convert-list points size setter)
    (let* ([len (length points)]
           [bvector (make-bytevector (* len size) 0)])
      (for-each (lambda (i p)
                  (setter bvector (* i size) p))
                (iota len)
                points)
      bvector))

  ;; Converts floating point list into a 'float vector that can be consumed by OpenGL.
  (define (list->opengl-float-vector points)
    (convert-list points
                  (foreign-sizeof 'float)
                  (lambda (v i p)
                    (bytevector-ieee-single-set! v i p 'little))))

  ;; Converts vector of intergers into an unsigned int vector to be consumed by OpenGL.
  (define (list->opengl-unsigned-int-vector points)
    (let ([size (foreign-sizeof 'unsigned-int)])
      (convert-list points
                    size
                    (lambda (v i p)
                      (bytevector-uint-set! v i p 'little size)))))

  ;; Loads static model with given vertices and vertex/fragment shaders and returns function
  ;; that can be invoked to render the model.
  (define (load-static-model vertices vertex-shader fragment-shader)
    (let ([program (create-shader vertex-shader fragment-shader)]
          [model-render (load-static-polygon (list->opengl-float-vector vertices))])
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
        (gl-draw-arrays :gl-triangles 0 6))))

  ;; Creates a renderer for a polygon using given vertices and indices.
  (define (create-polygon-renderer vertices indices)
    (let* ([buffers (allocate-buffers 2)]
           [vertex-buffer-id (vector-ref buffers 0)]
           [index-buffer-id (vector-ref buffers 1)]
           [array-id (vector-ref (allocate-vertex-arrays 1) 0)]
           [indices-count (vector-length indices)])
      (gl-bind-vertex-array array-id)
      (set-buffer-data vertex-buffer-id :gl-array-buffer (list->opengl-float-vector vertices) :gl-static-draw)
      (set-buffer-data index-buffer-id :gl-element-array-buffer (list->opengl-unsigned-int-vector indices) :gl-static-draw)
      (gl-vertex-attrib-pointer 0 3 :gl-float :gl-false 0 0)
      (gl-enable-vertex-attrib-array 0)
      (lambda ()
        (gl-bind-vertex-array array-id)
        (gl-draw-elements :gl-triangles indices-count :gl-unsigned-int 0)))))
