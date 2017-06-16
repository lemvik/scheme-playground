; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; Functions for model definitions in OpenGL
;;;;

(library (opengl model)
  (export create-model 
          model-renderer)
  (import (chezscheme)
          (opengl primitives)
          (opengl lib)
          (opengl shaders))

  ;; OpenGL model - vertices and indices for geometry,
  ;; shader definitions that allows a model to be drawn.
  (define-record-type geometry-model
    (nongenerative)
    (fields (immutable vertices)
            (immutable indices)
            (immutable vertex-shader)
            (immutable fragment-shader)))

  ;; Creates a model from constituents.
  (define (create-model v i vs fs)
    (make-geometry-model v i vs fs))

  ;; Returns a procedure that can be used to draw a model on the screen.
  (define (model-renderer model)
    (let ([verts (geometry-model-vertices model)]
          [inds (geometry-model-indices model)]
          [v-shader (geometry-model-vertex-shader model)]
          [f-shader (geometry-model-fragment-shader model)])
      (let ([shape-renderer (create-polygon-renderer verts inds)]
            [shader (create-shader v-shader f-shader)])
        (lambda ()
          (use-shader shader)
          (shape-renderer))))))
