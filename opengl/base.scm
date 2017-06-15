; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; An OpenGL bindings library.
;;;;

(library (opengl base)
  (export initialize-opengl
          with-opengl-window)
  (import (chezscheme))
