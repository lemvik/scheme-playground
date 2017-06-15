; -*- geiser-scheme-implementation: chez -*-
#!chezscheme

;;;;
;;;; Simple application that reads json from given file and outputs _id from expected top-level array.
;;;;

(import (chezscheme)
        (opengl raw)
        (opengl glfw)
        (opengl primitives))

(load-shared-object "opengl32")
(load-shared-object "glfw3")

(define +triangle+ '( 0.0  0.5  0.0
                      0.5 -0.5  0.0
                     -0.5 -0.5  0.0))

(define +vertex-shader+ "#version 400\nin vec3 vp;\nvoid main() {\ngl_Position = vec4(vp, 1.0);\n}")
(define +fragment-shader+ "#version 400\nout vec4 frag_colour;\nvoid main() {\n  frag_colour = vec4(0.5, 0.8, 0.5, 1.0);\n}")

(with-glfw-window (640 480 "Test window")
                  (let ([shape (load-static-model +triangle+ +vertex-shader+ +fragment-shader+)])
                    (in-event-loop
                     (gl-clear 16384)
                     (shape))))
