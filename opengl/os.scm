; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; OpenGL loading stuff and library dependencies. Mostly OS-dependent stuff.
;;;;

(library (opengl os)
  (export locate-foreign-procedure)
  (import (chezscheme))

  (meta-cond
   [(member (machine-type) '(ta6nt))
    (define wgl-get-proc-address (begin
                                   (load-shared-object "opengl32.dll")
                                   (load-shared-object "glfw3")
                                   (foreign-procedure "wglGetProcAddress" (string) uptr)))
    ;; Locates external procedure from OpenGL. Requires opened OpenGL context.
    (define (locate-foreign-procedure str)
      (wgl-get-proc-address str))]))
