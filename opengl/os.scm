; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; OpenGL loading stuff and library dependencies. Mostly OS-dependent stuff.
;;;;

(library (opengl os)
  (export locate-foreign-procedure)
  (import (chezscheme))

  (define (locate-foreign-procedure str)
    (define wgl-get-proc-address (foreign-procedure "wglGetProcAddress" (string) uptr))
    (wgl-get-proc-address str)))
