; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; Raw OpenGL bindings.
;;;;

(library (opengl raw)
  (export gl-attach-shader
          gl-bind-buffer
          gl-bind-vertex-array
          gl-buffer-data
          gl-clear
          gl-compile-shader
          gl-create-program
          gl-create-shader
          gl-debug-message-callback
          gl-delete-shader
          gl-draw-arrays
          gl-enable
          gl-enable-vertex-attrib-array
          gl-gen-buffers
          gl-gen-vertex-arrays
          gl-get-error
          gl-link-program
          gl-shader-source
          gl-use-program
          gl-vertex-attrib-pointer
          :gl-false
          :gl-true
          :gl-triangles
          :gl-float

          :gl-debug-output

          :gl-array-buffer
          :gl-static-draw

          :gl-fragment-shader
          :gl-vertex-shader)
  (import (chezscheme)
          (ffi base)
          (opengl os))

  (define :gl-false           #x0)
  (define :gl-true            #x1)
  (define :gl-triangles       #x0004)
  (define :gl-float           #x1406)

  (define :gl-debug-output    #x92e0)

  (define :gl-array-buffer    #x8892)
  (define :gl-static-draw     #x88e4)

  (define :gl-fragment-shader #x8b30)
  (define :gl-vertex-shader   #x8b31)

  (define gl-attach-shader              (lazy-foreign-procedure (locate-foreign-procedure "glAttachShader") (int int) void))
  (define gl-bind-buffer                (lazy-foreign-procedure (locate-foreign-procedure "glBindBuffer") (int unsigned-int) void))
  (define gl-bind-vertex-array          (lazy-foreign-procedure (locate-foreign-procedure "glBindVertexArray") (int) void))
  (define gl-buffer-data                (lazy-foreign-procedure (locate-foreign-procedure "glBufferData") (int unsigned-int u8* int) void))
  (define gl-clear                      (lazy-foreign-procedure "glClear" (unsigned-int) void))
  (define gl-compile-shader             (lazy-foreign-procedure (locate-foreign-procedure "glCompileShader") (int) void))
  (define gl-create-program             (lazy-foreign-procedure (locate-foreign-procedure "glCreateProgram") () int))
  (define gl-create-shader              (lazy-foreign-procedure (locate-foreign-procedure "glCreateShader") (int) int))
  (define gl-debug-message-callback     (lazy-foreign-procedure (locate-foreign-procedure "glDebugMessageCallback") (uptr uptr) void))
  (define gl-delete-shader              (lazy-foreign-procedure (locate-foreign-procedure "glDeleteShader") (int) void))
  (define gl-draw-arrays                (lazy-foreign-procedure "glDrawArrays" (unsigned-int int size_t) void))
  (define gl-enable                     (lazy-foreign-procedure "glEnable" (unsigned-int) void))
  (define gl-enable-vertex-attrib-array (lazy-foreign-procedure (locate-foreign-procedure "glEnableVertexAttribArray") (int) void))
  (define gl-gen-buffers                (lazy-foreign-procedure (locate-foreign-procedure "glGenBuffers") (int u8*) void))
  (define gl-gen-vertex-arrays          (lazy-foreign-procedure (locate-foreign-procedure "glGenVertexArrays") (int u8*) void))
  (define gl-get-error                  (lazy-foreign-procedure "glGetError" () unsigned-int))
  (define gl-link-program               (lazy-foreign-procedure (locate-foreign-procedure "glLinkProgram") (int) void))
  (define gl-shader-source              (lazy-foreign-procedure (locate-foreign-procedure "glShaderSource") (int int uptr uptr) int))
  (define gl-use-program                (lazy-foreign-procedure (locate-foreign-procedure "glUseProgram") (int) void))
  (define gl-vertex-attrib-pointer      (lazy-foreign-procedure (locate-foreign-procedure "glVertexAttribPointer") (unsigned-int int int int unsigned-int uptr) void)))
