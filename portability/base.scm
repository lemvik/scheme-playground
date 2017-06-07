; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; A re-export of chezscheme's format library.
;;;;

(library (portability base)
  (export format
          gensym)
  (import (chezscheme)))
