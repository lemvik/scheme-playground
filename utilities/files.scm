; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; File-related utilities.
;;;;

(library (utilities files)
  (export directory-graph)
  (import (chezscheme)
          (utilities base))

  ;; Constructs a directory graph for given directory.
  (define (directory-graph path)
    (let ([roots (make-hashtable string-hash string=?)]
          [separator (make-string 1 (directory-separator))])
      (define (full-path p f)
        (string-append p separator f))
      (define (traverse p)
        (when (file-directory? p)
          (let ([childs (map (curry full-path p) (directory-list p))])
            (for-each (lambda (c)
                        (hashtable-update! roots p (lambda (l)
                                                     (cons c l)) (list))
                        (traverse c))
                      childs))))
      (assert (file-directory? path))
      (traverse path)
      (let-values ([(keys values) (hashtable-entries roots)])
        (vector->list (vector-map (lambda (k v)
                                    (cons k (list v)))
                                  keys
                                  values))))))
