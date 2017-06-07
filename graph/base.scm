; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; Simple (immutable) graphs library.
;;;;

(library (graph base)
  (export make-directed-graph
          directed-graph?
          directed-graph->list
          depth-first-traversal
          reverse-directed-graph
          topological-sort)
  (import (rnrs)
          (utilities base))

  ;; Record containing graph structure.
  (define-record-type (directed-graph allocate-directed-graph directed-graph?)
    (nongenerative)
    (fields (immutable adjacency-list))) ; The list stored is actually a hashtable

  ;; Condition to raise if there is a graph-related error.
  (define-condition-type graph-error &condition make-graph-error graph-error?
    (offending-vertex vertex) ; The vertex that caused the error, if any
    (message message))        ; The description of the error.

  ;; Creates a graph from given list. List is assumed to be of form (adjacency list)
  ;; ((vertex-a (vertex-b vertex-c)) ...)
  ;; The representation of constructed graph is opaque.
  ;; NOTE: this function assumes equal? comparison and equal-hash hashing.
  (define (make-directed-graph lst)
    (allocate-directed-graph (normalize-directed-adjacency-list lst)))

  ;; Converts directed graph into a list 
  (define (directed-graph->list graph)
    (let ([adj-list (directed-graph-adjacency-list graph)]
          [result (list)])
      (let-values ([(verts neighs) (hashtable-entries adj-list)])
        (vector-for-each (lambda (k v)
                           (set! result (cons (list k v) result)))
                         verts
                         neighs)
        result)))

  ;; Reverses the edges in a directed graph
  (define (reverse-directed-graph graph)
    (let ([new-adj-list (make-hashtable equal-hash equal?)]
          [cur-adj-list (directed-graph-adjacency-list graph)])
      (vector-for-each
       (lambda (vert)
         (unless (hashtable-contains? new-adj-list vert)
           (hashtable-set! new-adj-list vert (list)))
         (for-each
          (lambda (neigh)
            (hashtable-update! new-adj-list
                               neigh
                               (lambda (lst)
                                 (cons vert lst))
                               (list)))
          (directed-neighbours graph vert)))
       (hashtable-keys cur-adj-list))
      (allocate-directed-graph new-adj-list)))

  ;; Normalizes input adjacency list
  ;; Constructs new list where each vertex has a adjacency list (as input list does't necessarily do)
  (define (normalize-directed-adjacency-list lst)
    (let ([nodes (make-hashtable equal-hash equal?)])
      (for-each (lambda (x)
                  (let ([vertex (car x)]
                        [neighbours (cadr x)])
                    (assert (list? neighbours))
                    (hashtable-set! nodes vertex neighbours)
                    (for-each (lambda (n)
                                (unless (hashtable-contains? nodes n)
                                  (hashtable-set! nodes n (list))))
                              neighbours)))
                lst)
      nodes))

  ;; Returns neighbours of the vertex in a directed graph.
  (define (directed-neighbours graph vert)
    (let ([adj-list (directed-graph-adjacency-list graph)])
      (hashtable-require adj-list vert (make-graph-error vert "Missing vertex from graph definition"))))

  ;; Traverses the graph depth-first invoking:
  ;; 1. pre-vertex - before the vertex or any of its neighbours is visited
  ;; 2. on-vertex - to process the vertex itself after it's neighbours were visited.
  ;; Since we cannot assume the input is a DAG, we keep a history of visited nodes.
  (define (depth-first-traversal graph pre-vertex post-vertex)
    (let* ([visited-nodes (make-hashtable equal-hash equal?)]
           [adj-list (directed-graph-adjacency-list graph)]
           [vertices (hashtable-keys adj-list)])
      (define (visit vert)
        (unless (hashtable-contains? visited-nodes vert)
          (pre-vertex vert)
          (hashtable-set! visited-nodes vert #t)
          (for-each visit (directed-neighbours graph vert))
          (post-vertex vert)))
      (vector-for-each visit vertices)))

  ;; Topologically sort a given graph.
  (define (topological-sort graph)
    (let ([being-traversed-marks (make-hashtable equal-hash equal?)]
          [result (list)])
      (define (pre-visit! vert)
        (when (hashtable-contains? being-traversed-marks vert)
          (raise (make-graph-error vert "Not a directed-acyclic-graph, topological sort is impossible.")))
        (hashtable-set! being-traversed-marks vert #t))
      (define (post-visit! vert)
        (set! result (cons vert result)))
      (depth-first-traversal graph pre-visit! post-visit!)
      result)))
