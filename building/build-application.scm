; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; Build process scripts. Chez-scheme specific.
;;;;

(library (build-application)
  (export build-application
          collect-required-libraries)
  (import (chezscheme)
          (utilities base)
          (containers rvector)
          (graph base)
          (json base)
          (json parser))

  ;; Condition to raise in case of build error.
  (define-condition-type &build-error &condition make-build-error build-error?
    (message message))

  ;; Build descriptor is a JSON file with following structure:
  ;; {
  ;;   "source-directories" : ["dirA", "dirB"],  - list of directories to search libraries in.
  ;;   "build-directory"    : "build",           - directory where compiled files will reside.
  ;;   "whole-program-opt"  : true,              - if true will try to perform whole program optimization.
  ;;   "entry-point"        : "entry.scm",       - the file with entry point of the application (without .scm)
  ;;   "output-file"        : "out-file",        - the name of the output file.
  ;;   "source-ext"         : ".scm"             - the expected extension of source files.
  ;; }

  ;; Builds an application using given application descriptor file name.
  (define (build-application descriptor-file-name)
    (let ([descriptor (json->scheme (parse-json (open-input-file descriptor-file-name)))])
      (let ([source-dir  (hashtable-require descriptor "source-directory"  (make-build-error "Missing source-directories parameter"))]
            [build-dir   (hashtable-require descriptor "build-directory"   (make-build-error "Missing build-directory parameter"))]
            [wpo?        (hashtable-ref     descriptor "whole-program-opt" #f)]
            [entry-point (hashtable-require descriptor "entry-point"       (make-build-error "Missing entry-point parameter"))]
            [out-file    (hashtable-require descriptor "output-file"       (make-build-error "Missing output-file parameter"))]
            [opt-level   (hashtable-ref     descriptor "opt-level"         2)]
            [source-ext  (hashtable-ref     descriptor "source-ext"        ".scm")])
        (ensure-directory build-dir)
        (parameterize ((library-directories (list (cons source-dir build-dir)))
                       (optimize-level opt-level)
                       (compile-imported-libraries #t)
                       (generate-wpo-files wpo?))
          (let* ([source-file (string-append source-dir "/" entry-point source-ext)]
                 [object-file (string-append build-dir "/" entry-point)]
                 ; I'd like to avoid compiling program here, but so far I couldn't find a way to
                 ; determine required libraries of program.
                 [required-libraries (collect-required-libraries (compile-program source-file object-file))])
            (for-each
             (lambda (lib)
               (let* ([library-name (apply (curry string-join "/") (map symbol->string lib))]
                      [library-source (string-append source-dir "/" library-name source-ext)]
                      [library-object (string-append build-dir "/" library-name ".so")]
                      [library-dir (path-parent library-object)])
                 (ensure-directory library-dir)
                 (compile-library library-source library-object)
                 (when wpo?
                   (let ([library-wpo (string-append build-dir "/" library-name ".wpo")])
                     (compile-whole-library library-wpo library-object)))))
             required-libraries)
            (compile-program source-file object-file) ; this recompilation is required because libraries that were compiled
                                                      ; are newer than the produced program and won't be loaded
                                                      ; due to "different compilation instance."
            (when wpo?
              (let ([wpo-file (string-append object-file ".wpo")]
                    [wpo-program (string-append object-file ".whole")])
                (compile-whole-program wpo-file wpo-file wpo-program))))))))

  ;; Returns a list of libraries that should be compiled for the program in order
  ;; they should be compiled
  (define (collect-required-libraries direct-requirements)
    (define system-libs-names (list 'rnrs 'scheme 'chezscheme))
    (define (system-lib? l)
      (exists (lambda (x) (equal? x (car l))) system-libs-names))

    (let ([lib-set (make-hashtable equal-hash equal?)]
          [requirements (list)])
      (define (add-library l)
        (unless (or (system-lib? l) (hashtable-contains? lib-set l))
          (hashtable-set! lib-set l (filter (lambda (l) (not (system-lib? l))) (library-requirements l)))
          (for-each add-library
                    (library-requirements l))))
      (for-each add-library direct-requirements)
      (let-values ([(keys vals) (hashtable-entries lib-set)])
        (vector-for-each (lambda (k v)
                           (set! requirements (cons (list k v) requirements)))
                         keys
                         vals))
      (topological-sort (reverse-directed-graph (make-directed-graph requirements)))))

  ;; Ensures that directory exists.
  (define (ensure-directory directory-name)
    ; TODO: something more clean/sane here.
    (unless (file-directory? directory-name)
      (mkdir directory-name))))
