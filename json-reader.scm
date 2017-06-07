; -*- geiser-scheme-implementation: chez -*-

;;;;
;;;; Simple application that reads json from given file and outputs _id from expected top-level array.
;;;;

(import (chezscheme)
        (json base)
        (json parser)
        (containers rvector))

(let* ([args (command-line-arguments)]
       [file (car args)]
       [json-v (json->scheme (parse-json (open-input-file file)))])
  (rvector-for-each json-v
                    (lambda (e)
                      (format #t "~&_id = ~a~%" (hashtable-ref e "_id" #f)))))
