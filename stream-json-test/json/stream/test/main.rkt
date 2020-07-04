#lang racket/base

(require racket/file
         racket/format
         racket/runtime-path
         racket/stream
         racket/string
         rackunit

         json
         json/stream)

(provide test-file->list)

(define-runtime-path test-files-path "minefield-tests")

(define (json-filename? p)
  (string-suffix? (path->string p) ".json"))

(define (success-filename? p)
  (define-values (_a b _c) (split-path p))
  (string-prefix? (path->string b) "y_"))

(define (failure-filename? p)
  (define-values (_a b _c) (split-path p))
  (string-prefix? (path->string b) "n_"))

(define (test-files)
  (for/list ([fname (in-directory test-files-path)]
             #:when (json-filename? fname))
    fname))

(define (success-test-files)
  (for/list ([fname (in-list (test-files))]
             #:when (success-filename? fname))
    fname))

(define (make-success-tests parser-name parse-fn)
  (make-test-suite
    (~a parser-name " should successfully parse")
    (for/list ([test-file-path (in-list (success-test-files))])
      (define short-file-name
        (let-values ([(a b c) (split-path test-file-path)]) b))
      (test-suite
        (path->string short-file-name)
        (with-check-info (['file-contents (string-info (~.s (file->bytes test-file-path)))])
          (test-not-exn
            "parse file"
            (lambda ()
              (call-with-input-file test-file-path parse-fn))))))))

(define (failure-test-files)
  (for/list ([fname (in-list (test-files))]
             #:when (failure-filename? fname))
    fname))

(define (make-failure-tests parser-name parse-fn)
  (make-test-suite
    (~a parser-name " should not successfully parse")
    (for/list ([test-file-path (in-list (failure-test-files))])
      (define short-file-name
        (let-values ([(a b c) (split-path test-file-path)]) b))
      (test-suite
        (path->string short-file-name)
        (with-check-info (['file-contents (string-info (~.s (file->bytes test-file-path)))])
          (test-exn "parse file"
                    exn:fail?
                    (lambda ()
                      (call-with-input-file test-file-path parse-fn))))))))

(define (test-file->list fname [wf? #t])
  (define p (build-path test-files-path fname))
  (writeln (file->bytes p))
  (call-with-input-file p
    (lambda (inp)
      (stream->list
        (port->json-stream inp #:well-formed? wf?)))))

(define (test-file-json fname)
  (define p (build-path test-files-path fname))
  (writeln (file->bytes p))
  (call-with-input-file p read-json))

(module* main #f
  (require racket/match)

  (match (current-command-line-arguments)
    [(vector fname)          (test-file->list fname)]
    [(vector "--nowf" fname) (test-file->list fname #f)]
    [(vector "--wf" fname)   (test-file->list fname #t)]
    [(vector "json" fname)   (test-file-json fname)]))

(module* test #f
  (require racket/match
           rackunit/text-ui
           json)

  (define (read-all-json-stream inp)
    (define s
      (port->json-stream inp #:well-formed? #t))
    (define (read-all s)
      (unless (stream-empty? s)
        (read-all (stream-rest s))))
    (read-all s))

  (define-values (name parser)
    (match (current-command-line-arguments)
      [(vector "json") (values "json" read-json)]
      [(vector "stream-json-lib")
       (values "stream-json-lib" read-all-json-stream)]
      [(vector) (values "stream-json-lib" read-all-json-stream)]))

  (run-tests
    (test-suite "minefield tests"
                (make-success-tests name parser)
                (make-failure-tests name parser))))

