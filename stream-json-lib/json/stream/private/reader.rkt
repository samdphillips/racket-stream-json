#lang racket/base

(provide read-json-event)

(require racket/match
         racket/port
         syntax/srcloc

         json/stream/private/stream-match
         json/stream/private/string-buf
         json/stream/private/types)

(module+ test
  (require rackunit
           racket/format
           racket/sequence))

;; read-json-string
;; input-port -> string
;; XXX: add rest of escapes
;; XXX: json specific exn
(define (read-json-string inp)
  (define buf (make-string-buf))
  (define (decode-escape)
    (match (read-char inp)
      [#\\ (string-buf-write-char! buf #\\)]
      [#\n (string-buf-write-char! buf #\newline)]
      [#\" (string-buf-write-char! buf #\")]
      [_   (error 'read-json-string "unknown escape")])
    (read-piece))
  (define (decode-utf8 c)
    ; XXX: parameter for permissive decoding
    (decode-utf8-loop 0 1 (make-bytes 6 c) (bytes-open-converter "UTF-8" "UTF-8")))
  (define (decode-utf8-loop start end buf conv)
    (define-values (wamt ramt state) (bytes-convert conv buf start end buf 0 6))
    (match state
      ['complete
       (string-buf-write-char! buf (bytes-utf-8-ref buf 0))
       (read-piece)]
      ['error
       ; XXX:
       (error 'read-json-string "utf8 decode error")]
      ['aborts
       (define c (read-byte inp))
       (cond
         [(eof-object? c) (error 'read-json-string "eof in string")]
         [else
           (bytes-set! buf end c)
           (decode-utf8-loop (+ start ramt) (add1 end) buf conv)])]))
  (define (seven-bit? c) (< c 128))
  (define (read-piece)
    (match (read-byte inp)
      [(? eof-object?)          (error 'read-json-string "eof in string")]
      [(== (char->integer #\")) (string-buf->string buf)]
      [(== (char->integer #\\)) (decode-escape)]
      [(? seven-bit? c)         (string-buf-write-byte! buf c) (read-piece)]
      [c                        (decode-utf8 c)]))

    (read-piece))

(module+ test
  (define-syntax -test-string
    (syntax-rules ()
      [(_ input expected)
       (check-equal? (call-with-input-string input read-json-string) expected)]))

  (-test-string (string #\\ #\" #\") (string #\"))
  (-test-string (string #\") "")
  (-test-string (string #\\ #\\ #\") (string #\\))
  (-test-string (~a "hello " (string #\\ #\") "world" (string #\\ #\" #\"))
                (~a "hello " (string #\") "world" (string #\"))))

(define (port->source-location port)
  (and (port-counts-lines? port)
       (let-values ([(line col pos) (port-next-location port)])
         (vector (object-name port) line col pos 0))))

(define (skip-whitespace inp)
  (define c (peek-char inp))
  (when (and (char? c) (char-whitespace? c))
    (read-char inp)
    (skip-whitespace inp)))

(define (read-literal bs inp)
  (define rs (read-bytes (bytes-length bs) inp))
  (unless (bytes=? bs rs)
    (error 'read-json-event "expected literal ~a got: ~a" bs rs)))

(define numeric-char-byte?
  (let ([ZERO (char->integer #\0)]
        [NINE (char->integer #\9)])
    (lambda (c)
      (and (>= c ZERO) (<= c NINE)))))

;; read-json-event
;; input-port -> (U json-event eof-object)
(define (read-json-event inp)
  (skip-whitespace inp)
  (let* ([start-loc (port->source-location inp)]
         [source-location
          (lambda ()
            (and (port-counts-lines? inp)
                 (build-source-location-vector
                  start-loc (port->source-location inp))))])
    ;; XXX: switch to peek-byte
    ;; XXX: make codepoints constants (or match macro?)
    (match (read-byte inp)
      [(? eof-object? v) v]

      [(== (char->integer #\n))
       (read-literal #"ull" inp)
       (json-value (source-location) 'null)]

      [(== (char->integer #\t))
       (read-literal #"rue" inp)
       (json-value (source-location) #t)]

      [(== (char->integer #\f))
       (read-literal #"alse" inp)
       (json-value (source-location) #f)]

      [(== (char->integer #\{))
       (json-object-start (source-location))]

      [(== (char->integer #\}))
       (json-object-end (source-location))]

      [(== (char->integer #\[))
       (json-array-start (source-location))]

      [(== (char->integer #\]))
       (json-array-end (source-location))]

      [(== (char->integer #\:))
       (json-delimiter (source-location) #\:)]

      [(== (char->integer #\,))
       (json-delimiter (source-location) #\,)]

      [(== (char->integer #\"))
       (let ([s (read-json-string inp)])
         (json-value (source-location) s))]

      [(? numeric-char-byte?  n)
       (json-value (source-location)
                   (- n (char->integer #\0)))]

      ;; XXX: actually re-implement this ...
      ;; XXX: understand exponential notation
      #;
      [(peek #px"^-?\\d+(\\.\\d+)?" s)
       (json-value (source-location)
                   (string->number
                     (bytes->string/utf-8 s)))])))

(module+ test
  (define-syntax -test-read
    (syntax-rules ()
      [(_ input token)
        (test-case (~a "read " 'token " token")
          (check-equal?
            (call-with-input-string input read-json-event)
            token))]
      [(_ input token0 token ...)
        (test-case (~a "read " 'token0 " token starting sequence")
        (check-equal?
          (call-with-input-string input
            (lambda (inp)
              (sequence->list (in-port read-json-event inp))))
          (list token0 token ...)))]))

  (-test-read " {" (json-object-start #f))
  (-test-read " }" (json-object-end #f))
  (-test-read " [" (json-array-start #f))
  (-test-read " ]" (json-array-end #f))
  (-test-read " null" (json-value #f 'null))
  (-test-read " :" (json-delimiter #f #\:))
  (-test-read " ," (json-delimiter #f #\,))
  (-test-read " true" (json-value #f #t))
  (-test-read " false" (json-value #f #f))

  (-test-read "42" (json-value #f 42))
  (-test-read "-3" (json-value #f -3))
  (-test-read "3.14159" (json-value #f 3.14159))

  (-test-read " { } " (json-object-start #f) (json-object-end #f))
  (-test-read "{ \"a\": \"b\", \"c\": 42 }"
              (json-object-start #f)
              (json-value #f "a") (json-delimiter #f #\:)
              (json-value #f "b") (json-delimiter #f #\,)
              (json-value #f "c") (json-delimiter #f #\:)
              (json-value #f 42)
              (json-object-end #f)))

