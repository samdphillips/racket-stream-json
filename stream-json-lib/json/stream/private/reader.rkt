#lang racket/base

(provide read-json-event)

(require racket/match
         racket/port
         syntax/srcloc
         (for-syntax racket/base)

         json/stream/private/stream-match
         json/stream/private/string-buf
         json/stream/private/types)

(module+ test
  (require rackunit
           racket/format
           racket/sequence))

(define-match-expander char-byte
  (lambda (stx)
    (syntax-case stx ()
      [(_ ch) (with-syntax ([c (char->integer (syntax->datum #'ch))])
                #'(== c))])))

(define ZERO    (char->integer #\0))
(define ONE     (char->integer #\1))
(define NINE    (char->integer #\9))

(define (onenine-byte? c)
  (and (integer? c) (>= c ONE) (<= c NINE)))

(define (digit-byte? c)
  (and (integer? c)
       (or (= ZERO c)
           (onenine-byte? c))))

(define (ws-byte? c)
  (or (= c #x20)
      (= c #x0a)
      (= c #x0d)
      (= c #x09)))

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

(define (byte->number c) (- c ZERO))

(define (read-json-number inp)
  (match (read-byte inp)
    [(char-byte #\0)     (read-zero inp #t)]
    [(char-byte #\-)     (read-negative-number inp)]
    [(? onenine-byte? c) (read-number inp #t (byte->number c))]))

(define (read-zero inp pos?)
  (match (peek-byte inp)
    [(or (char-byte #\.)
         (char-byte #\e)
         (char-byte #\E))
     (read-number inp pos? 0)]
    [_ 0]))

(define (read-negative-number inp)
  (match (peek-byte inp)
    [(char-byte #\0)
     (read-byte inp)
     (read-zero inp #f)]
    [(? onenine-byte?) (read-number inp #f 0)]))

(define (read-number inp pos? n)
  (define (peek1) (peek-byte inp))
  (define (read1) (read-byte inp))
  ; XXX: parameter to alter reading exactly
  (define (build-number num exp inexact-result?)
    (define exactness (if inexact-result? exact->inexact values))
    (define sign (if pos? 1 -1))
    (exactness (* sign num (expt 10 exp))))
  (define (read-integer n)
    (match (peek1)
      [(? digit-byte?)
       (read-integer (+ (* 10 n) (byte->number (read1))))]
      [(or (char-byte #\E) (char-byte #\e))
       (read1)
       (read-expt n 0)]
      [(char-byte #\.)
       (read1)
       (read-fraction n 0)]
      [_ (build-number n 0 #f)]))
  (define (read-fraction n p)
    (match (peek1)
      [(? digit-byte?)
       (read-fraction (+ (* 10 n) (byte->number (read1))) (sub1 p))]
      [(or (char-byte #\E) (char-byte #\e))
       (read1)
       (read-expt n p)]
      [_ (build-number n p #t)]))
  (define (read-expt n p)
    (define s
      (match (peek1)
        [(char-byte #\+) (read1)  1]
        [(char-byte #\-) (read1) -1]
        [_                        1]))
    (read-expt* n p s 0))
  (define (read-expt* n p s e)
    (match (peek1)
      [(? digit-byte?)
       (read-expt* n p s (+ (* 10 e) (byte->number (read1))))]
      [_ (build-number n (+ p (* s e)) #t)]))
  (read-integer n))

(module+ test
  (define-syntax -test-number
    (syntax-rules ()
      [(_ input expected)
       (test-equal? input (call-with-input-string input read-json-number) expected)]))

  (-test-number "0" 0)
  (-test-number "1" 1)
  (-test-number "42" 42)
  (-test-number "-1" -1)
  (-test-number "-42" -42)
  (-test-number "-0" 0)
  (-test-number "0.314159" 0.314159)
  (-test-number "-0.31" -0.31)
  (-test-number "3.14159" 3.14159)

  (-test-number "1.23456e4" 12345.6)
  (-test-number "1.23456e5" 123456.0)
  (-test-number "0.314159e1" 3.14159)
  (-test-number "1e5" 1e5)
  (-test-number "123e5" 123e5)

  ;; 3. is invalid
  ;; 3e is invalid
  ;; 0123 is invalid
  )

(define (port->source-location port)
  (and (port-counts-lines? port)
       (let-values ([(line col pos) (port-next-location port)])
         (vector (object-name port) line col pos 0))))

; XXX: use ws-byte?
(define (skip-whitespace inp)
  (define c (peek-char inp))
  (when (and (char? c) (char-whitespace? c))
    (read-char inp)
    (skip-whitespace inp)))

(define (read-literal bs inp)
  (define rs (read-bytes (bytes-length bs) inp))
  (unless (bytes=? bs rs)
    (error 'read-json-event "expected literal ~a got: ~a" bs rs)))

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
    ;; XXX: make codepoints constants (or match macro?)
    (match (peek-byte inp)
      [(? eof-object? v) v]

      [(char-byte #\n)
       (read-literal #"null" inp)
       (json-value (source-location) 'null)]

      [(char-byte #\t)
       (read-literal #"true" inp)
       (json-value (source-location) #t)]

      [(char-byte #\f)
       (read-literal #"false" inp)
       (json-value (source-location) #f)]

      [(char-byte #\{)
       (read-byte inp)
       (json-object-start (source-location))]

      [(char-byte #\})
       (read-byte inp)
       (json-object-end (source-location))]

      [(char-byte #\[)
       (read-byte inp)
       (json-array-start (source-location))]

      [(char-byte #\])
       (read-byte inp)
       (json-array-end (source-location))]

      [(char-byte #\:)
       (read-byte inp)
       (json-delimiter (source-location) #\:)]

      [(char-byte #\,)
       (read-byte inp)
       (json-delimiter (source-location) #\,)]

      [(== (char->integer #\"))
       (read-byte inp)
       (let ([s (read-json-string inp)])
         (json-value (source-location) s))]

      [(or (? digit-byte?) (char-byte #\-))
       (let ([n (read-json-number inp)])
         (json-value (source-location) n))])))

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

