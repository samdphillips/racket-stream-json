#lang racket/base

(require racket/contract
         racket/generator
         racket/match
         racket/port
         syntax/srcloc

         (for-syntax racket/base
                     syntax/parse)

         json/stream/private/port-match
         json/stream/private/stream-match)




(module+ test
  (require rackunit
           racket/format
           racket/sequence))

(struct json-event (location) #:transparent)

(struct json-value json-event (v) #:transparent)
(struct json-delimiter json-event (tok) #:transparent)

(struct json-object-start json-event () #:transparent)
(struct json-object-end   json-event () #:transparent)

(struct json-member-start json-event (name) #:transparent)
(struct json-member-end   json-event () #:transparent)

(struct json-array-start json-event () #:transparent)
(struct json-array-end   json-event () #:transparent)

(define ((make-json-value-pred pred?) v)
  (and (json-value? v)
       (pred? (json-value-v v))))

(define json-value-string? (make-json-value-pred string?))
(define json-value-number? (make-json-value-pred number?))
(define json-value-boolean? (make-json-value-pred boolean?))

;; read-json-string
;; input-port -> string
;; XXX: add rest of escapes
;; XXX: json specific exn
(define (read-json-string inp)
  (call-with-output-string
   (lambda (outp)
     (let/ec done
       (define (read-piece)
         (match inp
           [(peek #\") (done)]
           [(peek "\\\"") (write-char #\" outp)]
           [(peek "\\n") (write-char #\newline outp)]
           [(peek "\\\\") (write-char #\\ outp)]
           [(peek #px"^[^\\\\\"]+" s) (write-bytes s outp)]
           [(app (lambda (inp) (peek-string 10 0 inp)) buf)
            (error 'read-json-string
                   "at ~a, got: ~s"
                   (port->source-location inp)
                   buf)])
         (read-piece))
       (read-piece)))))

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

;; read-json-event
;; input-port -> (U json-event eof-object)
(define (read-json-event inp)
  (let* ([start-loc (port->source-location inp)]
         [source-location
          (lambda ()
            (and (port-counts-lines? inp)
                 (build-source-location-vector
                  start-loc (port->source-location inp))))])
    (match inp
      [(app peek-char (? eof-object? v)) v]

      [(peek #px"^\\s+")
       (read-json-event inp)]

      [(peek #"null")
       (json-value (source-location) 'null)]

      [(peek #"true")
       (json-value (source-location) #t)]

      [(peek #"false")
       (json-value (source-location) #f)]

      [(peek #\{)
       (json-object-start (source-location))]

      [(peek #\})
       (json-object-end (source-location))]

      [(peek #\[)
       (json-array-start (source-location))]

      [(peek #\])
       (json-array-end (source-location))]

      [(peek #\:) (json-delimiter (source-location) #\:)]

      [(peek #\,) (json-delimiter (source-location) #\,)]

      [(peek #\")
       (let ([s (read-json-string inp)])
         (json-value (source-location) s))]

      ;; XXX: understand exponential notation
      [(peek #px"^-?\\d+(\\.\\d+)?" s)
       (json-value (source-location)
                   (string->number
                     (bytes->string/utf-8 s)))])))

(define (port->json-stream inp #:well-formed? [wf? #f])
  (let ([wf (if wf? json-stream/well-formed values)])
    (wf
     (sequence->stream
      (in-port read-json-event inp)))))

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

;; json-stream/well-formed
;; stream[json-event] -> stream[json-event]
;; checks a stream for well-formedness as it is streaming
;; raises an exception when not well-formed
#|
  XXX: perhaps generalize what happens with a non-well-formed stream
    stream[json-event]
    (some-error-state -> stream[json-event])
      -> stream[json-event]

  error handler returns a stream of events to replace/splice into the
  output stream
|#

(define (json-stream/well-formed jst)
  ; XXX: use source location in reporting
  (define (wf-error state expected actual)
    (error 'json-stream/well-formed
           "~a incorrect state expected: ~a, got: ~a"
           state
           expected
           actual))

  ;; XXX: error if hit end of stream
  (define (next check s0)
    (match s0
      [(? stream-empty?) s0]
      [(stream* v s1) (check v s1 s0)]))

  (define ((check-value k [error-tag 'value] [expects null]) v s1 s0)
    (match v
      [(? json-value?)
       (stream* v (next k s1))]
      [(? json-array-start?)
       (stream* v (next (check-array-start k) s1))]
      [(? json-object-start?)
       (stream* v (next (check-object-start k) s1))]
      [_
       (wf-error error-tag
                 (append expects
                         '(atom array-start object-start))
                 v)]))

  (define ((check-array-start k) v s1 s0)
    (match v
      [(? json-array-end?)
       (stream* v (next k s1))]
      [_ (next (check-value (check-array-delim k)
                            'array-start
                            '(array-end))
               s0)]))

  (define ((check-array-delim k) v s1 s0)
    (match v
      [(json-delimiter _ #\,)  (next (check-array-value k) s1)]
      [(? json-array-end?) (stream* v (next k s1))]
      [_ (wf-error '(#\, array-end) v)]))

  (define ((check-array-value k) v s1 s0)
    (match v
      [(? json-array-end?)
       (stream* v (next k s1))]
      [_ (next (check-value (check-array-delim k)
                            'array-value
                            '(array-end))
               s0)]))

  (define ((check-object-start k) v s1 s0)
    (match v
      [(? json-object-end? v)
       (stream* v (next k s1))]
      [(json-value loc (? string? v))
       (stream* (json-member-start loc v)
                (next (check-object-kv-delim k) s1))]
      [_ (wf-error 'object-start '(object-key object-end) v)]))

  (define ((check-object-key k) v s1 s0)
    (match v
      [(json-value _ (? string?))
       (stream* v (next (check-object-kv-delim k) s1))]
      [_ (wf-error 'object-key '(object-key) v)]))

  (define ((check-object-kv-delim k) v s1 s0)
    (match v
      [(json-delimiter _ #\:)
       (next (check-value (check-object-delim k)
                          'object-value)
             s1)]
      [_ (wf-error 'object-kv-delim '(#\:) v)]))

  (define ((check-object-delim k) v s1 s0)
    (match v
      [(json-object-end loc)
       (stream* (json-member-end loc) v (next k s1))]
      [(json-delimiter loc #\,)
       (stream* (json-member-end loc) (next (check-object-key k) s1))]
      [_ (wf-error 'object-delim '(object-end #\,) v)]))

  (next (letrec ([k (lambda (v s1 s0) ((check-value k) v s1 s0))])
          (check-value k))
        jst))


(module+ test
  (test-case "json-stream/well-formed atoms - ok"
             (let ([s (list (json-value #f 1) (json-value #f 2))])
               (for ([a (in-list s)]
                     [b (in-stream (json-stream/well-formed s))])
                 (check-equal? a b))))

  (test-case "json-stream/well-formed atoms - not empty stream"
             (let ([s (list (json-value #f 1) (json-value #f 2))])
               (check-false (stream-empty? (json-stream/well-formed s)))))

  (test-case "json-stream/well-formed atoms - no delimiters"
             (check-exn #px"incorrect state"
                        (lambda ()
                          (stream->list
                           (json-stream/well-formed
                            (list (json-delimiter #f #\,))))))
             (check-exn #px"incorrect state"
                        (lambda ()
                          (stream->list
                           (json-stream/well-formed
                            (list (json-delimiter #f #\:)))))))

  (test-case "json-stream/well-formed array - ok"
             (let ([s (list (json-array-start #f)
                            (json-value #f 42)
                            (json-array-end #f))])
               (for ([a (in-list s)]
                     [b (in-stream (json-stream/well-formed s))])
                 (check-equal? a b))))

  (test-case "json-stream/well-formed array - ok"
             (let ([s (list (json-array-start #f)
                            (json-value #f 42)
                            (json-delimiter #f #\,)
                            (json-value #f 3)
                            (json-delimiter #f #\,)
                            (json-value #f 4)
                            (json-delimiter #f #\,)
                            (json-array-end #f))])
               (for ([expected (sequence-filter (match-lambda
                                                  [(json-delimiter _ #\,) #f]
                                                  [_ #t])
                                                (in-list s))]
                     [actual (in-stream (json-stream/well-formed s))])
                 (check-equal? actual expected))))

  (test-case "json-stream/well-formed deeper array - ok"
             (let ([s (list (json-array-start #f)
                            (json-array-start #f)
                            (json-value #f 42)
                            (json-array-end #f)
                            (json-array-end #f))])
               (for ([a (in-list s)]
                     [b (in-stream (json-stream/well-formed s))])
                 (check-equal? a b))))

  (test-case "json-stream/well-formed object - ok"
             (let ([in (list (json-object-start #f)
                             (json-value #f "foo")
                             (json-delimiter #f #\:)
                             (json-value #f 42)
                             (json-object-end #f))]
                   [out (list (json-object-start #f)
                              (json-member-start #f "foo")
                              (json-value #f 42)
                              (json-member-end #f)
                              (json-object-end #f))])
               (for ([b (in-list out)]
                     [a (in-stream (json-stream/well-formed in))])
                 (check-equal? a b))))

  (test-case "json-stream/well-formed object - malformed kv sequence"
             (let ([s (list (json-object-start #f)
                            (json-value #f "foo")
                            (json-delimiter #f #\:)
                            (json-value #f 42)
                            (json-delimiter #f #\,)
                            (json-value #f "bad")
                            (json-object-end #f))])
               (check-exn #px"kv-delim"
                          (lambda ()
                            (stream->list (json-stream/well-formed s)))))))

;; XXX: flag to read a single item from the stream
(define (make-json-stream-fold
         #:on-value        on-value
         #:on-array-start  on-array-start
         #:on-array-end    on-array-end
         #:on-object-start on-object-start
         #:on-object-end   on-object-end
         #:on-member-start on-member-start
         #:on-member-end   on-member-end)
  (define (do-fold pseed seed s)
    (cond
      [(stream-empty? s) seed]
      [else
       (let ([v (stream-first s)]
             [s (stream-rest s)])
         (match v
           [(? json-value?)
            (do-fold pseed (on-value seed v) s)]
           [(? json-array-start?)
            (do-fold (cons seed pseed) (on-array-start seed v) s)]
           [(? json-array-end?)
            (do-fold (cdr pseed) (on-array-end (car pseed) seed v) s)]
           [(? json-object-start?)
            (do-fold (cons seed pseed) (on-object-start seed v) s)]
           [(? json-object-end?)
            (do-fold (cdr pseed) (on-object-end (car pseed) seed v) s)]
           [(json-member-start _ key)
            (do-fold (list* seed key pseed) (on-member-start seed v) s)]
           [(? json-member-end?)
            (match-let ([(list parent key pseed ...) pseed])
              (do-fold pseed (on-member-end parent key seed v) s))]))]))
  (lambda (seed s)
    (do-fold null seed s)))

(module+ test
  (check-equal?
   (jsexpr-fold null (list (json-value #f 42)))
   '(42))

  (check-equal?
   (jsexpr-fold
    null (list (json-value #f 42) (json-value #f 85)))
   '(85 42))

  (check-equal?
   (jsexpr-fold
    null (list (json-array-start #f)
               (json-value #f 42)
               (json-value #f 85)
               (json-array-end #f)))
   '((42 85)))

  (check-equal?
   (jsexpr-fold
    null (list (json-array-start #f)
               (json-array-end #f)))
   '(()))

  (check-equal?
   (jsexpr-fold
    null (list (json-object-start #f)
               (json-object-end #f)))
   '(#hasheq()))

  (check-equal?
   (jsexpr-fold
    null (list (json-object-start #f)
               (json-member-start #f "a")
               (json-value #f 42)
               (json-member-end #f)
               (json-object-end #f)))
   '(#hasheq([a . 42]))))

;; json-stream->jsexpr
;; stream[json-event] -> <jsexpr, stream[json-event]>
;; expects a well formed stream of events
(define (json-stream->jsexpr s)
  (match s
    [(stream* (json-value _ v) s) (values v s)]
    [(stream* (json-array-start _) s)
     (json-stream->jsexpr-list s)]
    [(stream* (json-object-start _) s)
     (json-stream->jsexpr-hash (hasheq) s)]))

;; json-stream->jsexpr-list
;; stream[json-event] -> <jslist, stream[json-event]>
(define (json-stream->jsexpr-list s)
  (match s
    [(stream* (json-array-end _) s) (values null s)]
    [_
     (let*-values ([(head s) (json-stream->jsexpr s)]
                   [(tail s) (json-stream->jsexpr-list s)])
       (values (cons head tail) s))]))

;; json-stream->jsexpr-hash
;; hash stream[json-event] -> <jshash, stream[json-event]>
(define (json-stream->jsexpr-hash acc s)
  (match s
    [(stream* (json-object-end _) s) (values acc s)]
    [(stream* (json-member-end _) s) (json-stream->jsexpr-hash acc s)]
    [(stream* (json-member-start _ k) s)
     (let-values ([(v s) (json-stream->jsexpr s)]
                  [(k) (string->symbol k)])
       (json-stream->jsexpr-hash (hash-set acc k v) s))]))

(define jsexpr-fold
  (make-json-stream-fold
   #:on-value
   (lambda (seed v)
     (cons (json-value-v v) seed))
   #:on-array-start
   (lambda (seed v) null)
   #:on-array-end
   (lambda (pseed seed v)
     (cons (reverse seed) pseed))
   #:on-object-start
   (lambda (seed v)
     (hasheq))
   #:on-object-end
   (lambda (pseed seed v)
     (cons seed pseed))
   #:on-member-start
   (lambda (seed v) '())
   #:on-member-end
   (lambda (pseed k seed v)
     (hash-set pseed (string->symbol k) (car seed)))))


(module+ test
  (test-case "json-stream->jsexpr one value"
             (let-values ([(v s) (json-stream->jsexpr
                                  (list (json-value #f 'null)))])
               (check-eq? v 'null)
               (check-true (stream-empty? s))))

  (test-case "json-stream->jsexpr two value"
             (let ([e* (list (json-value #f 'null) (json-value #f 42))])
               (let-values ([(v s) (json-stream->jsexpr e*)])
                 (check-eq? v 'null)
                 (check-false (stream-empty? s))
                 (check-equal? (stream-first s) (json-value #f 42)))))

  (test-case "json-stream->jsexpr array"
             (let ([e* (list (json-array-start #f)
                             (json-value #f 1)
                             (json-value #f 2)
                             (json-value #f 3)
                             (json-array-end #f))])
               (let-values ([(v s) (json-stream->jsexpr e*)])
                 (check-equal? v (list 1 2 3))
                 (check-true (stream-empty? s)))))

  (test-case "json-stream->jsexpr hash"
             (let ([e* (list (json-object-start #f)
                             (json-member-start #f "a")
                             (json-array-start #f)
                             (json-value #f 1)
                             (json-value #f 2)
                             (json-value #f 3)
                             (json-array-end #f)
                             (json-member-end #f)
                             (json-member-start #f "b")
                             (json-array-start #f)
                             (json-value #f 4)
                             (json-value #f 5)
                             (json-value #f 6)
                             (json-array-end #f)
                             (json-member-end #f)
                             (json-object-end #f))])
               (let-values ([(v s) (json-stream->jsexpr e*)])
                 (check-equal? v (hasheq 'a (list 1 2 3)
                                         'b (list 4 5 6)))
                 (check-true (stream-empty? s))))))

;; jsexpr->json-stream
;; jsexpr -> stream[json-event]
(define (jsexpr->json-stream e)
  (sequence->stream
   (in-generator (jsexpr-walk e yield))))

;; jsexpr-walk
;; jsexpr (json-event -> void) -> void
(define (jsexpr-walk e callback)
  (match e
    [(or (? number?)
         (? string?)
         (? boolean?))
     (callback (json-value #f e))]

    ['null
     (callback (json-value #f 'null))]

    [(hash-table (k* v*) ...)
     (callback (json-object-start #f))
     (for ([k (in-list k*)]
           [v (in-list v*)])
       (callback (json-member-start #f (symbol->string k)))
       (jsexpr-walk v callback)
       (callback (json-member-end #f)))
     (callback (json-object-end #f))]

    [(list e* ...)
     (callback (json-array-start #f))
     (for ([e (in-list e*)])
       (jsexpr-walk e callback))
     (callback (json-array-end #f))]))

(module+ test
  (let ([obj (hasheq 'a '(1 2 3 4 null) 'b "c")])
    (test-equal? "jsexpr->json-stream"
                 (let-values ([(val s) (json-stream->jsexpr (jsexpr->json-stream obj))])
                   val)
                 obj)))

(provide
 (contract-out
  (struct json-value
    [(location source-location?)
     (v (or/c number? string? boolean? 'null))])
  (struct json-delimiter
    [(location source-location?)
     (tok (or/c #\: #\,))])
  (struct json-object-start [(location source-location?)])
  (struct json-object-end   [(location source-location?)])
  (struct json-array-start  [(location source-location?)])
  (struct json-array-end    [(location source-location?)])
  (struct json-member-start
    [(location source-location?)
     (name string?)])
  (struct json-member-end
    [(location source-location?)])

  (json-value-string? (-> any/c boolean?))
  (json-value-number? (-> any/c boolean?))
  (json-value-boolean? (-> any/c boolean?))
  (json-event? (-> any/c boolean?))
  (json-event-location (-> json-event? source-location?))
  (read-json-event
   (-> input-port? (or/c eof-object? json-event?)))
  (port->json-stream
   (-> input-port? (stream/c json-event?)))
  (json-stream/well-formed
   (-> (stream/c json-event?) (stream/c json-event?)))

  (make-json-stream-fold
   (-> #:on-value        (-> any/c json-value? any)
       #:on-array-start  (-> any/c json-array-start? any)
       #:on-array-end    (-> any/c any/c json-array-end? any)
       #:on-object-start (-> any/c json-object-start? any)
       #:on-object-end   (-> any/c any/c json-object-end? any)
       #:on-member-start (-> any/c json-member-start? any)
       #:on-member-end   (-> any/c string? any/c json-member-end? any)
       (-> any/c (stream/c json-event?) any)))))
