#lang racket/base

(provide port->json-stream
         json-stream/well-formed
         make-json-stream-fold
         jsexpr-object-key-converter
         json-stream->jsexpr
         jsexpr->json-stream)

(require racket/generator
         racket/match
         json/stream/private/reader
         json/stream/private/types
         json/stream/private/stream-match)

(module+ test
  (require rackunit
           racket/sequence))

(define (port->json-stream inp #:well-formed? [wf? #f])
  (let ([maybe-check-well-formed (if wf? json-stream/well-formed values)])
    (maybe-check-well-formed
     (sequence->stream
      (in-port read-json-event inp)))))

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
(define (json-stream/well-formed jst #:multiple? [multiple? #f])
  ; XXX: use source location in reporting
  (define (wf-error state expected actual)
    (error 'json-stream/well-formed
           "~a incorrect state expected: ~a, got: ~a"
           state
           expected
           actual))

  (define (empty-error error-tag expects)
    (wf-error error-tag expects "end of stream"))

  (define (next check s0) (check s0))

  (define ((check-value k allow-empty? error-tag expects) s0)
    (match s0
      [(? stream-empty?)
       (cond
         [allow-empty? s0]
         [else
           (empty-error error-tag expects)])]
      [(stream* (? json-value? v) s1)
       (stream* v (next k s1))]
      [(stream* (? json-array-start? v) s1)
       (stream* v (next (check-array-start k) s1))]
      [(stream* (? json-object-start? v) s1)
       (stream* v (next (check-object-start k) s1))]
      [(stream* v _)
       (wf-error error-tag
                 (append expects
                         '(atom array-start object-start))
                 v)]))

  (define ((check-array-start k) s0)
    (match s0
      [(stream* (? json-array-end? v) s1)
       (stream* v (next k s1))]
      [_ (next (check-value (check-array-delim k)
                            #f
                            'array-start
                            '(atom
                              object-start
                              array-start
                              array-end))
               s0)]))

  (define ((check-array-delim k) s0)
    (match s0
      [(stream* (json-delimiter _ #\,) s1)
       (next (check-array-value k) s1)]
      [(stream* (? json-array-end? v) s1)
       (stream* v (next k s1))]
      [(stream* v _)
       (wf-error 'array-delim '(#\, array-end) v)]))

  (define ((check-array-value k) s0)
    (match s0
      [(stream* (? json-array-end? v) _)
       (wf-error 'array-value '(array-value) v)]
      [_ (next (check-value (check-array-delim k)
                            #f
                            'array-value
                            '(array-end))
               s0)]))

  (define ((check-object-start k) s0)
    (match s0
      [(stream* (? json-object-end? v) s1)
       (stream* v (next k s1))]
      [(stream* (json-value loc (? string? v)) s1)
       (stream* (json-member-start loc v)
                (next (check-object-kv-delim k) s1))]
      [(stream* v _) (wf-error 'object-start '(object-key object-end) v)]))

  (define ((check-object-key k) s0)
    (match s0
      [(stream* (json-value loc (? string? v)) s1)
       (stream* (json-member-start loc v)
                (next (check-object-kv-delim k) s1))]
      [(stream* v _) (wf-error 'object-key '(object-key) v)]))

  (define ((check-object-kv-delim k) s0)
    (match s0
      [(stream* (json-delimiter _ #\:) s1)
       (next (check-value (check-object-delim k)
                          #f
                          'object-value
                          '(atom object-start array-start))
             s1)]
      [(stream* v _) (wf-error 'object-kv-delim '(#\:) v)]))

  (define ((check-object-delim k) s0)
    (match s0
      [(stream* (and v (json-object-end loc)) s1)
       (stream* (json-member-end loc) v (next k s1))]
      [(stream* (json-delimiter loc #\,) s1)
       (stream* (json-member-end loc) (next (check-object-key k) s1))]
      [(stream* v _) (wf-error 'object-delim '(object-end #\,) v)]))

  (define topk
    (if multiple?
        (lambda (s)
          ((check-value topk #t 'value '(atom object-start array-start)) s))
        (lambda (s)
          (cond
            [(not (stream-empty? s))
             (wf-error 'end '(empty-stream) (stream-first s))]
            [else s]))))

  (next (check-value topk #t 'value '(atom object-start array-start)) jst))

(module+ test
  (test-case "json-stream/well-formed atoms - ok"
             (let ([s (list (json-value #f 1) (json-value #f 2))])
               (for ([a (in-list s)]
                     [b (in-stream (json-stream/well-formed s #:multiple? #t))])
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

  (test-case "json-stream/well-formed array [42] - ok"
             (let ([s (list (json-array-start #f)
                            (json-value #f 42)
                            (json-array-end #f))])
               (for ([a (in-list s)]
                     [b (in-stream (json-stream/well-formed s))])
                 (check-equal? a b))))

  (test-case "json-stream/well-formed array [42,3,4] - ok"
             (let* ([s (list (json-array-start #f)
                             (json-value #f 42)
                             (json-delimiter #f #\,)
                             (json-value #f 3)
                             (json-delimiter #f #\,)
                             (json-value #f 4)
                             (json-array-end #f))]
                    [expected-seq
                      (sequence-filter
                        (match-lambda
                          [(json-delimiter _ #\,) #f]
                          [_ #t])
                        (in-list s))])
               (for ([expected expected-seq]
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

  (test-case "json-stream/well-formed object multiple kv pairs - ok"
             (let ([in (list (json-object-start #f)
                             (json-value #f "a")
                             (json-delimiter #f #\:)
                             (json-value #f "b")
                             (json-delimiter #f #\,)
                             (json-value #f "c")
                             (json-delimiter #f #\:)
                             (json-value #f "d")
                             (json-delimiter #f #\,)
                             (json-value #f "e")
                             (json-delimiter #f #\:)
                             (json-value #f "f")
                             (json-object-end #f))]
                   [out (list (json-object-start #f)
                              (json-member-start #f "a")
                              (json-value #f "b")
                              (json-member-end #f)
                              (json-member-start #f "c")
                              (json-value #f "d")
                              (json-member-end #f)
                              (json-member-start #f "e")
                              (json-value #f "f")
                              (json-member-end #f)
                              (json-object-end #f))])
               (for ([b (in-list out)]
                     [a (in-stream (json-stream/well-formed in))])
                 (check-equal? a b))))

  ; XXX: malformed keys that are not strings

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
;; XXX: maybe should use a foldable type that could have overriden parts?
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
   '(#hasheq([a . 42])))

  (check-equal?
   (jsexpr-fold
    null (list (json-object-start #f)
               (json-member-start #f "a")
               (json-value #f 42)
               (json-member-end #f)
               (json-member-start #f "b")
               (json-value #f 43)
               (json-member-end #f)
               (json-object-end #f)))
   '(#hasheq([a . 42] [b . 43]))))

(define jsexpr-object-key-converter
  (make-parameter string->symbol))

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
                  [(k) ((jsexpr-object-key-converter) k)])
       (json-stream->jsexpr-hash (hash-set acc k v) s))]))

;; FIXME: this is useful as demonstration, but the above do the same in practice?
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
                 (check-true (stream-empty? s)))))

  (test-case "json-stream->jsexpr hash use key converter"
             (let ([e* (list (json-object-start #f)
                             (json-member-start #f "a")
                             (json-value #f 1)
                             (json-member-end #f)
                             (json-member-start #f "b")
                             (json-value #f 4)
                             (json-member-end #f)
                             (json-object-end #f))])
               (parameterize ([jsexpr-object-key-converter values])
                 (let-values ([(v s) (json-stream->jsexpr e*)])
                   (check-equal? v (hasheq "a" 1 "b" 4))
                   (check-true (stream-empty? s))))))



             )

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

