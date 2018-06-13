#lang racket/base

(require racket/generator
         racket/match
         racket/port
         (rename-in racket/stream
                    [stream $tream]
                    [stream-cons $tream-cons])
         syntax/srcloc

         (for-syntax racket/base
                     syntax/parse))

(module+ test
  (require rackunit
           racket/format
           racket/sequence))

(struct js-event (location) #:transparent)

(struct js-value js-event (v) #:transparent)

(struct js-object-start js-event () #:transparent)
(struct js-object-end   js-event () #:transparent)
(struct js-object-key   js-event (name) #:transparent)

(struct js-array-start js-event () #:transparent)
(struct js-array-end   js-event () #:transparent)

(define ((make-js-value-pred pred?) v)
  (and (js-value? v)
       (pred? (js-value-v v))))

(define js-string? (make-js-value-pred string?))
(define js-number? (make-js-value-pred number?))

(define ($peek-char inp)
  (values (peek-char inp) inp))

(define (($peek-string count) inp)
  (values (peek-string count 0 inp) inp))

(define (($read-string count) inp)
  (read-string count inp))

(define (($regexp-try-match pat) inp)
  (values (regexp-try-match pat inp) #f))

(begin-for-syntax
  (define-syntax-class regexp
    #:description "regular expression"
    (pattern x #:when (regexp? (syntax-e #'x)))))

(define-match-expander peek
  (lambda (stx)
    (let ([$peek (lambda (pat peek read)
                   #`(app #,peek #,pat (app #,read _)))])
      (syntax-parse stx
        [(peek spat:string)
         #:with slen (string-length (syntax-e #'spat))
         ($peek #'spat #'($peek-string 'slen) #'($read-string 'slen))]

        [(peek cpat:char)
         ($peek #'cpat #'$peek-char #'read-char)]

        [(peek rxpat:regexp pat ...)
         ($peek #'(and (not #f)
                       (list-rest (app bytes->string/utf-8 pat) ... _))
                #'($regexp-try-match rxpat)
                #'values)]))))

;; read-js-string
;; input-port -> string
;; XXX: add rest of escapes
(define (read-js-string inp)
  (call-with-output-string
   (lambda (outp)
     (let/ec done
       (define (read-piece)
         (match inp
           [(peek #\") (done)]
           [(peek "\\\"") (write-char #\" outp)]
           [(peek "\\\\") (write-char #\\ outp)]
           [(peek #px"^[^\\\\\"]+" s) (write-string s outp)]
           [(app (lambda (inp) (peek-string 5 0 inp)) s)
            (error 'read-js-string "got: ~s" s)]
           )
         (read-piece))
       (read-piece)))))


(module+ test
  (check-equal?
   (call-with-input-string
    (string #\\ #\" #\")
    read-js-string)
   (string #\"))

  (check-equal?
   (call-with-input-string
    (string #\")
    read-js-string)
   "")

  (check-equal?
   (call-with-input-string
    (string #\\ #\\ #\")
    read-js-string)
   (string #\\))

  (check-equal?
   (call-with-input-string
    (string-append "hello "
                   (string #\\ #\")
                   "world"
                   (string #\\ #\" #\"))
    read-js-string)
   (string-append "hello " (string #\") "world" (string #\")))
  )

(define (port->source-location port)
  (and (port-count-lines-enabled)
       (let-values ([(line col pos) (port-next-location port)])
         (vector (object-name port) line col pos 0))))

;; read-js-event
;; input-port -> (U js-event eof-object)
;;  js-event includes delimiters (js-value _ #\:) and (js-value _ #\,)
(define (read-js-event inp)
  (let* ([start-loc (port->source-location inp)]
         [source-location
          (lambda ()
            (and (port-count-lines-enabled)
                 (build-source-location-vector
                  start-loc (port->source-location inp))))])
    (match inp
      [(app peek-char (? eof-object? v)) v]

      [(peek #px"^\\s+")
       (read-js-event inp)]

      [(peek "null")
       (js-value (source-location) 'null)]

      [(peek "true")
       (js-value (source-location) #t)]

      [(peek "false")
       (js-value (source-location) #f)]

      [(peek #\{)
       (js-object-start (source-location))]

      [(peek #\})
       (js-object-end (source-location))]

      [(peek #\[)
       (js-array-start (source-location))]

      [(peek #\])
       (js-array-end (source-location))]

      [(peek #\:) (js-value (source-location) #\:)]

      [(peek #\,) (js-value (source-location) #\,)]

      ;; XXX: understand exponential notation
      [(peek #px"^-?\\d+(\\.\\d+)?" s)
       (js-value (source-location) (string->number s))]

      [(peek #\")
       (let ([s (read-js-string inp)])
         (js-value (source-location) s))])))

(define (port->js-stream inp)
  (sequence->stream
   (in-port read-js-event inp)))

(module+ test
  (let-syntax ([test-read
                (syntax-rules ()
                  [(_ input token)
                   (test-case
                    (~a "read " 'token " token")
                    (check-equal?
                     (call-with-input-string input read-js-event)
                     token))]
                  [(_ input token0 token ...)
                   (test-case
                    (~a "read " 'token0 " token starting sequence")
                    (check-equal?
                     (call-with-input-string
                      input (lambda (inp)
                              (sequence->list (in-port read-js-event inp))))
                     (list token0 token ...)))]
                  )])
    (test-read " {" (js-object-start #f))
    (test-read " }" (js-object-end #f))
    (test-read " [" (js-array-start #f))
    (test-read " ]" (js-array-end #f))
    (test-read " null" (js-value #f 'null))
    (test-read " :" (js-value #f #\:))
    (test-read " ," (js-value #f #\,))
    (test-read " true" (js-value #f #t))
    (test-read " false" (js-value #f #f))

    (test-read "42" (js-value #f 42))
    (test-read "-3" (js-value #f -3))
    (test-read "3.14159" (js-value #f 3.14159))

    (test-read " { } " (js-object-start #f) (js-object-end #f))
    (test-read "{ \"a\": \"b\", \"c\": 42 }"
               (js-object-start #f)
               (js-value #f "a") (js-value #f #\:)
               (js-value #f "b") (js-value #f #\,)
               (js-value #f "c") (js-value #f #\:)
               (js-value #f 42)
               (js-object-end #f)))
  )

;; minimal stream and stream-cons match expanders
;; stream /only/ matches on first two elements and the rest
(define-match-expander stream-cons
  (syntax-rules ()
    [(_ a b)
     (? stream?
        (not (? stream-empty?))
        (app (lambda (s)
               (values (stream-first s)
                       (stream-rest s)))
             a b))])
  (make-rename-transformer #'$tream-cons))

(define-match-expander stream
  (syntax-rules ()
    [(_ a b c)
     (stream-cons a (stream-cons b c))])
  (make-rename-transformer #'$tream))


;; clean-js-delimiters
;; stream[U js-event #\: #\,] -> stream[js-event]
;; converts <string, #\:> sequences to js-object-key and removes #\,
(define (clean-js-delimiters s)
  (match s
    [(? stream-empty?) s]
    [(stream (js-value loc (? string? k)) #\: s)
     (stream-cons (js-object-key loc k)
                  (clean-js-delimiters s))]
    [(stream-cons #\, s) (clean-js-delimiters s)]
    [(stream-cons x s) (stream-cons x (clean-js-delimiters s))]))

;; wf-js-stream
;; stream[js-event] -> stream[js-event]
;; checks a stream for well-formedness as it is streaming
;; raises an exception when not well-formed
#|
  XXX: perhaps generalize what happens with a non-well-formed stream
    stream[js-event]
    (some-error-state -> stream[js-event])
      -> stream[js-event]

  error handler returns a stream of events to replace/splice into the
  output stream
|#

(define (wf-js-stream s)
  ; XXX: use source location in reporting
  (define (wf-error expected actual)
    (error 'wf-js-stream
           "incorrect state expected: ~a, got: ~a"
           expected
           actual))

  (define (next check s)
    (cond
      [(stream-empty? s) s]
      [else
       (let* ([v (stream-first s)]
              [s (stream-rest s)]
              [check (check v)])
         (stream-cons v (next check s)))]))

  (define (check-value v)
    (cond
      [(js-value? v)         check-value]
      [(js-array-start? v)  (check-array check-value)]
      [(js-object-start? v) (check-object-key check-value)]
      [else
       (wf-error '(atom array-start object-start) v)]))

  (define ((check-array k) v)
    (cond
      [(js-value? v)         (check-array k)]
      [(js-array-start? v)  (check-array (check-array k))]
      [(js-array-end? v)    k]
      [(js-object-start? v) (check-object-key (check-array k))]
      [else
       (wf-error '(atom array-start array-end object-start)
                 v)]))

  (define ((check-object-key k) v)
    (cond
      [(js-object-key? v) (check-object-value k)]
      [(js-object-end? v) k]
      [else
       (wf-error '(object-key) v)]))

  (define ((check-object-value k) v)
    (cond
      [(js-value? v) (check-object-key k)]
      [(js-array-start? v)  (check-array (check-object-key k))]
      [(js-object-start? v) (check-object-key (check-object-key k))]
      [else
       (wf-error '(atom array-start object-start) v)]))

  (next check-value s))

(module+ test
  (test-case "wf-js-stream atoms - ok"
             (let ([s (list (js-value #f 1) (js-value #f 2))])
               (for ([a (in-list s)]
                     [b (in-stream (wf-js-stream s))])
                 (check-equal? a b))))

  (test-case "wf-js-stream atoms - not empty stream"
             (let ([s (list (js-value #f 1) (js-value #f 2))])
               (check-false (stream-empty? (wf-js-stream s)))))

  (test-case "wf-js-stream array - ok"
             (let ([s (list (js-array-start #f)
                            (js-value #f 42)
                            (js-array-end #f))])
               (for ([a (in-list s)]
                     [b (in-stream (wf-js-stream s))])
                 (check-equal? a b))))

  (test-case "wf-js-stream deeper array - ok"
             (let ([s (list (js-array-start #f)
                            (js-array-start #f)
                            (js-value #f 42)
                            (js-array-end #f)
                            (js-array-end #f))])
               (for ([a (in-list s)]
                     [b (in-stream (wf-js-stream s))])
                 (check-equal? a b))))

  (test-case "wf-js-stream object - ok"
             (let ([s (list (js-object-start #f)
                            (js-object-key #f 'foo)
                            (js-value #f 42)
                            (js-object-end #f))])
               (for ([a (in-list s)]
                     [b (in-stream (wf-js-stream s))])
                 (check-equal? a b))))

  (test-case "wf-js-stream object - malformed kv sequence"
             (let ([s (list (js-object-start #f)
                            (js-object-key #f 'foo)
                            (js-value #f 42)
                            (js-value #f "bad")
                            (js-object-end #f))])
               (check-exn #px"incorrect state"
                          (lambda ()
                            (stream->list (wf-js-stream s))))))
  )



;; js-stream->jsexpr
;; stream[js-event] -> <jsexpr, stream[js-event]>
;; expects a well formed stream of events
(define (js-stream->jsexpr s)
  (match s
    [(stream-cons (js-value _ v) s) (values v s)]
    [(stream-cons (js-array-start _) s)
     (js-stream->jsexpr-list s)]
    [(stream-cons (js-object-start _) s)
     (js-stream->jsexpr-hash (hasheq) s)]))

;; js-stream->jsexpr-list
;; stream[js-event] -> <jslist, stream[js-event]>
(define (js-stream->jsexpr-list s)
  (match s
    [(stream-cons (js-array-end _) s) (values null s)]
    [_
     (let*-values ([(head s) (js-stream->jsexpr s)]
                   [(tail s) (js-stream->jsexpr-list s)])
       (values (cons head tail) s))]))

;; js-stream->jsexpr-hash
;; hash stream[js-event] -> <jshash, stream[js-event]>
(define (js-stream->jsexpr-hash acc s)
  (match s
    [(stream-cons (js-object-end _) s) (values acc s)]
    [(stream-cons (js-object-key _ k) s)
     (let*-values ([(v s) (js-stream->jsexpr s)])
       (js-stream->jsexpr-hash (hash-set acc k v) s))]))

(module+ test
  (test-case "stream->jsexpr one value"
             (let-values ([(v s) (js-stream->jsexpr
                                  (list (js-value #f 'null)))])
               (check-eq? v 'null)
               (check-true (stream-empty? s))))

  (test-case "stream->jsexpr two value"
             (let ([e* (list (js-value #f 'null) (js-value #f 42))])
               (let-values ([(v s) (js-stream->jsexpr e*)])
                 (check-eq? v 'null)
                 (check-false (stream-empty? s))
                 (check-equal? (stream-first s) (js-value #f 42)))))

  (test-case "stream->jsexpr array"
             (let ([e* (list (js-array-start #f)
                             (js-value #f 1)
                             (js-value #f 2)
                             (js-value #f 3)
                             (js-array-end #f))])
               (let-values ([(v s) (js-stream->jsexpr e*)])
                 (check-equal? v (list 1 2 3))
                 (check-true (stream-empty? s)))))

  (test-case "stream->jsexpr hash"
             (let ([e* (list (js-object-start #f)
                             (js-object-key #f 'a)
                             (js-array-start #f)
                             (js-value #f 1)
                             (js-value #f 2)
                             (js-value #f 3)
                             (js-array-end #f)
                             (js-object-key #f 'b)
                             (js-array-start #f)
                             (js-value #f 4)
                             (js-value #f 5)
                             (js-value #f 6)
                             (js-array-end #f)
                             (js-object-end #f))])
               (let-values ([(v s) (js-stream->jsexpr e*)])
                 (check-equal? v (hasheq 'a (list 1 2 3)
                                         'b (list 4 5 6)))
                 (check-true (stream-empty? s)))))
  )

;; jsexpr->js-stream
;; jsexpr -> stream[js-event]
(define (jsexpr->js-stream e)
  (sequence->stream
   (in-generator (jsexpr-walk e yield))))

;; jsexpr-walk
;; jsexpr (js-event -> void) -> void
(define (jsexpr-walk e callback)
  (match e
    [(or (? number?)
         (? string?)
         (? boolean?))
     (callback (js-value #f e))]

    ['null
     (callback (js-value #f 'null))]

    [(hash-table (k* v*) ...)
     (callback (js-object-start #f))
     (for ([k (in-list k*)]
           [v (in-list v*)])
       (callback (js-object-key #f k))
       (jsexpr-walk v callback))
     (callback (js-object-end #f))]

    [(list e* ...)
     (callback (js-array-start #f))
     (for ([e (in-list e*)])
       (jsexpr-walk e callback))
     (callback (js-array-end #f))]))

(module+ test
  (let ([o (hash 'a '(1 2 3 4 null) 'b "c")])
    (test-equal? "jsexpr->js-stream"
                 (stream->list (jsexpr->js-stream o))
                 (list (js-object-start #f)
                       (js-object-key #f 'a)
                       (js-array-start #f)
                       (js-value #f 1)
                       (js-value #f 2)
                       (js-value #f 3)
                       (js-value #f 4)
                       (js-value #f 'null)
                       (js-array-end #f)
                       (js-object-key #f 'b)
                       (js-value #f "c")
                       (js-object-end #f))))
  )

