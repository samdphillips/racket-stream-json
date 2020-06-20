#lang racket/base

(provide (all-defined-out))

(require racket/match
         (for-syntax racket/base
                     syntax/parse))

(define ($peek-byte inp)
  (values (peek-byte inp) inp))

(define ($peek-char inp)
  (values (peek-char inp) inp))

(define (($peek-bytes count) inp)
  (values (peek-bytes count 0 inp) inp))

(define (($peek-string count) inp)
  (values (peek-string count 0 inp) inp))

(define (($read-bytes count) inp)
  (read-bytes count inp))

(define (($read-string count) inp)
  (read-string count inp))

(define (($regexp-try-match pat) inp)
  (match (regexp-match pat (peek-bytes 1024 0 inp))
    [(and match-val (cons mstr _))
     (values match-val (lambda () (read-bytes (bytes-length mstr) inp)))]
    [_ (values #f #f)]))

(define (doit f) (f))

(begin-for-syntax
  (define-syntax-class regexp
    #:description "regular expression"
    (pattern x #:when (regexp? (syntax-e #'x)))))

(define-match-expander peek
  (lambda (stx)
    (let ([$peek (lambda (pat peek read)
                   #`(app #,peek #,pat (app #,read _)))])
      (syntax-parse stx
        [(peek bpat:bytes)
         #:with blen (bytes-length (syntax-e #'bpat))
         ($peek #'bpat #'($peek-bytes 'blen) #'($read-bytes 'blen))]

        [(peek spat:string)
         #:with slen (string-length (syntax-e #'spat))
         ($peek #'spat #'($peek-string 'slen) #'($read-string 'slen))]

        [(peek cpat:char)
         (let ([bval (char->integer (syntax->datum #'cpat))])
           (if (< 128 bval)
               ($peek (datum->syntax #'cpat bval) #'$peek-byte #'read-byte)
               ($peek #'cpat #'$peek-char #'read-char)))]

        [(peek rxpat:regexp pat ...)
         ($peek #'(and (not #f)
                       (list-rest pat ... _))
                #'($regexp-try-match rxpat)
                #'doit)]))))
