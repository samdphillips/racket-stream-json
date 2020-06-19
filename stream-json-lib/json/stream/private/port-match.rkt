#lang racket/base

(provide (all-defined-out))

(require racket/match
         (for-syntax racket/base
                     syntax/parse))

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
