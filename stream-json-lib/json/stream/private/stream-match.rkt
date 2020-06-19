#lang racket/base

(provide (all-defined-out)
         (all-from-out racket/stream))

(require racket/match
         (rename-in racket/stream
                    [stream* $tream*]
                    [stream-cons $tream-cons])
         (for-syntax racket/base))

;; minimal stream* and stream-cons match expanders

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

(define-match-expander stream*
  (syntax-rules ()
    [(_ a) a]
    [(_ a b ...)
     (stream-cons a (stream* b ...))])
  (make-rename-transformer #'$tream*))
