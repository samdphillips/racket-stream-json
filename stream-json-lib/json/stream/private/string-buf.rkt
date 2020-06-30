#lang racket/base

(provide make-string-buf
         string-buf->string
         string-buf-write-char!
         string-buf-write-byte!)

(struct string-buf (store pos) #:mutable)

(define (make-string-buf)
  (string-buf (make-string 16) 0))

(define (string-buf->string sbuf)
  (define store (string-buf-store sbuf))
  (define pos (string-buf-pos sbuf))
  (substring store 0 pos))

(define (string-buf-full? sbuf)
  (= (string-buf-pos sbuf)
     (string-length
       (string-buf-store sbuf))))

(define (string-buf-grow! sbuf)
  (define pos (string-buf-pos sbuf))
  (define new-store (make-string (* 2 pos)))
  (string-copy! new-store 0 (string-buf-store sbuf) 0 pos)
  (set-string-buf-store! sbuf new-store))

(define (string-buf-write-char! sbuf ch)
  (when (string-buf-full? sbuf)
    (string-buf-grow! sbuf))
  (define pos (string-buf-pos sbuf))
  (string-set! (string-buf-store sbuf) pos ch)
  (set-string-buf-pos! sbuf (add1 pos)))

(define (string-buf-write-byte! sbuf c)
  (string-buf-write-char! sbuf (integer->char c)))

