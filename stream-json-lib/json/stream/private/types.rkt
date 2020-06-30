#lang racket/base

(provide (struct-out json-value)
         (struct-out json-delimiter)
         (struct-out json-object-start)
         (struct-out json-object-end)
         (struct-out json-array-start)
         (struct-out json-array-end)
         (struct-out json-member-start)
         (struct-out json-member-end)
         json-value-string?
         json-value-number?
         json-value-boolean?
         json-event?
         json-event-location)

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

