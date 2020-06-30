#lang racket/base

(require racket/contract
         racket/stream
         syntax/srcloc

         json/stream/private/reader
         json/stream/private/stream
         json/stream/private/types)

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
   (->* (input-port?) (#:well-formed? boolean?) (stream/c json-event?)))
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
