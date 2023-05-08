#lang racket
; need to add verif tx
(struct tx (sender receiver value) #:prefab)

(define (tx->jsexpr tx-val)
  (hash 'sender
        (tx-sender tx-val)
        'receiver
        (tx-receiver tx-val)
        'value
        (tx-value tx-val)))

(define (tx->string tx-val)
  (string-append (tx-sender tx-val)
                 (tx-receiver tx-val)
                 (number->string (tx-value tx-val))))

(provide (struct-out tx)
         tx->string
         tx->jsexpr)
