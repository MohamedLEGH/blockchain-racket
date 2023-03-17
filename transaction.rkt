#lang racket
(require json)
; need to add verif tx
(struct tx (sender receiver value) #:prefab)

(define (tx->jsexpr tx_val)
  (hash 'sender
        (tx-sender tx_val)
        'receiver
        (tx-receiver tx_val)
        'value
        (tx-value tx_val)))

(define (tx->string tx_val)
  (string-append (tx-sender tx_val)
                 (tx-receiver tx_val)
                 (number->string (tx-value tx_val))))

(provide (struct-out tx)
         tx->string
         tx->jsexpr)
