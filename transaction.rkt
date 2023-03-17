#lang racket
(require json)
; need to add verif tx
(struct tx (sender receiver value) #:prefab)

(define (tx->jsexpr tx)
  (hash 'sender (tx-sender tx) 'receiver (tx-receiver tx) 'value (tx-value tx)))

(define (tx->string tx_val)
  (string-append (tx-sender tx_val)
                 (tx-receiver tx_val)
                 (number->string (tx-value tx_val))))

(provide (struct-out tx)
         tx->string
         tx->jsexpr)
