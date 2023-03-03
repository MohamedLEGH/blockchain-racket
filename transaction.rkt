#lang racket

(struct tx (sender receiver value) #:prefab)

(define (tx->string tx_val)
  (string-append (tx-sender tx_val) (tx-receiver tx_val) (number->string (tx-value tx_val))))

(provide (struct-out tx) tx->string)