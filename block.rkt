#lang racket
(require racket/match)
(require "transaction.rkt")

(struct block (index 
               previous_hash 
               nonce 
               timestamp 
               tx_list 
               miner_address 
               hash_val) #:prefab)

(define (add_tx_block t b)
  (define new_list (cons t (block-tx_list b)))
  (define new_block (struct-copy block b [tx_list new_list]))
  new_block)

(define (txlist->string txlist)
  (define (txlist->string/acc acc txlist)
    (match (length txlist)
        [0 acc]
        [_ (txlist->string/acc (string-append acc (tx->string (car txlist))) 
                            (cdr txlist))]))
  (txlist->string/acc "" txlist))

; txlist->string acc txlist
; txlist((string-append ...) cdr txlist)
; start with acc ""

(define (block->string b)
  (string-append (number->string (block-index b)) 
                 (block-previous_hash b) 
                 (number->string (block-nonce b))
                 (block-timestamp b)
                 (txlist->string (block-tx_list b))
                 (block-miner_address b)
                 (block-hash_val b)))

(provide (struct-out block) 
         add_tx_block 
         txlist->string 
         block->string
         (all-from-out "transaction.rkt"))