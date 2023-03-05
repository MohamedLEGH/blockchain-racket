#lang racket
(require "block.rkt")

; tx_pool is a list of transactions
(struct blockchain (blocklist tx_pool 
                    difficulty_bit_level) #:prefab)

(define (blocklist->string bl)
  (define (string-append/block b str)
    (string-append (block->string b) str))
  (foldl string-append/block "" bl))

(define (blockchain->string bc)
  (string-append (blocklist->string (blockchain-blocklist bc))
                 (txlist->string (blockchain-tx_pool bc))
                 (number->string (blockchain-difficulty_bit_level bc))))

(provide (struct-out blockchain) 
         blocklist->string
         blockchain->string
         ;add_block_chain 
         (all-from-out "block.rkt"))