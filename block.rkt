#lang racket
(require sha)
(require "transaction.rkt")

; need to add reward ??
; need to add verif block
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
  (define (string-append/tx tx str)
    (string-append (tx->string tx) str))
  (foldl string-append/tx "" txlist))

(define (block->string b)
  (string-append (number->string (block-index b)) 
                 (block-previous_hash b) 
                 (number->string (block-nonce b))
                 (number->string (block-timestamp b))
                 (txlist->string (block-tx_list b))
                 (block-miner_address b)
                 (block-hash_val b)))

(define (hash_block b n)
  (define new_block (struct-copy block b [nonce n]))
  (bytes->hex-string (sha256 (string->bytes/utf-8 (block->string new_block)))))

(define (mine_block b difficulty_bit_level)
   ; return final nonce
   ; target is 2^(256-difficulty)-1
  (define target (- (expt 2 (- 256 difficulty_bit_level)) 1))
  (define (proof_of_work b acc)
    (define hash_val (string->number (hash_block b acc) 16)) ; hexstring to number
    (if (< hash_val target)
        acc
        (proof_of_work b (add1 acc))))
  (proof_of_work b 0))

(provide (struct-out block) 
         add_tx_block
         txlist->string
         block->string
         hash_block
         mine_block
         (all-from-out "transaction.rkt"))