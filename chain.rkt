#lang racket
(require "block.rkt")

; tx_pool is a list of transactions
; need to add block reward
; need to add verif chain
(struct blockchain (blocklist tx_pool 
                    difficulty_bit_level) #:prefab)

(define (add_block_chain b bc)
  (define new_list (cons b (blockchain-blocklist bc)))
  (define new_chain (struct-copy blockchain bc [blocklist new_list]))
  new_chain)

(define (add_tx_chain tx bc)
  (define new_list (cons tx (blockchain-tx_pool bc)))
  (define new_chain (struct-copy blockchain bc [tx_pool new_list]))
  new_chain)

(define (blocklist->string bl)
  (define (string-append/block b str)
    (string-append (block->string b) str))
  (foldl string-append/block "" bl))

(define (blockchain->string bc)
  (string-append (blocklist->string (blockchain-blocklist bc))
                 (txlist->string (blockchain-tx_pool bc))
                 (number->string (blockchain-difficulty_bit_level bc))))

; need to add mining price transaction
(define (create_genesis_block bc)
  (define timestamp (current-seconds))
  (define first_block (block 0 "" 0 timestamp '() "miner" ""))
  (define nonceval (mine_block first_block (blockchain-difficulty_bit_level bc)))
  (define hashval (hash_block first_block nonceval))
  (define first_block_mined (struct-copy block first_block [nonce nonceval] [hash_val hashval]))
  (define chain (add_block_chain first_block_mined bc))
  ; clearing the tx pool
  (define chain_clear (struct-copy blockchain chain [tx_pool '()]))
  chain_clear)

; need to add mining price transaction
(define (mine_new_block bc)
  (define timestamp (current-seconds))
  (define last_block (first (blockchain-blocklist bc)))
  (define index (add1 (block-index last_block)))
  (define previoushash (block-hash_val last_block))
  (define miner_addr "miner")
  (define txs (blockchain-tx_pool bc))
  (define new_block (block index previoushash 0 timestamp txs miner_addr ""))
  (define nonceval (mine_block new_block (blockchain-difficulty_bit_level bc)))
  (define hashval (hash_block new_block nonceval))
  (define first_block_mined (struct-copy block new_block [nonce nonceval] [hash_val hashval]))
  (define chain (add_block_chain first_block_mined bc))
  ; clearing the tx pool
  (define chain_clear (struct-copy blockchain chain [tx_pool '()]))
  chain_clear)

(provide (struct-out blockchain) 
         add_block_chain
         add_tx_chain
         blocklist->string
         blockchain->string
         create_genesis_block
         mine_new_block
         (all-from-out "block.rkt"))