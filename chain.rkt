#lang racket
(require "block.rkt")

; tx-pool is a list of transactions
; need to add block reward
; need to add verif chain
(struct blockchain (blocklist tx-pool difficulty-bit-level) #:prefab)

(define (add-block-chain b bc)
  (define new-list (cons b (blockchain-blocklist bc)))
  (define new-chain (struct-copy blockchain bc [blocklist new-list]))
  new-chain)

(define (add-tx-chain tx bc)
  (define new-list (cons tx (blockchain-tx-pool bc)))
  (define new-chain (struct-copy blockchain bc [tx-pool new-list]))
  new-chain)

(define (blocklist->string bl)
  (define (string-append/block b str)
    (string-append (block->string b) str))
  (foldl string-append/block "" bl))

(define (blockchain->string bc)
  (string-append (blocklist->string (blockchain-blocklist bc))
                 (txlist->string (blockchain-tx-pool bc))
                 (number->string (blockchain-difficulty-bit-level bc))))

(define (blocklist->jsexpr blocklist)
  (define (build-blocklist-jsexpr js acc)
    (cons (block->jsexpr js) acc))
  (foldl build-blocklist-jsexpr '() blocklist))

(define (blockchain->jsexpr bc)
  (hash 'blocklist
        (blocklist->jsexpr (blockchain-blocklist bc))
        'tx-pool
        (txlist->jsexpr (blockchain-tx-pool bc))
        'difficulty-bit-level
        (blockchain-difficulty-bit-level bc)))

; need to add mining price transaction
(define (create-genesis-block bc)
  (define timestamp (current-seconds))
  (define first-block (block 0 "" 0 timestamp '() "miner" ""))
  (define nonceval
    (mine-block first-block (blockchain-difficulty-bit-level bc)))
  (define hashval (hash-block first-block nonceval))
  (define first-block-mined
    (struct-copy block first-block [nonce nonceval] [hash-val hashval]))
  (define chain (add-block-chain first-block-mined bc))
  ; clearing the tx pool
  (define chain-clear (struct-copy blockchain chain [tx-pool '()]))
  chain-clear)

; need to add mining price transaction
(define (mine-new-block bc)
  (define timestamp (current-seconds))
  (define last-block (first (blockchain-blocklist bc)))
  (define index (add1 (block-index last-block)))
  (define previoushash (block-hash-val last-block))
  (define miner-addr "miner")
  (define txs (blockchain-tx-pool bc))
  (define new-block (block index previoushash 0 timestamp txs miner-addr ""))
  (define nonceval (mine-block new-block (blockchain-difficulty-bit-level bc)))
  (define hashval (hash-block new-block nonceval))
  (define first-block-mined
    (struct-copy block new-block [nonce nonceval] [hash-val hashval]))
  (define chain (add-block-chain first-block-mined bc))
  ; clearing the tx pool
  (define chain-clear (struct-copy blockchain chain [tx-pool '()]))
  chain-clear)

(define (mining-block bc)
  (if (equal? '() (blockchain-blocklist bc))
      (create-genesis-block bc)
      (mine-new-block bc)))

(provide (struct-out blockchain)
         add-block-chain
         add-tx-chain
         blocklist->string
         blockchain->string
         blocklist->jsexpr
         blockchain->jsexpr
         create-genesis-block
         mine-new-block
         mining-block
         (all-from-out "block.rkt"))
