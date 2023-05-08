#lang racket
(require "crypto-utils.rkt")
(require "transaction.rkt")

; need to add reward ??
; need to add verif block
(struct block
        (index previous-hash nonce timestamp tx-list miner-address hash-val)
  #:prefab)

(define (add-tx-block t b)
  (define new-list (cons t (block-tx-list b)))
  (define new-block (struct-copy block b [tx-list new-list]))
  new-block)

(define (txlist->string txlist)
  (define (string-append/tx tx str)
    (string-append (tx->string tx) str))
  (foldl string-append/tx "" txlist))

(define (block->string b)
  (string-append (number->string (block-index b))
                 (block-previous-hash b)
                 (number->string (block-nonce b))
                 (number->string (block-timestamp b))
                 (txlist->string (block-tx-list b))
                 (block-miner-address b)
                 (block-hash-val b)))

(define (txlist->jsexpr txlist)
  (define (build-txlist-jsexpr js acc)
    (cons (tx->jsexpr js) acc))
  (foldl build-txlist-jsexpr '() txlist))

(define (block->jsexpr b)
  (hash 'index
        (block-index b)
        'previous-hash
        (block-previous-hash b)
        'nonce
        (block-nonce b)
        'timestamp
        (block-timestamp b)
        'tx-list
        (txlist->jsexpr (block-tx-list b))
        'miner-address
        (block-miner-address b)
        'hash-val
        (block-hash-val b)))

(define (hash-block b n)
  (define new-block (struct-copy block b [nonce n]))
  (sha256-hex (string->bytes/utf-8 (block->string new-block))))

(define (mine-block b difficulty-bit-level)
  ; return final nonce
  ; target is 2^(256-difficulty)-1
  (define target (- (expt 2 (- 256 difficulty-bit-level)) 1))
  (define (proof-of-work b-val acc)
    (define hash-val
      (string->number (hash-block b-val acc) 16)) ; hexstring to number
    (if (< hash-val target) acc (proof-of-work b-val (add1 acc))))
  (proof-of-work b 0))

(provide (struct-out block)
         add-tx-block
         txlist->string
         block->string
         txlist->jsexpr
         block->jsexpr
         hash-block
         mine-block
         (all-from-out "transaction.rkt"))
