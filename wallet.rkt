#lang racket
(require racket/random)
(require "curve.rkt")

(define max_32bitvalue #xffffffff)
(define bitcoin_wifprefix #x80)
(define bitcoin_addrprefix #x00)

(define (generate_random)
  (for/fold ([result 0]) ([byte (in-bytes (crypto-random-bytes 32))])
    (+ byte (* result 256))))

(define (generate_private_key)
  (define p (generate_random))
  (if (and (< p N) (> p 0)) p (generate_private_key)))

(provide generate_private_key)
