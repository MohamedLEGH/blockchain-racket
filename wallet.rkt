#lang racket
(require racket/random)
(require "crypto.rkt")

(define max_32bitvalue #xffffffff)
(define bitcoin_wifprefix #x80)
(define bitcoin_addrprefix #x00)

(provide generate_private_key)
