#lang racket
(require racket/random)
(require "crypto.rkt")
(require "base58.rkt")

(define bitcoin_wifprefix "80")
(define bitcoin_pubkeyprefix "04")
(define bitcoin_addrprefix "00")

; value is a hexastring
(define (base58check_encode value)
    (define regex_val (regexp-match #rx"^[0]*" value))
    (define nb_0 (string-length (car regex_val)))
    (define nb_1 (quotient nb_0 2))
    (define prefix (make-string nb_1 #\1))
    (string-append prefix (base58encode (string->number value 16))))

(define (base58check_decode value)
    (define regex_val (regexp-match #rx"^[1]*" value))
    (define nb_1 (string-length (car regex_val)))
    (define nb_0 (* nb_1 2))
    (define prefix (make-string nb_0 #\0))
    (string-append prefix (number->string (base58decode value) 16)))

(define (generate_checksum key)
    (substring (doublesha256 key) 0 8))

(define (private_to_wif pk) ; key in hexa string format
    ; todo : compressed keys (begin with K/L instead of 5, or c instead of 9 for testnet)
    (define pk_with_prefix (string-append bitcoin_wifprefix pk))
    (define checksum (generate_checksum pk_with_prefix))
    (define pk_with_checksum (string-append pk_with_prefix checksum))
    (base58check_encode pk_with_checksum))

(define (wif_to_private wif)
    (define wif_hex (base58check_decode wif))
    (define l_wif (string-length wif_hex))
    (define wif_drop_checksum (substring wif_hex 0 (- l_wif 8)))
    (define pk (substring wif_drop_checksum 2)) ; compressed : should remove last byte (2 hex)
    pk)

(define (private_to_pubkey pk)
    ; todo : make it work with compressed keys
    (string-append bitcoin_pubkeyprefix (priv_to_pub pk)))

(define (pubkey_to_pubkeyhash pubkey)
    (define pubkeyhash (string-append bitcoin_addrprefix (hash160 pubkey)))
    (define checksum (generate_checksum pubkeyhash))
    (define pubkeyhash_with_checksum (string-append pubkeyhash checksum))
    (base58check_encode pubkeyhash_with_checksum))

(define (private_to_pubkeyhash pk)
    (define pubkey (private_to_pubkey pk))
    (pubkey_to_pubkeyhash pubkey))

(provide base58check_encode
         base58check_decode
         private_to_wif
         wif_to_private
         private_to_pubkey
         pubkey_to_pubkeyhash
         private_to_pubkeyhash)

