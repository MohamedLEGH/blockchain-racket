#lang racket
(require "crypto.rkt")
(require "base58.rkt")

;;
; todo : testnet sufix
; compressed wif keys: c instead of 9 for testnet)
;; mainnet prefix
(define bitcoin_wifprefix "80")
(define bitcoin_wifsufix_compressed "01")
(define bitcoin_pubkeyprefix "04")
(define bitcoin_pubkeyprefix_even "02")
(define bitcoin_pubkeyprefix_odd "03")
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

(define (pub_to_compressed pub)
    ; pub parameter no prefix 
    (define x (substring pub 0 64))
    (define y (substring pub 64))
    (define y_number (string->number y 16))
    (define prefix 
        (if (= (modulo y_number 2) 0)
            bitcoin_pubkeyprefix_even
            bitcoin_pubkeyprefix_odd))
    (string-append prefix x))

(define (pubkey_to_compressed pubkey)
    (pub_to_compressed (substring pubkey 2)))

(define (generate_checksum key)
    (substring (doublesha256 key) 0 8))

(define (private_to_wif pk #:compressed [compressed #t]) ; key in hexa string format
    (define pk_with_prefix 
        (if (equal? compressed #t)
            (string-append bitcoin_wifprefix pk bitcoin_wifsufix_compressed)
            (string-append bitcoin_wifprefix pk)))
    (define checksum (generate_checksum pk_with_prefix))
    (define pk_with_checksum (string-append pk_with_prefix checksum))
    (base58check_encode pk_with_checksum))

(define (wif_to_private wif)
    (define prefix (string-ref wif 0))
    (define wif_hex (base58check_decode wif))
    (define l_wif (string-length wif_hex))
    (define wif_drop_checksum (substring wif_hex 0 (- l_wif 8)))
    (define pk (substring wif_drop_checksum 2))
    (define l_pk (string-length pk))
    (case prefix
        [(#\L #\K) (substring pk 0 (- l_pk 2))]
        [(#\5) pk]
        [else (error "wrong format for wif")]))

(define (private_to_pubkey pk #:compressed [compressed #t])
    (define pub_val (priv_to_pub pk))
    (if compressed
        (pub_to_compressed pub_val)
        (string-append bitcoin_pubkeyprefix pub_val)))

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
         private_to_pubkeyhash
         pub_to_compressed
         pubkey_to_compressed)

