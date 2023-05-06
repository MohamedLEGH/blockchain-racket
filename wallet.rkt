#lang racket
(require "crypto-utils.rkt")
(require "schnorr.rkt")
(require "base58check.rkt")
(require "bech32.rkt")

;; ALL VALUES ARE IN HEXA
; todo : testnet sufix
; compressed wif keys: c instead of 9 for testnet)
;; mainnet prefix
(define bitcoin_wifprefix "80")
(define bitcoin_wifsufix_compressed "01")
(define bitcoin_pubkeyprefix "04")
(define bitcoin_pubkeyprefix_even "02")
(define bitcoin_pubkeyprefix_odd "03")
(define bitcoin_addrprefix "00")
(define bitcoin_addrprefix_scripthash
  "05") ; Version byte is 5 for a main-network address, 196 for a testnet address
;; opcodes
(define OP_0 "00")
(define OP_DUP "76")
(define OP_HASH160 "a9")
(define OP_EQUALVERIFY "88")
(define OP_CHECKSIG "ac")
;(define OP_CHECKMULTISIG "ae")
;(define OP_1 "81")

(define (pushdataval val) ; val in hexa
  (number->string (/ (string-length val) 2) 16))

(define (generate_pk)
  (number->string (generate_random) 16))

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

(define (pubhex_to_pubschnorr pub)
  (substring pub 0 64))

(define (pubkey_to_compressed pubkey)
  (pub_to_compressed (substring pubkey 2)))

(define (generate_checksum key)
  (substring (doublesha256 key) 0 8))

(define (private_to_wif
         pk
         #:compressed [compressed #t]) ; key in hexa string format
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

(define (private_to_pubkey pk
                           #:compressed [compressed #t]
                           #:version [version 1]) ; pk in hex format string
  (when (and (not (= version 0)) (equal? compressed #f))
    (error "pubkeys can only be compressed in version 0"))
  (define pub_val (priv_to_pub pk))
  (if (= version 0)
      (if compressed
          (pub_to_compressed pub_val)
          (string-append bitcoin_pubkeyprefix pub_val))
      (pubhex_to_pubschnorr pub_val)))

(define (pubkey_to_P2PK pubkey)
  (define pushdata_val (pushdataval pubkey))
  (string-append pushdata_val pubkey OP_CHECKSIG))

(define (pubkey_to_pubkeyhash pubkey)
  (define pubkeyhash (string-append bitcoin_addrprefix (hash160 pubkey)))
  (define checksum (generate_checksum pubkeyhash))
  (define pubkeyhash_with_checksum (string-append pubkeyhash checksum))
  (base58check_encode pubkeyhash_with_checksum))

(define (private_to_pubkeyhash pk #:compressed [compressed #t])
  (define pubkey (private_to_pubkey pk #:version 0 #:compressed compressed))
  (pubkey_to_pubkeyhash pubkey))

(define (pubkeyhashbase58_to_pubkeyhashhex
         pubkeyhash) ; from base58 format to hex format of hash160(pubkey)
  (define hexa_val (base58check_decode pubkeyhash))
  (define hexa_val_l (string-length hexa_val))
  (define val_no_checksum (substring hexa_val 0 (- hexa_val_l 8)))
  (define val_no_prefix (substring val_no_checksum 2))
  val_no_prefix)
; La scriptPubKey dans P2PKH est la suivante :
; OP_DUP OP_HASH160 <hash de clé publique> OP_EQUALVERIFY OP_CHECKSIG
;scriptPubKey: OP_HASH160 [20-byte-hash of {[pubkey] OP_CHECKSIG} ] OP_EQUAL
; Our scriptSig needs to take the form: <0 0x14 <20-byte-key-hash>>
; return a scriptPubKey
(define (pubkeyhash_to_P2PKH pubkeyhash)
  (define hexa_pubkey (pubkeyhashbase58_to_pubkeyhashhex pubkeyhash))
  (define pushdata_val (pushdataval hexa_pubkey))
  (string-append OP_DUP
                 OP_HASH160
                 pushdata_val
                 hexa_pubkey
                 OP_EQUALVERIFY
                 OP_CHECKSIG))

(define (pubkey_to_pubkeyscripthash
         pubkey) ; pubkey in hex format with pubkey prefix
  (define script (pubkey_to_P2PK pubkey))
  (define address
    (string-append bitcoin_addrprefix_scripthash (hash160 script)))
  (base58check_encode (string-append address (generate_checksum address))))

(define (private_to_pubkeyscripthash pk #:compressed [compressed #t])
  (define pubkey (private_to_pubkey pk #:version 0 #:compressed compressed))
  (pubkey_to_pubkeyscripthash pubkey))

(define (pubkey_to_nestedpubkeyhash pubkey) ; compressed key is mandatory
  (define hash160_val (hash160 pubkey))
  (define pushdata_val (pushdataval hash160_val))
  (define script (string-append OP_0 pushdata_val hash160_val))
  (define address
    (string-append bitcoin_addrprefix_scripthash (hash160 script)))
  (base58check_encode (string-append address (generate_checksum address))))

(define (private_to_nestedpubkeyhash pk #:compressed [compressed #t])
  (define pubkey (private_to_pubkey pk #:version 0 #:compressed compressed))
  (pubkey_to_nestedpubkeyhash pubkey))

(define (pubkey_to_bech32
         pubkey
         #:version [version 1]) ; compressed pubkey is mandatory
  (define val_to_encode (if (= version 0) (hash160 pubkey) pubkey))
  (bech32_encode val_to_encode #:version version))

(define (private_to_bech32 pk #:version [version 1])
  (define pub
    (private_to_pubkey pk #:version version)) ; compressed keys are mandatory
  (pubkey_to_bech32 pub #:version version))

(define (pubkey_to_bech32taproot
         pubkey) ; pub as a x-only public key in hex format
  (define Qx (tweak_pubkey pubkey "")) ; tweak pubkey with a "NULL" value
  ; convert to bech32
  (pubkey_to_bech32 Qx))

(define (private_to_bech32taproot pk)
  (define pub (private_to_pubkey pk))
  (pubkey_to_bech32taproot pub))

(provide generate_checksum
         generate_pk
         private_to_wif
         wif_to_private
         private_to_pubkey
         pubkey_to_pubkeyhash
         private_to_pubkeyhash
         pub_to_compressed
         pubhex_to_pubschnorr
         pubkey_to_compressed
         pubkey_to_P2PK
         pubkeyhashbase58_to_pubkeyhashhex
         pubkeyhash_to_P2PKH
         pubkey_to_pubkeyscripthash
         private_to_pubkeyscripthash
         pubkey_to_nestedpubkeyhash
         private_to_nestedpubkeyhash
         pubkey_to_bech32
         private_to_bech32
         pubkey_to_bech32taproot
         private_to_bech32taproot)
