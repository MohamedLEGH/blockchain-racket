#lang racket
(require base58check
         bech32
         crypto-sign)

;; ALL VALUES ARE IN HEXA
; todo : testnet sufix
; compressed wif keys: c instead of 9 for testnet)
;; mainnet prefix
(define bitcoin-wifprefix "80")
(define bitcoin-wifsufix-compressed "01")
(define bitcoin-pubkeyprefix "04")
(define bitcoin-pubkeyprefix-even "02")
(define bitcoin-pubkeyprefix-odd "03")
(define bitcoin-addrprefix "00")
(define bitcoin-addrprefix-scripthash
  "05") ; Version byte is 5 for a main-network address, 196 for a testnet address
;; opcodes
(define OP-0 "00")
(define OP-DUP "76")
(define OP-HASH160 "a9")
(define OP-EQUALVERIFY "88")
(define OP-CHECKSIG "ac")
;(define OP-CHECKMULTISIG "ae")
;(define OP-1 "81")

(define (pushdataval val) ; val in hexa
  (number->string (/ (string-length val) 2) 16))

(define (generate-pk)
  (number->string (generate-random-value) 16))

(define (pub-to-compressed pub)
  ; pub parameter no prefix
  (define x (substring pub 0 64))
  (define y (substring pub 64))
  (define y-number (string->number y 16))
  (define prefix
    (if (= (modulo y-number 2) 0)
        bitcoin-pubkeyprefix-even
        bitcoin-pubkeyprefix-odd))
  (string-append prefix x))

(define (pubhex-to-pubschnorr pub)
  (substring pub 0 64))

(define (pubkey-to-compressed pubkey)
  (pub-to-compressed (substring pubkey 2)))

(define (generate-checksum-base58 key)
  (substring (doublesha256 key) 0 8))

(define (private-to-wif
         pk
         #:compressed [compressed #t]) ; key in hexa string format
  (define pk-with-prefix
    (if (equal? compressed #t)
        (string-append bitcoin-wifprefix pk bitcoin-wifsufix-compressed)
        (string-append bitcoin-wifprefix pk)))
  (define checksum (generate-checksum-base58 pk-with-prefix))
  (define pk-with-checksum (string-append pk-with-prefix checksum))
  (base58check-encode pk-with-checksum))

(define (validate-checksum-base58 base58address)
  (define hash (base58check-decode base58address))
  (define l-hash (string-length hash))
  (define data (substring hash 0 (- l-hash 8)))
  (define checksum (substring hash (- l-hash 8)))
  (equal? (generate-checksum-base58 data) checksum))

(define (wif-to-private wif)
  (when (not (validate-checksum-base58 wif))
    (error "checksum of the wif is not valid"))
  (define prefix (string-ref wif 0))
  (define wif-hex (base58check-decode wif))
  (define l-wif (string-length wif-hex))
  (define wif-drop-checksum (substring wif-hex 0 (- l-wif 8)))
  (define pk (substring wif-drop-checksum 2))
  (define l-pk (string-length pk))
  (case prefix
    [(#\L #\K) (substring pk 0 (- l-pk 2))]
    [(#\5) pk]
    [else (error "wrong format for wif")]))

(define (private-to-pubkey pk
                           #:compressed [compressed #t]
                           #:version [version 1]) ; pk in hex format string
  (when (and (not (= version 0)) (equal? compressed #f))
    (error "pubkeys can only be compressed in version 0"))
  (define pub-val (priv-to-pub pk))
  (if (= version 0)
      (if compressed
          (pub-to-compressed pub-val)
          (string-append bitcoin-pubkeyprefix pub-val))
      (pubhex-to-pubschnorr pub-val)))

(define (pubkey-to-P2PK pubkey)
  (define pushdata-val (pushdataval pubkey))
  (string-append pushdata-val pubkey OP-CHECKSIG))

(define (pubkey-to-pubkeyhash pubkey)
  (define pubkeyhash (string-append bitcoin-addrprefix (hash160 pubkey)))
  (define checksum (generate-checksum-base58 pubkeyhash))
  (define pubkeyhash-with-checksum (string-append pubkeyhash checksum))
  (base58check-encode pubkeyhash-with-checksum))

(define (private-to-pubkeyhash pk #:compressed [compressed #t])
  (define pubkey (private-to-pubkey pk #:version 0 #:compressed compressed))
  (pubkey-to-pubkeyhash pubkey))

(define (pubkeyhashbase58-to-pubkeyhashhex
         pubkeyhash) ; from base58 format to hex format of hash160(pubkey)
  (when (not (validate-checksum-base58 pubkeyhash))
    (error "checksum of the address is not valid"))
  (define hexa-val (base58check-decode pubkeyhash))
  (define hexa-val-l (string-length hexa-val))
  (define val-no-checksum (substring hexa-val 0 (- hexa-val-l 8)))
  (define val-no-prefix (substring val-no-checksum 2))
  val-no-prefix)

; La scriptPubKey dans P2PKH est la suivante :
; OP-DUP OP-HASH160 <hash de clé publique> OP-EQUALVERIFY OP-CHECKSIG
;scriptPubKey: OP-HASH160 [20-byte-hash of {[pubkey] OP-CHECKSIG} ] OP-EQUAL
; Our scriptSig needs to take the form: <0 0x14 <20-byte-key-hash>>
; return a scriptPubKey
(define (pubkeyhash-to-P2PKH pubkeyhash)
  (define hexa-pubkey (pubkeyhashbase58-to-pubkeyhashhex pubkeyhash))
  (define pushdata-val (pushdataval hexa-pubkey))
  (string-append OP-DUP
                 OP-HASH160
                 pushdata-val
                 hexa-pubkey
                 OP-EQUALVERIFY
                 OP-CHECKSIG))

(define (pubkey-to-pubkeyscripthash
         pubkey) ; pubkey in hex format with pubkey prefix
  (define script (pubkey-to-P2PK pubkey))
  (define address
    (string-append bitcoin-addrprefix-scripthash (hash160 script)))
  (base58check-encode
   (string-append address (generate-checksum-base58 address))))

(define (private-to-pubkeyscripthash pk #:compressed [compressed #t])
  (define pubkey (private-to-pubkey pk #:version 0 #:compressed compressed))
  (pubkey-to-pubkeyscripthash pubkey))

(define (pubkey-to-nestedpubkeyhash pubkey) ; compressed key is mandatory
  (define hash160-val (hash160 pubkey))
  (define pushdata-val (pushdataval hash160-val))
  (define script (string-append OP-0 pushdata-val hash160-val))
  (define address
    (string-append bitcoin-addrprefix-scripthash (hash160 script)))
  (base58check-encode
   (string-append address (generate-checksum-base58 address))))

(define (private-to-nestedpubkeyhash pk #:compressed [compressed #t])
  (define pubkey (private-to-pubkey pk #:version 0 #:compressed compressed))
  (pubkey-to-nestedpubkeyhash pubkey))

(define (pubkey-to-bech32
         pubkey
         #:version [version 1]) ; compressed pubkey is mandatory
  (define val-to-encode (if (= version 0) (hash160 pubkey) pubkey))
  (bech32-encode val-to-encode #:version version))

(define (private-to-bech32 pk #:version [version 1])
  (define pub
    (private-to-pubkey pk #:version version)) ; compressed keys are mandatory
  (pubkey-to-bech32 pub #:version version))

(define (pubkey-to-bech32taproot
         pubkey) ; pub as a x-only public key in hex format
  (define Qx (tweak-pubkey pubkey "")) ; tweak pubkey with a "NULL" value
  ; convert to bech32
  (pubkey-to-bech32 Qx))

(define (private-to-bech32taproot pk)
  (define pub (private-to-pubkey pk))
  (pubkey-to-bech32taproot pub))

(provide generate-checksum-base58
         validate-checksum-base58
         generate-pk
         private-to-wif
         wif-to-private
         private-to-pubkey
         pubkey-to-pubkeyhash
         private-to-pubkeyhash
         pub-to-compressed
         pubhex-to-pubschnorr
         pubkey-to-compressed
         pubkey-to-P2PK
         pubkeyhashbase58-to-pubkeyhashhex
         pubkeyhash-to-P2PKH
         pubkey-to-pubkeyscripthash
         private-to-pubkeyscripthash
         pubkey-to-nestedpubkeyhash
         private-to-nestedpubkeyhash
         pubkey-to-bech32
         private-to-bech32
         pubkey-to-bech32taproot
         private-to-bech32taproot)
