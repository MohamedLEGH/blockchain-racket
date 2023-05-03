#lang racket
(require racket/string)
(require "crypto.rkt")
(require "base58.rkt")

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
(define bitcoin_addrprefix_scripthash "05"); Version byte is 5 for a main-network address, 196 for a testnet address
(define bech32_bitcoin_prefix "bc") ; "tb" for testnet

;; opcodes
(define OP_DUP "76")
(define OP_HASH160 "a9")
(define OP_EQUALVERIFY "88")
(define OP_CHECKSIG "ac")
(define OP_CHECKMULTISIG "ae")
(define OP_1 "81")

; bech32
(define bech32chars "qpzry9x8gf2tvdw0s3jn54khce6mua7l")

(define (pushdataval val) ; val in hexa
    (number->string (/ (string-length val) 2) 16))

(define (generate_pk)
    (number->string (generate_random) 16))

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

; used in bech32 format
(define (split5part str) ; maybe I can do it in pure forme with fold
    (define str_l (string-length str))
    (define nb_string (quotient str_l 5))
    (define rest_string (remainder str_l 5))
    (define list_str '())
    (for ([i nb_string])
        (set! list_str (cons (substring str (* i 5) (* (+ i 1) 5)) list_str)))
    (when (> rest_string 0) (set! list_str (cons (substring str (* nb_string 5)) list_str)))
    (reverse list_str))

; used in bech32 format
(define (hexbytes_to_hex5bit hexstring)
    (define char_list (string->list hexstring))
    (define str_list (map string char_list))
    (define nb_list (map (lambda (nb) (string->number nb 16)) str_list))
    (define binary_list (map (lambda (val) (~r val #:base 2 #:min-width 4 #:pad-string "0")) nb_list))
    (define concac_list (string-append* binary_list))
    (define splitlist (split5part concac_list))
    (define binarylistnew (map (lambda (nb) (string->number nb 2)) splitlist))
    binarylistnew)

(define (expandhrp) ; should allow to choose the hrp (for now only "bc")
    (define charlist (string->list bech32_bitcoin_prefix))
    (define char_val (map char->integer charlist))
    (define left_val (map (lambda (nb) (arithmetic-shift nb -5)) char_val))
    (define right_val (map (lambda (nb) (bitwise-and nb 31)) char_val))
    (append left_val '(0) right_val))

(define (bech32_polymod values) ; values in list of int
    (define GEN '(#x3b6a57b2 #x26508e6d #x1ea119fa #x3d4233dd #x2a1462b3))
    (define chk 1)
    (for-each (lambda (nb) 
                (define b (arithmetic-shift chk -25))
                (set! chk (bitwise-xor (arithmetic-shift (bitwise-and chk #x1ffffff) 5) nb))
                (for ([i 5]) (set! chk (bitwise-xor chk (if (= (bitwise-and (arithmetic-shift b (- i)) 1) 1) (list-ref GEN i) 0))))
    ) values)
    chk)

(define (generate_checksum_bech32 val)
    (define values (append (expandhrp) val))
    (define polymod (bitwise-xor (bech32_polymod (append values '(0 0 0 0 0 0))) 1))
    (define checksum '(0 1 2 3 4 5))
    (map (lambda (i) (bitwise-and (arithmetic-shift polymod (- (* 5 (- 5 i)))) 31)) checksum))

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

(define (pubkey_to_P2PK pubkey)
    (define pushdata_val (pushdataval pubkey))
    (string-append pushdata_val pubkey OP_CHECKSIG))

(define (pubkey_to_P2MS1of1 pubkey)
    (define pushdata_val (pushdataval pubkey))
    (string-append OP_1 pushdata_val pubkey OP_1 OP_CHECKMULTISIG))

(define (pubkey_to_pubkeyhash pubkey)
    (define pubkeyhash (string-append bitcoin_addrprefix (hash160 pubkey)))
    (define checksum (generate_checksum pubkeyhash))
    (define pubkeyhash_with_checksum (string-append pubkeyhash checksum))
    (base58check_encode pubkeyhash_with_checksum))

(define (private_to_pubkeyhash pk)
    (define pubkey (private_to_pubkey pk))
    (pubkey_to_pubkeyhash pubkey))

(define (pubkeyhashbase58_to_pubkeyhashhex pubkeyhash) ; from base58 format to hex format of hash160(pubkey)
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
    (string-append OP_DUP OP_HASH160 pushdata_val hexa_pubkey OP_EQUALVERIFY OP_CHECKSIG))

(define (pubkey_to_pubkeyscripthash pubkey) ; pubkey in hex format with pubkey prefix
    (define script (pubkey_to_P2PK pubkey))
    (define address (string-append bitcoin_addrprefix_scripthash (hash160 script)))
    (base58check_encode (string-append address (generate_checksum address))))

(define (pubkey_to_scrithash1of1musig pubkey) ; pubkey in hex format with pubkey prefix
    (define script (pubkey_to_P2MS1of1 pubkey))
    (define address (string-append bitcoin_addrprefix_scripthash (hash160 script)))
    (base58check_encode (string-append address (generate_checksum address))))

(define (pub_to_bech32 pub) ; compressed pubkey is mandatory
    (define hash160_val (hash160 pub))
    (define hex5bit (hexbytes_to_hex5bit hash160_val))
    (define val_list (cons 0 hex5bit))
    (define checksum (generate_checksum_bech32 val_list))
    (define list_and_checksum (append val_list checksum))
    (define char_list (map (lambda (nb) (string-ref bech32chars nb)) list_and_checksum))
    (define string-list (list->string char_list))
    (string-append bech32_bitcoin_prefix "1" string-list))

(provide base58check_encode
         base58check_decode
         generate_checksum
         bech32chars
         expandhrp
         bech32_polymod
         generate_checksum_bech32
         generate_pk
         private_to_wif
         wif_to_private
         private_to_pubkey
         pubkey_to_pubkeyhash
         private_to_pubkeyhash
         pub_to_compressed
         pubkey_to_compressed
         pubkey_to_P2PK
         pubkey_to_P2MS1of1
         pubkeyhashbase58_to_pubkeyhashhex
         pubkeyhash_to_P2PKH
         pubkey_to_pubkeyscripthash
         pubkey_to_scrithash1of1musig
         split5part
         hexbytes_to_hex5bit
         pub_to_bech32)

