#lang racket

; bech32 constants
(define bech32_bitcoin_prefix "bc") ; "tb" for testnet
(define bech32chars "qpzry9x8gf2tvdw0s3jn54khce6mua7l")
(define BECH32_CONST 1)
(define BECH32M_CONST #x2bc830a3)

; used in bech32 format, take a str with binary data, return list of 5bytes str
(define (split5part str) ; maybe I can do it in pure forme with fold
  (define str_l (string-length str))
  (define nb_string (quotient str_l 5))
  (define rest_string (remainder str_l 5))
  (define list_str '())
  (for ([i nb_string])
    (set! list_str (cons (substring str (* i 5) (* (+ i 1) 5)) list_str)))
  (when (> rest_string 0)
    (set!
     list_str
     (cons (~a (substring str (* nb_string 5)) #:min-width 5 #:pad-string "0")
           list_str)))
  (reverse list_str))

; used in bech32 format
(define (hexbytes_to_hex5bit hexstring)
  (define char_list (string->list hexstring))
  (define str_list (map string char_list))
  (define nb_list (map (lambda (nb) (string->number nb 16)) str_list))
  (define binary_list
    (map (lambda (val) (~r val #:base 2 #:min-width 4 #:pad-string "0"))
         nb_list))
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
  (for-each
   (lambda (nb)
     (define b (arithmetic-shift chk -25))
     (set! chk
           (bitwise-xor (arithmetic-shift (bitwise-and chk #x1ffffff) 5) nb))
     (for ([i 5])
       (set! chk
             (bitwise-xor chk
                          (if (= (bitwise-and (arithmetic-shift b (- i)) 1) 1)
                              (list-ref GEN i)
                              0)))))
   values)
  chk)

(define (generate_checksum_bech32 val #:version [version 1])
  (define values (append (expandhrp) val))
  (define const_val (if (= version 0) BECH32_CONST BECH32M_CONST))
  (define polymod
    (bitwise-xor (bech32_polymod (append values '(0 0 0 0 0 0))) const_val))
  (define checksum '(0 1 2 3 4 5))
  (map
   (lambda (i) (bitwise-and (arithmetic-shift polymod (- (* 5 (- 5 i)))) 31))
   checksum))

(define (bech32_encode hashval #:version [version 1]) ; take a hashvalue
  (define hex5bit (hexbytes_to_hex5bit hashval))
  (define val_list (cons version hex5bit)) ; witness version
  (define checksum (generate_checksum_bech32 val_list #:version version))
  (define list_and_checksum (append val_list checksum))
  (define char_list
    (map (lambda (nb) (string-ref bech32chars nb)) list_and_checksum))
  (define string-list (list->string char_list))
  (string-append bech32_bitcoin_prefix "1" string-list))

(provide bech32_encode)
