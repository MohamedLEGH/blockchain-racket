#lang racket

; bech32 constants
(define bech32_bitcoin_prefix "bc") ; "tb" for testnet
(define bech32chars "qpzry9x8gf2tvdw0s3jn54khce6mua7l")
(define BECH32_CONST 1)
(define BECH32M_CONST #x2bc830a3)

; used in bech32 format, take a str with binary data, return list of 5bytes str
(define (split5part str)
  (define (split5 lst acc)
    (cond
      [(= (length lst) 0) acc]
      [(< (length lst) 5) (cons lst acc)]
      [else (split5 (list-tail lst 5) (cons (take lst 5) acc))]))
  (define list_split5 (split5 (string->list str) '()))
  (define list_string (map list->string list_split5))
  (define (pad_str str_val)
    (~a str_val #:min-width 5 #:pad-string "0"))
  (define list_str (list-update list_string 0 pad_str))
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

(define (expandhrp hrp) ; should allow to choose the hrp (for now only "bc")
  (define charlist (string->list hrp))
  (define char_val (map char->integer charlist))
  (define left_val (map (lambda (nb) (arithmetic-shift nb -5)) char_val))
  (define right_val (map (lambda (nb) (bitwise-and nb 31)) char_val))
  (append left_val '(0) right_val))

(define (bech32_polymod values) ; values is a list of int
  (define GEN '(#x3b6a57b2 #x26508e6d #x1ea119fa #x3d4233dd #x2a1462b3))
  (define chk 1)
  (define (chk_xor_gen chk_v gen_val inc b_val)
    (bitwise-xor
     chk_v
     (if (= (bitwise-and (arithmetic-shift b_val (- inc)) 1) 1) gen_val 0)))
  (define (update_chk_value chk_val value)
    (define b_val (arithmetic-shift chk_val -25))
    (define (update_chk_with_GEN gen acc inc)
      (if (equal? gen '())
          acc
          (update_chk_with_GEN (cdr gen)
                               (chk_xor_gen acc (car gen) inc b_val)
                               (+ 1 inc))))
    (update_chk_with_GEN
     GEN
     (bitwise-xor (arithmetic-shift (bitwise-and chk_val #x1ffffff) 5) value)
     0))
  (define (polymod_compute values_list acc)
    (if (equal? values_list '())
        acc
        (polymod_compute (cdr values_list)
                         (update_chk_value acc (car values_list)))))
  (polymod_compute values chk))

(define (generate_checksum_bech32 val
                                  #:version [version 1]
                                  #:hrp [hrp bech32_bitcoin_prefix])
  (define values (append (expandhrp hrp) val))
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
