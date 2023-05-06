#lang racket
(require racket/list)

; bech32 constants
(define bech32_bitcoin_prefix "bc")
(define bech32_testnet_prefix "tb")
(define bech32_separator "1")
(define bech32chars "qpzry9x8gf2tvdw0s3jn54khce6mua7l")
(define BECH32_CONST 1)
(define BECH32M_CONST #x2bc830a3)

(define (pad_str str_val nb)
  (~a str_val #:min-width nb #:pad-string "0"))

(define (pad_int_binarystr int_val pad_val)
  (~r int_val #:base 2 #:min-width pad_val #:pad-string "0"))

(define (pad_int_hexstr int_val pad_val)
  (~r int_val #:base 16 #:min-width pad_val #:pad-string "0"))

; used in bech32 format, take a str with binary data, return list of nb bytes str
(define (splitnbpart str nb)
  (define (splitnb lst acc)
    (cond
      [(= (length lst) 0) acc]
      [(< (length lst) nb) (cons lst acc)]
      [else (splitnb (list-tail lst nb) (cons (take lst nb) acc))]))
  (define list_splitnb (splitnb (string->list str) '()))
  (define list_string (map list->string list_splitnb))
  (reverse list_string))

; used in bech32 format
(define (hexbytes_to_hex5bit hexstring)
  (define char_list (string->list hexstring))
  (define str_list (map string char_list))
  (define nb_list (map (lambda (nb) (string->number nb 16)) str_list))
  (define binary_list (map (lambda (val) (pad_int_binarystr val 4)) nb_list))
  (define concac_list (string-append* binary_list))
  (define splitlist (splitnbpart concac_list 5))
  (define list_str (list-update splitlist (- (length splitlist) 1) pad_str))
  (define binarylistnew (map (lambda (nb) (string->number nb 2)) list_str))
  binarylistnew)

(define (expandhrp hrp)
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
  (string-append bech32_bitcoin_prefix bech32_separator string-list))

(define (bech32_recompute_checksum hrp bech32_intlist)
  (define const (bech32_polymod (append (expandhrp hrp) bech32_intlist)))
  const)

(define (bech32_verify_checksum hrp bech32_intlist)
  (define const (bech32_recompute_checksum hrp bech32_intlist))
  (cond
    [(or (equal? const BECH32_CONST) (equal? const BECH32M_CONST)) #t]
    [else #f]))

(define (bech32_decode bech32_str)
  ; TODO : check if the data part of the bech32 string only contains CHARSET element
  ;Decoders MUST NOT accept strings where some characters are uppercase and some are lowercase (such strings are referred to as mixed case strings).
  (when (and (not (equal? bech32_str (string-downcase bech32_str)))
             (not (equal? bech32_str (string-upcase bech32_str))))
    (error "mixed case strings are not allowed"))
  (define bech32_string (string-downcase bech32_str))
  (define hrp_compute (substring bech32_string 0 2))
  ; MUST verify that the human-readable part is "bc" for mainnet and "tb" for testnet.
  (when (and (not (equal? hrp_compute bech32_bitcoin_prefix))
             (not (equal? hrp_compute bech32_testnet_prefix)))
    (error "not a valid hrp"))
  (define separator_compute (substring bech32_string 2 3))
  (when (not (equal? separator_compute bech32_separator))
    (error "not a valid separator"))
  (define code_list (string->list bech32chars))
  (define bech32_string_vals (substring bech32_string 3))
  (define bech32_charlist (string->list bech32_string_vals))
  (define bech32_intlist
    (map (lambda (char) (index-of code_list char)) bech32_charlist))
  (when (not (bech32_verify_checksum hrp_compute bech32_intlist))
    (error "checksum is not valid"))
  (define bech32const_val
    (bech32_recompute_checksum hrp_compute bech32_intlist))
  (define first_val (car bech32_intlist))
  ; MUST verify that the first decoded data value (the witness version) is between 0 and 16, inclusive.
  (when (or (> first_val 16) (< first_val 0))
    (error "first value cannot be superior to 16 or inferior to 0"))
  (when (or (and (= first_val 0) (not (= bech32const_val BECH32_CONST)))
            (and (not (= first_val 0)) (not (= bech32const_val BECH32M_CONST))))
    (error "first value do not match the checksum"))
  ; now we have our (5bit encoded) list as int
  ; convert each int in base 2
  (define bech32_intlist_no_first_val (cdr bech32_intlist))
  (define bech32_intlist_nochecksum (drop-right bech32_intlist_no_first_val 6))
  (define bech32_binarylist
    (map (lambda (nb) (pad_int_binarystr nb 5)) bech32_intlist_nochecksum))
  (define concac_list (string-append* bech32_binarylist))
  (define splitlist (splitnbpart concac_list 8))
  ; if incomplete group at the end, it must be of 4bit or less, must be all zeros, should be discarded
  (define splitlist_clean
    (cond
      [(= (string-length (last splitlist)) 8) splitlist]
      [(and (<= (string-length (last splitlist)) 4)
            (equal? (last splitlist)
                    (make-string (string-length (last splitlist)) #\0)))
       (drop-right splitlist 1)]
      [else
       (error
        "not a valid bech32 string: imcomplete group is more than 5bit")]))
  ; convert the (8 bit encoded str) into the int value
  (define intlistnew (map (lambda (nb) (string->number nb 2)) splitlist_clean))
  ;There MUST be between 2 and 40 groups, which are interpreted as the bytes of the witness program.
  (when (or (< (length intlistnew) 2) (> (length intlistnew) 40))
    (error "not a valid bech32: number of groups invalid [2:40]"))
  ; convert the int into a hex str
  ; each hexstr should be of size 2 char
  (define hexlist (map (lambda (nb) (pad_int_hexstr nb 2)) intlistnew))
  ; concat the hex
  (define concac_hex (string-append* hexlist))
  concac_hex)

(provide bech32_encode
         bech32_decode
         bech32_verify_checksum)
