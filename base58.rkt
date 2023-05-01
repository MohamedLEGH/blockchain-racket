#lang racket
(require racket/list)

(define code_string "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
(define base (string-length code_string)) ; should give 58

; value is an int
(define (base58encode value)
    (define (ref->string str k)
        (string (string-ref str k)))
    (define (base58enc acc value)
        (if (= value 0)
            acc
            (base58enc (string-append (ref->string code_string (modulo value base)) acc) (quotient value base))))
    (base58enc "" value))

; value is a base58 encoded string
(define (base58decode value)
    (define code_list (string->list code_string))
    (define l (string->list value))
    (define reverse_l (reverse l))
    (define (base58dec acc power value)
        (if (equal? value '())
            acc
            (base58dec (+ (* (index-of code_list (car value)) (expt base power)) acc) (+ power 1) (cdr value))))
    (base58dec 0 0 reverse_l))

(provide base58encode base58decode)