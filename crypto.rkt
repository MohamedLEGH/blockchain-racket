#lang racket
(require math/number-theory)

(struct galois-field (prime) #:prefab)

(define (in_field gf value)
  (and (<= 0 value) (< value (galois-field-prime gf))))

(struct field-element (value field) #:prefab)

(define (add_element fe1 fe2)
  (field-element (with-modulus (galois-field-prime (field-element-field fe1)) (mod+ (field-element-value fe1) (field-element-value fe2))) (field-element-field fe1) ))

(define (sub_element fe1 fe2)
  (field-element (with-modulus (galois-field-prime (field-element-field fe1)) (mod- (field-element-value fe1) (field-element-value fe2))) (field-element-field fe1) ))

(define (rmul_element fe1 scalar)
  (field-element (modulo (* (field-element-value fe1) scalar) (galois-field-prime (field-element-field fe1))) (field-element-field fe1)))

(define (mul_element fe1 fe2)
  (field-element (with-modulus (galois-field-prime (field-element-field fe1)) (mod* (field-element-value fe1) (field-element-value fe2))) (field-element-field fe1) ))

(define (pow_element fe1 exponent)
  (field-element (with-modulus (galois-field-prime (field-element-field fe1)) (modexpt (field-element-value fe1) exponent)) (field-element-field fe1) ))

(define (truediv_element fe1 fe2)
  (field-element (with-modulus (galois-field-prime (field-element-field fe1)) (mod/ (field-element-value fe1) (field-element-value fe2))) (field-element-field fe1) ))

(provide (struct-out galois-field)
         (struct-out field-element)
         in_field add_element sub_element rmul_element mul_element
         pow_element truediv_element)
