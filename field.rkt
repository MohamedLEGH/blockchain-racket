#lang racket
(require math/number-theory)

(struct field-element (value field) #:prefab)

(define (field_to_string field_val)
  (number->string (field-element-value field_val) 16))

(define (in_field? value field)
  (and (<= 0 value) (< value field)))

(define (add_element fe1 fe2)
  ; should assert same field for fe1 and fe2
  (field-element (with-modulus (field-element-field fe1)
                               (mod+ (field-element-value fe1)
                                     (field-element-value fe2)))
                 (field-element-field fe1)))

(define (sub_element fe1 fe2)
  ; should assert same field for fe1 and fe2
  (field-element (with-modulus (field-element-field fe1)
                               (mod- (field-element-value fe1)
                                     (field-element-value fe2)))
                 (field-element-field fe1)))

(define (rmul_element fe1 scalar) ; should use mul*
  (field-element (modulo (* (field-element-value fe1) scalar)
                         (field-element-field fe1))
                 (field-element-field fe1)))

(define (mul_element fe1 fe2)
  ; should assert same field for fe1 and fe2
  (field-element (with-modulus (field-element-field fe1)
                               (mod* (field-element-value fe1)
                                     (field-element-value fe2)))
                 (field-element-field fe1)))

(define (pow_element fe1 exponent)
  (field-element (with-modulus (field-element-field fe1)
                               (modexpt (field-element-value fe1) exponent))
                 (field-element-field fe1)))

(define (truediv_element fe1 fe2)
  ; should assert same field for fe1 and fe2
  (field-element
   (with-modulus (field-element-field fe1)
                 (mod/ (field-element-value fe1) (field-element-value fe2)))
   (field-element-field fe1)))

(provide (struct-out field-element)
         in_field?
         field_to_string
         add_element
         sub_element
         rmul_element
         mul_element
         pow_element
         truediv_element)
