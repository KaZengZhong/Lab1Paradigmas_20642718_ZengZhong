#lang racket

(provide (all-defined-out))

; flow
(define (flow id name-msg . options)
  (list id name-msg (filter-duplicates options '())))

(define (filter-duplicates options seen-ids)
  (if (null? options)
      '()
      (if (member (car (car options)) seen-ids)  ; Se usa car para obtener el id de la opción
          (filter-duplicates (cdr options) seen-ids)
          (cons (car options) (filter-duplicates (cdr options) (cons (car (car options)) seen-ids))))))

(define (option-ids options)
  (map car options))

(define (remove-duplicates lst)
  (cond ((null? lst) '())
        ((member (car lst) (cdr lst)) (remove-duplicates (cdr lst)))
        (else (cons (car lst) (remove-duplicates (cdr lst))))))

; flow-add-option
(define (flow-add-option flow option)
  (if (option-exists? (caddr flow) (car option))
      (begin
        (display "Error: La opcion de ID ya existe en el flujo.\n")
        flow)  
      (list (car flow) (cadr flow) (append (caddr flow) (list option))) 
  )
)

(define (option-exists? flow-options option-id)
  (member option-id (map car flow-options)))