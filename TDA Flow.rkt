;TDA Flow

#lang racket

(require racket/date)

; flow
(define (flow id name-msg . options)
  (if (unique-options? options)
      (list id name-msg options)
      (error "Los opciones de ID deben ser unicas")))

(define (unique-options? options)
  (equal? (length (option-ids options)) 
          (length (remove-duplicates (option-ids options)))))

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