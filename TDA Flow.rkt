;TDA Flow

#lang racket

(require racket/date)

(define (flow id name-msg . options)
  (if (unique-options? options)
      (list id name-msg options)
      (error "Las ID de opciones deben ser unicas")))

(define (unique-options? options)
  (let ((ids (map (lambda (option) (car option)) options)))
    (equal? (length ids) (length (remove-duplicates ids)))))

(define (remove-duplicates lst)
  (cond ((null? lst) '())
        ((member (car lst) (cdr lst)) (remove-duplicates (cdr lst)))
        (else (cons (car lst) (remove-duplicates (cdr lst))))))