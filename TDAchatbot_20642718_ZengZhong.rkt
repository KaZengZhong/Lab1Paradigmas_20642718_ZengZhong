#lang racket

(provide (all-defined-out))

; chatbot - constructor
#|(define (chatbot chatbotID name welcomeMessage startFlowId . flows)
  (list chatbotID name welcomeMessage startFlowId (unique-flows flows)))

(define (unique-flows flows)
  (foldl add-if-unique '() (flatten flows)))

(define (add-if-unique acc flow)
  (if (not (flow-exists? flow acc))
      (cons flow acc)
      acc))

(define (flow-exists? flow flows)
  (cond ((null? flows) #f)
        ((equal? (car flow) (car (car flows))) #t)
        (else (flow-exists? flow (cdr flows)))))
|#
(define (chatbot chatbotID name welcomeMessage startFlowId . flows)
  (list chatbotID name welcomeMessage startFlowId (unique-flows flows)))

(define (unique-flows flows)
  (foldl add-if-unique '() flows))

(define (add-if-unique acc flow)
  (if (not (flow-exists? flow acc))
      (cons flow acc)
      acc))

(define (flow-exists? flow flows)
  (cond ((null? flows) #f)
        ((null? (car flows)) #f) ; Aquí nos aseguramos de que el car de flows no sea nulo
        ((equal? (car flow) (car (car flows))) #t)
        (else (flow-exists? flow (cdr flows)))))


; chatbot - modificador

(define (chatbot-add-flow chatbot flow)
  (list (car chatbot)
        (cadr chatbot)
        (caddr chatbot)
        (cadddr chatbot)
        (append (car (cddddr chatbot)) (list (add-if-unique flow (car (cddddr chatbot)))))))
