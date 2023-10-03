#lang racket

(provide (all-defined-out))

; chatbot - constuctor
(define (chatbot chatbotID name welcomeMessage startFlowId . flows)
  (list chatbotID name welcomeMessage startFlowId (unique-flows flows)))

(define (unique-flows flows)
  (remove-duplicates2 flows flow-id-equal?))

(define (flow-id-equal? flow1 flow2)
  (eq? (car flow1) (car flow2)))

(define (remove-duplicates2 lst equal?)
  (foldl (lambda (item acc)
           (if (any (lambda (x) (equal? x item)) acc)
               acc
               (cons item acc)))
         '()
         lst))

(define (any pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (any pred (cdr lst)))))




; chatbot - modificador

(define (chatbot-add-flow chatbot flow)
  (list (car chatbot)
        (cadr chatbot)
        (caddr chatbot)
        (cadddr chatbot)
        (append (car (cddddr chatbot)) (list (add-if-unique flow (car (cddddr chatbot)))))))

(define (add-if-unique acc flow)
  (if (not (flow-exists? flow acc))
      (cons flow acc)
      acc))

(define (flow-exists? flow flows)
  (cond ((null? flows) #f)
        ((equal? (car flow) (car (car flows))) #t)
        (else (flow-exists? flow (cdr flows)))))