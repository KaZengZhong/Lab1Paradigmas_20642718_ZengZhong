#lang racket

(require racket/date)

; chatbot
(define (chatbot chatbotID name welcomeMessage startFlowId . flows)
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

(define (flatten lst)
  (cond ((null? lst) '())
        ((not (pair? lst)) (list lst))
        (else (append (flatten (car lst)) (flatten (cdr lst))))))