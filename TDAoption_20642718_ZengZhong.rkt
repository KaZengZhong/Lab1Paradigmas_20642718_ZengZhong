#lang racket

;TDA option

(provide (all-defined-out))

(define (option code message ChatbotCodeLink InitialFlowCodeLink . keywords)
  (list code message ChatbotCodeLink InitialFlowCodeLink keywords)
)
