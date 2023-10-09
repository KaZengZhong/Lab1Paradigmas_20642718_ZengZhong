#lang racket

;TDA option

(provide (all-defined-out))

(define (option code message ChatbotCodeLink InitialFlowCodeLink . keywords)
  (append (list code message ChatbotCodeLink InitialFlowCodeLink) keywords))
