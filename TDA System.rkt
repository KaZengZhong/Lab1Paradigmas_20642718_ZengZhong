#lang racket

(require racket/date)

; constructor
(define fecha (date->string (current-date)))
(define hora (date-hour (current-date)))
(define minuto (date-minute (current-date)))
(define fechaFinal (format "~a:~a ~a" hora minuto fecha))

(define (system name InitialChatbotCodeLink . chatbots)
  (list name
        InitialChatbotCodeLink
        chatbots
        '() 
        fechaFinal))


