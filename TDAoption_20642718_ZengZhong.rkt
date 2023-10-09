#lang racket

(provide (all-defined-out))

; Función: option
; Descripción: Crea una lista que combina información de una opción.
; Estrategia: Utiliza 'append' para combinar listas.
; Entradas: code, message, ChatbotCodeLink, InitialFlowCodeLink, keywords (Lista variable en longitud)
; Retorno: Lista.
(define (option code message ChatbotCodeLink InitialFlowCodeLink . keywords)
  (append (list code message ChatbotCodeLink InitialFlowCodeLink) keywords))

