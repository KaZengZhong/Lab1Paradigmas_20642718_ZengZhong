#lang racket

(provide (all-defined-out))

; Función: chatbot
; Descripción: Constructor para un chatbot, asegura flujos únicos.
; Estrategia: Verificación y eliminación de duplicados.
; Entradas: chatbotID, name, welcomeMessage, startFlowId, flows (Lista variable de flujos)
; Retorno: Lista estructurada de chatbot.
(define (chatbot chatbotID name welcomeMessage startFlowId . flows)
  (list chatbotID name welcomeMessage startFlowId (unique-flows flows)))


; Función: unique-flows
; Descripción: Elimina flujos duplicados basado en su id.
; Estrategia: Uso de 'remove-duplicates2' con una función de igualdad personalizada.
; Entradas: flows (Lista de flujos)
; Retorno: Lista de flujos sin duplicados.
(define (unique-flows flows)
  (remove-duplicates2 flows flow-id-equal?))


; Función: flow-id-equal?
; Descripción: Compara si dos flujos tienen el mismo id.
; Estrategia: Comparación directa usando 'eq?'.
; Entradas: flow1, flow2 (Flujos a comparar)
; Retorno: #t si son iguales, #f si no.
(define (flow-id-equal? flow1 flow2)
  (eq? (car flow1) (car flow2)))


; Función: remove-duplicates2
; Descripción: Elimina duplicados de una lista basándose en una función de igualdad.
; Estrategia: Acumulación mediante 'foldl'.
; Entradas: lst (Lista), equal? (Función de igualdad)
; Retorno: Lista sin elementos duplicados.
(define (remove-duplicates2 lst equal?)
  (foldl (lambda (item acc)
           (if (any (lambda (x) (equal? x item)) acc)
               acc
               (cons item acc)))
         '()
         lst))


; Función: any
; Descripción: Verifica si algún elemento de la lista satisface un predicado.
; Estrategia: Recursión por división.
; Entradas: pred (Función predicado), lst (Lista)
; Retorno: #t si algún elemento satisface, #f si no.
(define (any pred lst)
  (cond ((null? lst) #f)
        ((pred (car lst)) #t)
        (else (any pred (cdr lst)))))


; ---------------------------------------------------------------------------------------------------------

; Función: chatbot-add-flow
; Descripción: Añade un flujo al chatbot si es único.
; Estrategia: Verificación condicional.
; Entradas: chatbot (Estructura de chatbot), flow (Flujo a añadir)
; Retorno: Chatbot actualizado.
(define (chatbot-add-flow chatbot flow)
  (list (car chatbot)
        (cadr chatbot)
        (caddr chatbot)
        (cadddr chatbot)
        (add-if-unique flow (car (cddddr chatbot)))))


; Función: add-if-unique
; Descripción: Añade un flujo a una lista si es único.
; Estrategia: Verificación condicional.
; Entradas: acc (Lista acumulada de flujos), flow (Flujo a añadir)
; Retorno: Lista de flujos actualizada.
(define (add-if-unique flow acc)
  (if (not (flow-exists? flow acc))
      (cons flow acc)
      acc))


; Función: flow-exists?
; Descripción: Verifica si un flujo ya existe en una lista de flujos.
; Estrategia: Recursión por división.
; Entradas: flow (Flujo a verificar), flows (Lista de flujos)
; Retorno: #t si el flujo existe, #f si no.
(define (flow-exists? flow flows)
  (cond ((null? flows) #f)
        ((equal? (car flow) (car (car flows))) #t) 
        (else (flow-exists? flow (cdr flows)))))