#lang racket

(provide (all-defined-out))

; Función: flow
; Descripción: Crea una lista con un id, mensaje y opciones sin duplicados.
; Estrategia: Utiliza 'filter-duplicates' para evitar duplicados.
; Entradas: id, name-msg, options (Lista variable de opciones)
; Retorno: Lista con id, mensaje y opciones.
(define (flow id name-msg . options)
  (list id name-msg (filter-duplicates options '())))


; Función: filter-duplicates
; Descripción: Filtra opciones duplicadas basado en su id.
; Estrategia: Recursión por división.
; Entradas: options (Lista de opciones), seen-ids (Lista de ids ya vistos)
; Retorno: Lista de opciones sin duplicados.
(define (filter-duplicates options seen-ids)
  (if (null? options)
      '()
      (if (member (car (car options)) seen-ids)
          (filter-duplicates (cdr options) seen-ids)
          (cons (car options) (filter-duplicates (cdr options) (cons (car (car options)) seen-ids))))))


; Función: option-ids
; Descripción: Extrae los ids de una lista de opciones.
; Estrategia: Map para transformación.
; Entradas: options (Lista de opciones)
; Retorno: Lista de ids.
(define (option-ids options)
  (map car options))


; Función: remove-duplicates
; Descripción: Elimina elementos duplicados de una lista.
; Estrategia: Recursión por división.
; Entradas: lst (Lista)
; Retorno: Lista sin elementos duplicados.
(define (remove-duplicates lst)
  (cond ((null? lst) '())
        ((member (car lst) (cdr lst)) (remove-duplicates (cdr lst)))
        (else (cons (car lst) (remove-duplicates (cdr lst))))))


; ---------------------------------------------------------------------------------------------------------

; Función: flow-add-option
; Descripción: Agrega una opción al flujo, muestra error si el id ya existe.
; Estrategia: Verificación condicional.
; Entradas: flow (Lista con estructura de flujo), option (Lista con estructura de opción)
; Retorno: Flujo actualizado o el original en caso de error.
(define (flow-add-option flow option)
  (if (option-exists? (caddr flow) (car option))
      (begin
        (display "Error: La opcion de ID ya existe en el flujo.\n")
        flow)  
      (list (car flow) (cadr flow) (append (caddr flow) (list option))) 
  )
)

; Función: option-exists?
; Descripción: Verifica si una opción con cierto id ya existe.
; Estrategia: Uso de 'member' y 'map' para la búsqueda.
; Entradas: flow-options (Lista de opciones en el flujo), option-id (Identificador de la opción a verificar)
; Retorno: #t si existe, #f si no.
(define (option-exists? flow-options option-id)
  (member option-id (map car flow-options)))

