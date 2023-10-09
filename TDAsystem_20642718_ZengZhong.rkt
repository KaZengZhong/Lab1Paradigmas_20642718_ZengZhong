#lang racket

(require racket/date)
(provide (all-defined-out))

#|
(define dia (date->string (current-date)))
(define hora (date-hour (current-date)))
(define minuto (date-minute (current-date)))
(define fecha (format "~a:~a ~a" hora minuto dia))
|#

; Función: system
; Descripción: Constructor para un sistema con chatbots.
; Entradas: name (Nombre del sistema), InitialChatbotCodeLink (Link inicial), chatbots (Lista variable de chatbots)
; Retorno: Lista estructurada de sistema.
(define (system name InitialChatbotCodeLink . chatbots)
  (list name
        InitialChatbotCodeLink
        chatbots
        '() 
        '()))


; -------------------------------------------------------------------------------------------------------------------

; Función: chatbot-exists?
; Descripción: Verifica si un chatbot ya existe en el sistema.
; Estrategia: Uso de 'ormap' para comprobar existencia.
; Entradas: system (Estructura del sistema), chatbot (Chatbot a verificar)
; Retorno: #t si el chatbot existe, #f si no.
(define (chatbot-exists? system chatbot)
  (ormap (lambda (existing-chatbot)
           (= (car existing-chatbot) (car chatbot)))
         (caddr system)))


; Función: system-add-chatbot
; Descripción: Añade un chatbot al sistema si no existe.
; Estrategia: Verificación condicional.
; Entradas: system (Estructura del sistema), chatbot (Chatbot a añadir)
; Retorno: Sistema actualizado o el sistema original si el chatbot ya existe.
(define (system-add-chatbot system chatbot)
  (cond
    ((chatbot-exists? system chatbot)
     (display "El chatbot ya existe.\n")
     system)
    (else
     (list (car system)                  
           (cadr system)                  
           (append (caddr system) (list chatbot)) 
           (cadddr system)))))            


; -------------------------------------------------------------------------------------------------------------------

; Función: user-exists?
; Descripción: Verifica si un usuario ya existe en el sistema.
; Estrategia: Uso de 'ormap' para comprobar existencia.
; Entradas: system (Estructura del sistema), user (Usuario a verificar)
; Retorno: #t si el usuario existe, #f si no.
(define (user-exists? system user)
  (ormap (lambda (existing-user) (string=? existing-user user))
         (cadddr system))) 


; Función: system-add-user
; Descripción: Añade un usuario al sistema si no existe.
; Estrategia: Verificación condicional.
; Entradas: system (Estructura del sistema), user (Usuario a añadir)
; Retorno: Sistema actualizado o el sistema original si el usuario ya existe.
(define (system-add-user system user)
  (if (user-exists? system user)
      (begin
        (display "El usuario ya existe.\n")
        system)  
      (list (car system)       
            (cadr system)      
            (caddr system)    
            (append (cadddr system) (list user))
            (list-ref system 4) 
            (if (>= (length system) 6) (list-ref system 5) #f))))


; -------------------------------------------------------------------------------------------------------------------

; Función: system-login
; Descripción: Inicia sesión para un usuario en el sistema.
; Estrategia: Verificación condicional.
; Entradas: system (Estructura del sistema), user (Usuario que intenta iniciar sesión)
; Retorno: Sistema actualizado con el usuario logueado o el sistema original si el inicio de sesión no es exitoso.
(define (system-login system user)
  (cond
    ((or (not (user-exists? system user))
         (and (>= (length system) 6) (list-ref system 5)))
     (display "No se pudo iniciar sesion.\n")
     system)
    (else
     (list (car system)      
           (cadr system)    
           (caddr system)   
           (cadddr system)  
           (list-ref system 4) 
           user)))) 


; -------------------------------------------------------------------------------------------------------------------

; Función: system-logout
; Descripción: Cierra la sesión del usuario en el sistema.
; Estrategia: Reconstrucción de la estructura del sistema.
; Entradas: system (Estructura del sistema con un usuario logueado)
; Retorno: Sistema actualizado con el campo de usuario que ha iniciado sesión reseteado a #f.
(define (system-logout system)
  (list (car system)     
        (cadr system)    
        (caddr system)   
        (cadddr system)  
        (list-ref system 4)  
        #f)) 


; -------------------------------------------------------------------------------------------------------------------

; Función: find-chatbot-by-code
; Descripción: Busca y devuelve un chatbot en la lista de chatbots según un código especificado.
; Estrategia: Recursión lineal
; Entradas: chatbots (Lista de chatbots), code (Código del chatbot a buscar)
; Retorno: Chatbot si se encuentra, o #f si no.
(define (find-chatbot-by-code chatbots code)
  (if (null? chatbots)
      #f
      (if (= (first (first chatbots)) code)
          (first chatbots)
          (find-chatbot-by-code (rest chatbots) code))))


; Función: replace-chatbot
; Descripción: Reemplaza la información de un chatbot en la lista de chatbots.
; Estrategia: Recursión lineal
; Entradas: chatbots (Lista de chatbots), chatbot (Chatbot a reemplazar), new-start-flow-id (Nuevo ID de flujo inicial)
; Retorno: Lista de chatbots con el chatbot actualizado.
(define (replace-chatbot chatbots chatbot new-start-flow-id)
  (if (null? chatbots)
      '()
      (if (= (first (first chatbots)) (first chatbot))
          (cons (list (first chatbot) (second chatbot) (third chatbot) new-start-flow-id (fifth chatbot)) 
                (rest chatbots))
          (cons (first chatbots) 
                (replace-chatbot (rest chatbots) chatbot new-start-flow-id)))))


; Función: find-flow-by-id
; Descripción: Busca y devuelve un flujo por su ID.
; Estrategia: Recursión lineal
; Entradas: flows (Lista de flujos), id (ID del flujo a buscar)
; Retorno: Flujo si se encuentra, o #f si no.
(define (find-flow-by-id flows id)
  (if (null? flows)
      #f
      (if (= (first (first flows)) id)
          (first flows)
          (find-flow-by-id (rest flows) id))))

; Función: string-numeric?
; Descripción: Verifica si una cadena puede ser convertida a número.
; Estrategia: Uso de funciones predefinidas
; Entrada: str (Cadena a verificar)
; Retorno: #t si es numérico, #f en caso contrario.
(define (string-numeric? str)
  (let ((num-str (string->number str)))
    (and num-str (number? num-str))))


; Función: get-response
; Descripción: Obtiene una respuesta del chatbot basada en un mensaje del usuario.
; Estrategia: Procesamiento de listas y búsqueda
; Entradas: chatbot (Información del chatbot), message (Mensaje del usuario)
; Retorno: Una lista que contiene la respuesta, el ID del siguiente flujo y el ID del chatbot.
(define (get-response chatbot message)
  (let* ((start-flow-id (fourth chatbot))
         (flows (fifth chatbot))
         (welcome-message (third chatbot))
         (flow (find-flow-by-id flows start-flow-id)))
    (if (not flow)
        (list "No se encontró el flujo." start-flow-id 1)
        (let* ((response-and-ids (find-option-message-and-ids-by-keyword flow message))
               (response (first response-and-ids))
               (chatbot-id (second response-and-ids))
               (flow-id (third response-and-ids)))
          (if (string=? response "Opción no encontrada.")
              (list welcome-message start-flow-id 0)
              (list response flow-id chatbot-id))))))


; Función: find-option-message-and-ids-by-keyword
; Descripción: Busca un mensaje y sus ID asociados en un flujo según una palabra clave.
; Estrategia: Recursión y procesamiento de listas
; Entradas: flow (Flujo actual), input (Entrada o mensaje del usuario)
; Retorno: Una lista que contiene el mensaje, el ID del chatbot y el ID del flujo.
(define (find-option-message-and-ids-by-keyword flow input)
  (let ((options (third flow)))
    (cond 
      ((null? options) (list "Opción no encontrada." 1 0))
      (else
       (let* ((current-option (first options))
              (current-option-id (first current-option))
              (current-option-keywords (list-tail current-option 4)))
         (cond 
             ((member input current-option-keywords) (list (second current-option) 
                                                           (third current-option)
                                                           (fourth current-option)))
             ((and (string-numeric? input) (= (string->number input) current-option-id)) (list (second current-option) 
                                                                                              (third current-option)
                                                                                              (fourth current-option)))
             (else (find-option-message-and-ids-by-keyword (list (first flow) (second flow) (rest options)) input))))))))


; Función: update-system-with-response
; Descripción: Actualiza el sistema con una nueva respuesta del chatbot.
; Estrategia: Procesamiento de listas y reemplazo
; Entradas: system (Estado actual del sistema), user-message (Mensaje del usuario), system-response (Respuesta del sistema)
; next-flow-id (ID del siguiente flujo), next-chatbot-id (ID del siguiente chatbot)
; Retorno: Sistema actualizado.
(define (update-system-with-response system user-message system-response next-flow-id next-chatbot-id)
  (let* ((chatbot-code next-chatbot-id) ; Asigna directamente el next-chatbot-id a chatbot-code
         (chatbots (third system))
         (chatbot (find-chatbot-by-code chatbots chatbot-code)))
    (list (first system)
          chatbot-code
          (replace-chatbot chatbots chatbot next-flow-id)
          (fourth system)
          (append (fifth system) (list (list user-message system-response)))
          (sixth system))))


; Función: system-talk-rec
; Descripción: Procesa un mensaje del usuario y actualiza el sistema con la respuesta del chatbot.
; Estrategia: Procesamiento de listas y actualización de estado
; Entradas: system (Estado actual del sistema), message (Mensaje del usuario)
; Retorno: Sistema actualizado.
(define (system-talk-rec system message)
  (let* ((user-code (first system))
         (chatbot-code (second system))
         (chatbots (third system))
         (chatbot (find-chatbot-by-code chatbots chatbot-code))
         (response-and-next-ids (get-response chatbot message))
         (response (first response-and-next-ids))
         (next-flow (second response-and-next-ids))
         (next-chatbot-id (third response-and-next-ids))
         (updated-system (update-system-with-response system message response next-flow next-chatbot-id)))
    updated-system))


; -------------------------------------------------------------------------------------------------------------------

; Función: find-chatbot-by-code2
; Descripción: Busca y devuelve un chatbot en la lista de chatbots según un código especificado.
; Estrategia: Bucle iterativo local
(define (find-chatbot-by-code2 chatbots code)
  (let loop ((chatbots chatbots))
    (if (null? chatbots)
        #f
        (if (= (first (first chatbots)) code)
            (first chatbots)
            (loop (rest chatbots))))))


; Función: replace-chatbot2
; Descripción: Reemplaza la información de un chatbot en la lista de chatbots.
; Estrategia: Bucle iterativo local
(define (replace-chatbot2 chatbots chatbot new-start-flow-id)
  (let loop ((chatbots chatbots) (new-list '()))
    (if (null? chatbots)
        (reverse new-list)
        (let ((current-chatbot (first chatbots)))
          (if (= (first current-chatbot) (first chatbot))
              (loop (rest chatbots) 
                    (cons (list (first chatbot) 
                                (second chatbot) 
                                (third chatbot) 
                                new-start-flow-id 
                                (fifth chatbot)) 
                          new-list))
              (loop (rest chatbots) 
                    (cons current-chatbot new-list)))))))


; Función: find-flow-by-id2
; Descripción: Busca y devuelve un flujo por su ID.
; Estrategia: Bucle iterativo local
(define (find-flow-by-id2 flows id)
  (let loop ((flows flows))
    (if (null? flows)
        #f
        (if (= (first (first flows)) id)
            (first flows)
            (loop (rest flows))))))


; Función: find-option-message-and-ids-by-keyword2
; Descripción: Busca un mensaje y sus ID asociados en un flujo según una palabra clave.
; Estrategia: Bucle iterativo local
(define (find-option-message-and-ids-by-keyword2 flow input)
  (let loop ((options (third flow)))
    (if (null? options)
        (list "Opción no encontrada." 1 0)
        (let* ((current-option (first options))
               (current-option-id (first current-option))
               (current-option-keywords (list-tail current-option 4)))
          (if (or (member input current-option-keywords) 
                  (and (string-numeric? input) 
                       (= (string->number input) current-option-id)))
              (list (second current-option) 
                    (third current-option)
                    (fourth current-option))
              (loop (rest options)))))))


; Función: get-response2
; Descripción: Obtiene una respuesta del chatbot basada en un mensaje del usuario.
; Estrategia: Procesamiento de listas y búsqueda
(define (get-response2 chatbot message)
  (let* ((start-flow-id (fourth chatbot))
         (flows (fifth chatbot))
         (welcome-message (third chatbot))
         (flow (find-flow-by-id2 flows start-flow-id)))
    (if (not flow)
        (list "No se encontró el flujo." start-flow-id 1)
        (let* ((response-and-ids (find-option-message-and-ids-by-keyword2 flow message))
               (response (first response-and-ids))
               (chatbot-id (second response-and-ids))
               (flow-id (third response-and-ids)))
          (if (string=? response "Opción no encontrada.")
              (list welcome-message start-flow-id 0)
              (list response flow-id chatbot-id))))))


; Función: update-system-with-response2
; Descripción: Actualiza el sistema con una nueva respuesta del chatbot.
; Estrategia: Procesamiento de listas y reemplazo
(define (update-system-with-response2 system user-message system-response next-flow-id next-chatbot-id)
  (let* ((chatbot-code next-chatbot-id) 
         (chatbots (third system))
         (chatbot (find-chatbot-by-code2 chatbots chatbot-code)))
    (list (first system)
          chatbot-code
          (replace-chatbot2 chatbots chatbot next-flow-id)
          (fourth system)
          (append (fifth system) (list (list user-message system-response)))
          (sixth system))))


; Función: system-talk-norec
; Descripción: Procesa un mensaje del usuario y actualiza el sistema con la respuesta del chatbot sin usar recursión directa.
; Estrategia: Procesamiento de listas y actualización de estado
(define (system-talk-norec system message)
  (let* ((user-code (first system))
         (chatbot-code (second system))
         (chatbots (third system))
         (chatbot (find-chatbot-by-code2 chatbots chatbot-code))
         (response-and-next-ids (get-response chatbot message))
         (response (first response-and-next-ids))
         (next-flow (second response-and-next-ids))
         (next-chatbot-id (third response-and-next-ids))
         (updated-system (update-system-with-response2 system message response next-flow next-chatbot-id)))
    updated-system))


; -------------------------------------------------------------------------------------------------------------------

; Descripción: Concatena y formatea el historial de chat entre usuario y sistema.
; Estrategia: Iteración a través del historial de chat usando `map`.
; Entradas: system (lista) - estructura del sistema; user (cadena) - usuario actual (no utilizado).
; Retorno: Cadena - representación textual del historial de chat.
; Recursión: No aplica.
(define (system-synthesis system user)
  (let* ((chat-history (fifth system)))
    (apply string-append
           (map (lambda (entry)
                  (let* ((user-message (first entry))
                         (system-response (second entry)))
                    (format "Mensaje: ~a\nOpcion: ~a\n---\n" user-message system-response)))
                chat-history))))

