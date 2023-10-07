#lang racket

(require racket/date)
(provide (all-defined-out))

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

; add-chatbot
;; Verifica si un chatbot ya existe en el sistema
(define (chatbot-exists? system chatbot)
  (ormap (lambda (existing-chatbot)
           (= (car existing-chatbot) (car chatbot)))
         (caddr system)))

;; Añade un chatbot al sistema si no existe
(define (system-add-chatbot system chatbot)
  (cond
    ;; Si el chatbot ya existe, muestra un mensaje de error y devuelve el sistema sin cambios
    ((chatbot-exists? system chatbot)
     (display "El chatbot ya existe.\n")
     system)

    ;; En caso contrario, añade el chatbot al sistema
    (else
     (list (car system)                   ; Nombre del sistema
           (cadr system)                  ; InitialChatbotCodeLink
           (append (caddr system) (list chatbot)) ; Lista de chatbots con el nuevo chatbot añadido
           (cadddr system)))))            ; Lista de usuarios

; add-user
(define (user-exists? system user)
  (ormap (lambda (existing-user) (string=? existing-user user))
         (cadddr system))) 

(define (system-add-user system user)
  (if (user-exists? system user)
      (begin
        (display "El usuario ya existe.\n")
        system)  ; Si el usuario ya existe, simplemente devuelve el sistema sin cambios
      (list (car system)        ; Nombre del sistema
            (cadr system)       ; InitialChatbotCodeLink
            (caddr system)      ; Lista de chatbots
            (append (cadddr system) (list user)) ; Añadir el nuevo usuario a la lista de usuarios
            (list-ref system 4) ; chatHistory
            (if (>= (length system) 6) (list-ref system 5) #f)))) ; Mantener el usuario logueado (si hay alguno) o #f si no hay sesión iniciada


; login
(define (system-login system user)
  (cond
    ;; Si el usuario no existe en el sistema o alguien ya ha iniciado sesión
    ((or (not (user-exists? system user))
         (and (>= (length system) 6) (list-ref system 5)))
     (display "No se pudo iniciar sesion.\n")
     system) ; Devolver el sistema tal como está

    ;; En caso contrario, establecer el usuario que ha iniciado sesión
    (else
     (list (car system)      ; Nombre del sistema
           (cadr system)     ; InitialChatbotCodeLink
           (caddr system)    ; Lista de chatbots
           (cadddr system)   ; Lista de usuarios
           (list-ref system 4) ; chatHistory
           user))))  ; Establecer el usuario que ha iniciado sesión

; logout
(define (system-logout system)
  (list (car system)     ; Nombre del sistema
        (cadr system)    ; InitialChatbotCodeLink
        (caddr system)   ; Lista de chatbots
        (cadddr system)  ; Lista de usuarios
        (list-ref system 4)  ; chatHistory
        #f))  ; Resetear el campo de usuario que ha iniciado sesión a #f

; talk-rec
(define (system-talk-rec system message)
  ;; Comprobar si alguien ha iniciado sesión
  (if (not (list-ref system 5))
      (begin
        (display "Necesitas iniciar sesión primero.\n")
        system)
      (if (find-response-in-flow (find-flow-by-id (find-chatbot-by-id system (cadr system)) (caddr (find-chatbot-by-id system (cadr system)))) message)
          (begin
            (display (string-append "Chatbot: " (find-response-in-flow (find-flow-by-id (find-chatbot-by-id system (cadr system)) (caddr (find-chatbot-by-id system (cadr system)))) message) "\n"))
            system)
          (begin
            (display "Chatbot: No entiendo ese mensaje. Intenta de nuevo.\n")
            system))))

(define (find-chatbot-by-id system id)
  (cond
    ((null? (caddr system)) #f)
    ((= id (caar (caddr system))) (car (caddr system)))
    (else (find-chatbot-by-id (cdr (caddr system)) id))))

(define (find-flow-by-id chatbot id)
  (cond
    ((null? (cadddr chatbot)) #f)
    ((= id (caar (cadddr chatbot))) (car (cadddr chatbot)))
    (else (find-flow-by-id (cdr (cadddr chatbot)) id))))

(define (find-response-in-flow flow message)
  (if (assoc message (caddr flow))  ; Buscar la opción por mensaje
      (cadr (assoc message (caddr flow)))
      #f))





