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

; add-chatbot
(define (chatbot-exists? system chatbot)
  (ormap (lambda (existing-chatbot)
           (= (car existing-chatbot) (car chatbot)))
         (caddr system)))

(define (system-add-chatbot system chatbot)
  (if (chatbot-exists? system chatbot)
      system
      (cons (car system)
            (cons (cadr system)
                  (cons (append (caddr system) (list chatbot))
                        (cdr (cddr system)))))))

; add-user
(define (user-exists? system user)
  (ormap (lambda (existing-user) (string=? existing-user user))
         (cadddr system))) 

(define (system-add-user system user)
  (if (user-exists? system user)
      system  
      (list (car system)  
            (cadr system) 
            (caddr system)  
            (append (cadddr system) (list user)) 
            )))

; login
(define (system-login system user)
  (if (or (not (user-exists? system user))  ; Si el usuario no existe en el sistema
          (list-ref system 5))  ; o si alguien ya ha iniciado sesión (la sexta parte del sistema es el nombre del usuario)
      system  ; Devolver el sistema tal como está
      (list (car system)     ; Nombre del sistema
            (cadr system)    ; InitialChatbotCodeLink
            (caddr system)   ; Lista de chatbots
            (cadddr system)  ; Lista de usuarios
            (list-ref system 4)  ; chatHistory
            user)))  ; Establecer el usuario que ha iniciado sesión

; logout
(define (system-logout system)
  (list (car system)     ; Nombre del sistema
        (cadr system)    ; InitialChatbotCodeLink
        (caddr system)   ; Lista de chatbots
        (cadddr system)  ; Lista de usuarios
        (list-ref system 4)  ; chatHistory
        #f))  ; Resetear el campo de usuario que ha iniciado sesión a #f





