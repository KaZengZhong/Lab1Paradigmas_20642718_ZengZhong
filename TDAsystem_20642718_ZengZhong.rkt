#lang racket

(require racket/date)
(provide (all-defined-out))
;TDA CHATBOT
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
        '()))

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

; Funciones Auxiliares

(define (find-chatbot-by-code chatbots code)
  (if (null? chatbots)
      #f
      (if (= (first (first chatbots)) code)
          (first chatbots)
          (find-chatbot-by-code (rest chatbots) code))))

(define (replace-chatbot chatbots chatbot new-start-flow-id)
  (if (null? chatbots)
      '()
      (if (= (first (first chatbots)) (first chatbot))
          (cons (list (first chatbot) (second chatbot) (third chatbot) new-start-flow-id (fifth chatbot)) 
                (rest chatbots))
          (cons (first chatbots) 
                (replace-chatbot (rest chatbots) chatbot new-start-flow-id)))))

(define (find-flow-by-id flows id)
  (if (null? flows)
      #f
      (if (= (first (first flows)) id)
          (first flows)
          (find-flow-by-id (rest flows) id))))

(define (string-numeric? str)
  (let ((num-str (string->number str)))
    (and num-str (number? num-str))))

; Funciones Principales

(define (get-response chatbot message)
  (let* ((start-flow-id (fourth chatbot))
         (flows (fifth chatbot))
         (welcome-message (third chatbot))
         (flow (find-flow-by-id flows start-flow-id)))
    
    ; Comprueba si el flujo fue encontrado
    (if (not flow)
        (list "No se encontró el flujo." start-flow-id 1)
        (let* ((response-and-ids (find-option-message-and-ids-by-keyword flow message))
               (response (first response-and-ids))
               (chatbot-id (second response-and-ids))
               (flow-id (third response-and-ids)))
          ; Si no se encuentra una opción en el flujo para el mensaje, devuelve el mensaje de bienvenida
          (if (string=? response "Opción no encontrada.")
              (list welcome-message start-flow-id 0)
              (list response flow-id chatbot-id))))))



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

;system-synthesis
(define (system-synthesis system user)
  (let* ((chat-history (fifth system)))
    (apply string-append
           (map (lambda (entry)
                  (let* ((user-message (first entry))
                         (system-response (second entry)))
                    (format "Mensaje: ~a\nOpcion: ~a\n---\n" user-message system-response)))
                chat-history))))













