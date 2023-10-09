#lang racket

(require "TDAsystem_20642718_ZengZhong.rkt")
(require "TDAchatbot_20642718_ZengZhong.rkt")
(require "TDAflow_20642718_ZengZhong.rkt")
(require "TDAoption_20642718_ZengZhong.rkt")

; -------------------- Script de pruebas personal ------------------------------------------------------------------------------

(define op1 (option 1 "1) Si" 0 2 "si"))
(define op2 (option 2 "2) No" 0 2 "no"))
(define op3 (option 1 "1) Probablemente" 1 2 "probablemente"))
(define op4 (option 2 "2) En absoluto" 1 2 "en absoluto"))
(define op5 (option 1 "1) A lo mejor" 2 2 "a lo mejor"))
(define op6 (option 2 "2) Imposible" 2 2 "imposible"))
(define op7 (option 3 "3) Tal vez" 0 2 "tal vez"))
(define op8 (option 3 "3) Tal vez" 1 2 "tal vez"))
(define op9 (option 3 "3) Tal vez" 2 2 "tal vez"))

(define f1 (flow 1 "¿Te gusta estudiar?" op1 op2))
(define f2 (flow 2 "¿Te gusta trabajar?" op3 op4))
(define f3 (flow 3 "¿Te gusta jugar?" op5 op6))

(define f4 (flow-add-option f1 op7))
(define f5 (flow-add-option f2 op8))
(define f6 (flow-add-option f3 op9)) 

(define cb0 (chatbot 0 "Chatbot 0" "Bienvenido, responde la siguiente pregunta" 1 f4))
(define cb1 (chatbot 1 "Chatbot 1" "Bienvenido, responde la siguiente pregunta" 1 f5))
(define cb2 (chatbot 2 "Chatbot 2" "Bienvenido, responde la siguiente pregunta" 1 f6))

(define cb3 (chatbot-add-flow cb0 f5))
(define cb4 (chatbot-add-flow cb1 f6))
(define cb5 (chatbot-add-flow cb2 f4))

(define s0 (system "Chatbot Principal" 0 cb3 cb4 cb5))
(define s99 (system "Chatbot Principal" 0 cb4))
(define s98 (system "Chatbot Principal" 0 cb5))

(define s1 (system-add-chatbot s99 cb5))
(define s2 (system-add-chatbot s99 cb3))
(define s97 (system-add-chatbot s99 cb5))

(define s3 (system-add-user s0 "diego"))
(define s4 (system-add-user s3 "benjamin"))
(define s5 (system-add-user s4 "jorge"))

(define s6 (system-login s5 "diego"))
(define s96 (system-login s5 "benjamin"))
(define s95 (system-login s5 "jorge"))

(define s94 (system-logout s6))
(define s93 (system-logout s96))
(define s92 (system-logout s95))

(define s7 (system-talk-rec s6 "2"))
(define s8 (system-talk-rec s7 "1"))
(define s9 (system-talk-rec s8 "2"))

(define s10 (system-talk-norec s6 "1"))
(define s11 (system-talk-norec s10 "2"))
(define s12 (system-talk-norec s11 "1"))

(display(system-synthesis s9 "diego"))
(display(system-synthesis s12 "diego"))
(display(system-synthesis s7 "diego"))