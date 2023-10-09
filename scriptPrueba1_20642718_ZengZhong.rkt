#lang racket

(require "TDAsystem_20642718_ZengZhong.rkt")
(require "TDAchatbot_20642718_ZengZhong.rkt")
(require "TDAflow_20642718_ZengZhong.rkt")
(require "TDAoption_20642718_ZengZhong.rkt")

; -------------------- Script de pruebas 1 --------------------
 (define op1 (option 1 "1) Viajar" 2 1 "viajar" "turistear" "conocer"))
 (define op2 (option 2 "2) Estudiar" 3 1 "estudiar" "aprender" "perfeccionarme"))
 (define f10 (flow 1 "flujo1" op1 op2 op2 op2 op2 op1)) 
 (define f11 (flow-add-option f10 op1)) 
 (define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10))  
 (define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0))
 (define s1 (system-add-chatbot s0 cb0))
 (define s2 (system-add-user s1 "user1"))
 (define s3 (system-add-user s2 "user2"))
 (define s4 (system-add-user s3 "user2")) 
 (define s5 (system-add-user s4 "user3"))
 (define s6 (system-login s5 "user8")) 
 (define s7 (system-login s6 "user1"))
 (define s8 (system-login s7 "user2"))
 (define s9 (system-logout s8))
 (define s10 (system-login s9 "user2"))
