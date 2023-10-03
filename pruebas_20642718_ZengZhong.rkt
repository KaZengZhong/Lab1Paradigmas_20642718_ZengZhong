#lang racket

(require racket/date)
(require "TDAsystem_20642718_ZengZhong.rkt")
(require "TDAchatbot_20642718_ZengZhong.rkt")
(require "TDAflow_20642718_ZengZhong.rkt")
(require "TDAoption_20642718_ZengZhong.rkt")

; Script de pruebas 1
; (define op1 (option 1 "1) Viajar" 2 1 "viajar" "turistear" "conocer"))
; (define op2 (option 2 "2) Estudiar" 3 1 "estudiar" "aprender" "perfeccionarme"))
; (define f10 (flow 1 "flujo1" op1 op2 op2 op2 op2 op1)) ;solo añade una ocurrencia de op2
; (define f11 (flow-add-option f10 op1)) ;se intenta añadir opción duplicada