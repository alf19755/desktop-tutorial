;UNIVERSIDAD DEL VALLE DE GUATEMALA
;IE2023 Programación de Microcontroladores
;Autor:		Mónica Alfaro
;Compilador:	pic-as (v2.36), MPLABX (v6.00)
;
;Programa:	POSTLaboratorio4 (Contador bonario de 4 bits con 2 botones de
;			      incremento y decremento.
;			      Los botones deben funcionar con interrupciones
;			      del PORTB Y estar configurados como PULL UPS int.
;
;				Un segundo contador binario de 4 bits que
;				opera con interrupción del tmr0, estando a
;				20 ms, pero el contador incrementa cada 1seg.
;				Este contador debe desplegarse a un display de
;				7 segmentos.
;
;Dispositivo:	PIC16F887
;Hardware:	LEDs en el puerto A, botones en el puerto B
;
;Creado: 13 de febrero , 2023
;Última modificación:  13 de febrero , 2023


PROCESSOR 16F887
#include <xc.inc>
;configuration word 1
 CONFIG FOSC=INTRC_NOCLKOUT //OSCILADOR INTERNO SIN SALIDAS
 CONFIG WDTE=OFF //WDT DISEABLED (REINICIO REPETITIVO DEL PIC)
 CONFIG PWRTE=OFF //PWRT ENABLED (ESPERA DE 72ms AL INICIAR)
 CONFIG MCLRE=OFF //EL PIN DE MCLR SE UTILIZA COMO I/0
 CONFIG CP=OFF	//SIN PROTECCIÓN DE CÓDIGO
 CONFIG CPD=OFF	//SIN PROTECCIÓN DE DATOS

 CONFIG BOREN=OFF //SIN REINICIO CUÁNDO EL VOLTAJE DE ALIMENTACIÓN BAJA DE 4V
 CONFIG IESO=OFF //REINCIO SIN CAMBIO DE RELOJ DE INTERNO A EXTERNO
 CONFIG FCMEN=OFF //CAMBIO DE RELOJ EXTERNO A INTERNO EN CASO DE FALLO
 CONFIG LVP=OFF //PROGRAMACIÓN EN BAJO VOLTAJE PERMITIDA

;configuration word 2
 CONFIG WRT=OFF	//PROTECCIÓN DE AUTOESCRITURA POR EL PROGRAMA DESACTIVADA
 CONFIG BOR4V=BOR40V //REINICIO ABAJO DE 4V, (BOR21V=2.1V)

PSECT udata_bank0	; common memory


 PSECT udata
 W_TEMP:
    DS 1
 STATUS_TEMP:
    DS 1
cont_t0: DS 1
;Nuevas
display_unidades: DS 1
display_decenas: DS 1

PSECT resVect, class=code, abs, delta=2
;----------------------------------VECTOR RESET----------------------------
ORG 00h			    ;Posicion 0000h para el vector
resetVec:
    PAGESEL main
    goto main

PSECT code, delta=2, abs
ORG 100h		    ;Posicion para el codigo

;--------------------------VECTOR DE INTERRUPCIONES-----------------------------

PSECT code, delta=2, abs
 ORG 0x0004
PUSH:			    ;Parte de el código que menciona el datasheet.
    MOVWF W_TEMP
    SWAPF STATUS, W
    MOVWF STATUS_TEMP

;Interrupción de PORTB
I_RBIF:
    btfsc INTCON, 0	    ; Revisa si la bandera IRBIF está activada
			    ;?antes estaba en Btfss pero aca estaba toda la info
			    ;de incremento y decremento de botones.
    call botonesB

;Interrupciones del TMR0
I_T0IF:
    btfsc   T0IF	    ;Ver si la bandera T0IF es 0, me salto una línea
    call    contador2	    ;Pero si no es 0 (es 1) me voy a la subrutina




POP:
    SWAPF STATUS_TEMP, W
    MOVWF STATUS
    SWAPF W_TEMP, F
    SWAPF W_TEMP, W
    RETFIE		    ;REGRESA DE LA INTERRUPCIÓN
;*******************************************************************************
;CÓDIGO PRINCIPAL
;*******************************************************************************
PSECT CODE, delta = 2, abs
 ORG 0x100
;-------------Tabla display antes de código-------------------
contadorhexa: ;Entradas en A y salidas en B
   ;Tablas para cátodo común

   clrf PCLATH		    ;Program Counter
   bsf	PCLATH, 0
   ;--------------Volviendo de 4 bits---------------------------
   andlw 0x0f		    ;Solo puedo llegar a 16 con el and.
   addwf PCL		    ;PC= PCLATH+ PCL +W

   retlw 00111111B ; 0	    ;No usamos return normal,ya que esa devuelve una lit
   retlw 00000110B  ;1
   retlw 01011011B  ;2
   retlw 01001111B  ;3
   retlw 01100110B  ;4
   retlw 01101101B  ;5
   retlw 01111101B  ;6
   retlw 00000111B  ;7
   retlw 01111111B  ;8
   retlw 01101111B  ;9
   retlw 01110111B  ;A
   retlw 01111100B  ;b
   retlw 00111001B  ;C
   retlw 01011110B  ;d
   retlw 01111001B  ;E
   retlw 01110001B  ;F

   /*A la hora de conectarlo se ve así
   Bit0 del puerto = Línea superior (pin 4 de arriba)
   Bit1 del puerto = Línea inferior derecha (pin 4 de abajo)
   Bit2 del puerto = Línea
   Bit3 del puerto = Línea
   Bit4 del puerto = Línea
   Bit5 del puerto = Línea
   Bit5 del puerto = Línea



   */

main:

    call configuracion_inicial
    call config_clck
    call config_tmr0

    call interrupcion_puertoB
    call interrupciones


    BANKSEL PORTC
    clrf PORTE

    clrf PORTC
    clrf PORTB
    clrf PORTD
    clrf PORTA
    clrf  cont_t0
    clrf display_unidades	;Limpiar estas variables permite iniciar en 00
    clrf display_decenas

;*******************************************************************************
;LOOP INFINITO
;*******************************************************************************

loop:

   goto loop

;*******************************************************************************
;CONFIGURACIONES
;*******************************************************************************

configuracion_inicial:

    banksel ANSEL	    ; Configuración de pines digitales
    clrf ANSEL
    clrf ANSELH

    banksel TRISC
    clrf TRISC
    clrf TRISB

    banksel TRISB
    ;Salidas


    movlw 0b11110000	    ;4 bits de salida para puerto d (Contador2)
    movwf TRISD


    clrf TRISC		    ;Todo el puerto C como salida  (Display Contador2)


    clrf TRISA		    ;Todo el puerto A como salida  (Display Contador2)

    ;Entradas

    movlw 0b00000011	    ;2 bits de entrada para puerto b(Botones Contador1)
    movwf TRISB

    return

config_clck:

    banksel OSCCON	    ;Vamos al banco 1, donde está el OSCCON
    bcf OSCCON, 4	    ;Los bits 5 y 6 del registro OSCCON son IRCF y
    bsf OSCCON, 5	    ;la combinación de esos bits determina el FOSC
    bcf OSCCON, 6	    ;IRCF=010  250khZ --> verificar ircf en datasheet
    bsf OSCCON, 0	    ;Selección de reloj interno con el bit 0 (SCS)
    return

config_tmr0:
    banksel OPTION_REG	    ;Vamos al banco 1
    bcf OPTION_REG, 5; T0CS		    ;? RELOJ INTERNO

    bcf OPTION_REG, 3 ;PSA		    ;PRESCALER

    bsf OPTION_REG, 2; PS2
    bsf OPTION_REG, 1 ;PS1
    bsf OPTION_REG, 0 ;PS0  ;PS=111 --> verificar en hoja de datos
    banksel PORTA
    call reiniciar_tmr0 ;optimización del reinicio

    return

reiniciar_tmr0:
    movlw 251                     ;251
    movwf TMR0
    bcf   T0IF
    return
;-------------------RECORDAR FORMULA PARA SABER LA FRECUENCIA DEL TIMER-----------
;   TEMPORIZACIÓN=TOSC*TMR0*PRESCALER
;   FOSC(frecuencia de oscilación)= 250 khz
;   TOSC (periodo de oscilación)= 1/250 000 = 0.000004 O 4 micro segundos
;   TMR0 = 256- n
;   N=VALOR A CARGAR EN TMR0
;   prescaler= 111 equivale a 256 (revisar datasheet)
;Entonces
;	20ms =4 * 4exp-6 * (256-tmr0)* 256
;	20*10^-3/ (4*256*4*10^-6)=256-tmr0    -->4.0960*10-3
;	4.8828-256= -tmr0
;	251.11=tmr0



interrupciones:

    banksel INTCON
    clrf INTCON


    bsf INTCON, 6	    ; Habilita las interrupciones Periféricas PEIE

    ;Interrupciones Puerto B
    bsf INTCON, 3	    ; Habilita las interrupciones RBIE
    bsf INTCON, 0	    ; Habilita las interrupciones RBIF

    ;Interrupciones TMR0
    bsf INTCON, 2	    ; Habilita las interrupciones T0IF
    bsf INTCON, 5	    ; Habilita las interrupciones T0IE

    bsf INTCON, 7	    ; Habilita las interrupciones Globales GIE

    return

interrupcion_puertoB:

    BANKSEL IOCB
    BSF IOCB, 0
    BSF IOCB, 1		    ; HABILITANDO RB0 Y RB1 PARA LAS ISR DE RBIE

    BANKSEL OPTION_REG
    BCF OPTION_REG, 7	    ; HABILITANDO PULLUPS PUERTO B

    BSF WPUB, 0
    BSF WPUB, 1		    ; HABILITANDO LOS PULLUPS EN RB0 Y RB1

    RETURN

botonesB:
    revisarboton1:
	btfss PORTB, 0	    ; Revisa si el botón está presinoado (llegan 5v)
	incf PORTD, F	    ; Incrementa el contador en PORTC
	;Antirebote
	btfss PORTB, 0
	goto  $-1

	bcf INTCON, 0	    ; S

    revisarboton2:
	btfss PORTB, 1	    ; Revisa si el botón está presinoado (llegan 5v)
	decf PORTD, F	    ; Decrementa el contador en PORTC
	;Antirebote
	btfss PORTB, 1
	goto  $-1
	bcf  INTCON, 0
	;
	return

contador2:
    call    reiniciar_tmr0  ;Llamo a la subrutina para poner el delay del tmr0.

    ;Dado quee stá incrementando a 20 ms, hacemos una variable para que incre-
    ;mente a 1 segundo. 1 segundo tiene 50 veces 20 ms, por lo que:

    ;-------------------------PRUEBA 1 SEGUNDO----------------------------------
    ;Tiempo
    incf    cont_t0	    ;Cuando se reinicia el timer0, incremento una variab
    movf    cont_t0, w	    ;Muevo esa variable a w
    sublw    50		    ;Y le resto 50
    btfss   STATUS, 2	    ;Verifico si esa resta da 1. De ser así, salto
    return		    ;Pero si es 0, vuelvo de donde me hicieron el call
    clrf    cont_t0	    ;Pero si es 1, se limpia el contador e

    ;::::::::::::::::::::::::::::DISPLAYS:::::::::::::::::::::::::::::::::::::::
    ;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    call unidades


    return

			    ;¿Por qué se usa btfsc STATUS, 2? Dado que el subwf
			    ;afecta la bandera z (Que es el bit 2 del registo
			    ;de STATUS), en vez de verificar la variable "pasaunseg"
			    ;con el btfs, verificamos si la bandera ya se encendió

unidades:
    ;Incrementar contador definido por el tiempo anterior
    incf display_unidades   ;Incremento el variable unidades.

    ;Volviéndolo de 10 digitos, nada más
    movf display_unidades, w	;
    sublw 10			;HASTA LLEGAS A 10
    btfss STATUS, 2
    goto $+2
    ;Ir a incrementar las decenas cuando ya sea 10
    call decenas

    ;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ;Ir a incrementar las decenas cuando ya sea 0



    movf display_unidades, w

    ;COMO YA NO MOSTRAMOS EL VALOR  DEL CONTADOR BINARIO DEL DISPLAY EN UN PUERTO,
    ;AHORA COMENTAMOS ESTAS LÍNEAS.
    ;AHORA ESTE PUERTO ESTARÁ DISPONIBLE PARA EL SEGUNDO DISPLAY DEL TMR0.

    ;movwf   PORTC	    ;Mostramos el valor de la variable en PORTC

    ;------------------------Ingresando el valor a displays---------------------

    ;movf PORTC, w	    ;configuración para el display de 7 segmentos hexa
    call contadorhexa
    movwf PORTA		    ;Y lo muestro en el puerto A
    return

decenas:
    clrf display_unidades	;Primero reseteo el display de unidades para
				;Que vuelva a ser 0


    ;Incrementar contador definido por el tiempo anterior
    incf display_decenas   ;Incremento el variable decenas.

    ;Volviéndolo de 10 digitos, nada más
    movf display_decenas, w	;
    sublw 6			;Para que llegue a 60 segundos
    btfss STATUS, 2
    goto $+2
    clrf display_decenas	    ;Importante
    ;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    ;Ir a
    movf display_decenas, w


    ;------------------------Ingresando el valor a displays---------------------

    ;movf PORTC, w	    ;configuración para el display de 7 segmentos hexa
    call contadorhexa
    movwf PORTC		    ;Y lo muestro en el puerto C
    return


END

