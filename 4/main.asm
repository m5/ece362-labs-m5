;***********************************************************************
; ECE 362 - Experiment 4 - Spring 2010
;
; Mini-Monitor with Memory Editor, Go, Memory Display, Register Display
;
; SRAM mapped to 3800-3FFF, Flash mapped to 8000-FFFF
;***********************************************************************
;
; Completed by: Micah Fivecoate
;               2442-F
;               
;
;
; Academic Honesty Statement:  In signing this statement, I hereby certify
; that I am the individual who created this 9S12C32 source file and that I have
; not copied the work of any other student (past or present) while completing it.
; I understand that if I fail to honor this agreement, I will receive a grade of
; ZERO and be subject to possible disciplinary action.
;
;
; Signature: ________________________________________________   Date: ____________
;
; NOTE: The printed hard copy of this file you submit for evaluation must be signed
;       in order to receive credit.
;
;***********************************************************************
;

INITRM	equ	$0010	; INITRM - INTERNAL SRAM POSITION REGISTER
INITRG	equ	$0011	; INITRG - INTERNAL REGISTER POSITION REGISTER
RAMBASE	equ	$3800	; 2KB SRAM located at 3800-3FFF

;***********************************************************************
;
; ASCII character definitions
;
CR	equ	$D	; RETURN
LF	equ	$A	; LINE FEED
NULL	equ	$0	; NULL
DASH	equ	'-'	; DASH (MINUS SIGN)
PERIOD	equ	'.'	; PERIOD

;***********************************************************************
;
; Macro Definitions
;

$macro  print
        jsr     pmsg
        fcb     %1
        fcb     CR,LF,NULL
$macroend

$macro  prompt
        jsr     pmsg
        fcb     %1
        fcb     NULL
        jsr     inchar
$macroend

$macro  beq2
        cmpa    #%1
        beq     #%3
        cmpa    #%2
        beq     #%3
$macroend

$macro  niblx
        exg     d,x
        LSLD
        LSLD
        LSLD
        LSLD
        exg     d,x
$macroend   
        


;***********************************************************************

	org	$8000	; start of application program memory (32K Flash)

;
;  Boot-up entry point
;

startup_code

	movb	#$39,INITRM	  ; map RAM ($3800 - $3FFF)
  lds 	#$3FCE	    	; initialize stack pointer
	jsr 	ssinit	    	; initialize system clock and serial I/O

;***********************************************************************
;
; Start Mini-Monitor Application
;

main
	jsr	pmsg	; display welcome message upon reset/power-up
	fcb	CR,LF
	fcc	"9S12C32 Mini-Monitor S10 V1.0"
	fcb	CR,LF
	fcc	"Created by:  Micah Fivecoate 2442-F"
	fcb	CR,LF
	fcc	"Last updated:  February 15, 2010"
	fcb	CR,LF,NULL

mprmpt	jsr	pmsg	; display monitor prompt
	fcb	CR,LF
	fcc	"=>"
	fcb	NULL
	jsr	inchar	; input monitor command

        beq2    'm','M',mem_edit
        beq2    'g','G',go_addr
        beq2    'd','D',dump_mem
        beq2    'r','R',dump_reg
        
mexit	jmp	main    ; End of main loop


mem_edit
        print   "Memory edit mode..."
med_loop
        jsr     pmsg
        fcb     "Enter Address: "
        fcb     NULL
        jsr     hexin16
        cpx     #$3800
        blt     med_bad_addy
        cpx     #$4000
        bge     med_bad_addy
        jsr     pmsg
        fcb     "Enter New Value: "
        fcb     NULL

med_bad_addy
        print   "Error - Invalid Address - Try Again..."
        jsr med_loop


printscii
        psha
        pshb
        pshx
        tfr     a,b
        anda    #$0F
        jsr     hex2ascii
        jsr     outchar
        tfr     b,a
        lsra
        lsra
        lsra
        lsra
        jsr     hex2ascii
        jsr     outchar
        
hexin16
        psha
        pshb
        pshx
        lday    #$0000
        ldax    #$0000
hip_loop
        jsr     inchar
        tab
        cmpa    CR
        beq     hip_end
        jsr     ascii2hex
        bmi     hip_loop
        exg     a,b
        jsr     outchar
        niblx
        abx
        iny
        cpy     #$0004
        bne     hip_loop

hip_end
        pulx
        pulb
        pula
        rts

hex2ascii
        cmpa    #0F
        bgt     h2a_err
        cmpa    #09
        bgt     h2a_alph
        adda    #'0'
        bra     h2a_end

h2a_alph
        suba    #$0A
        adda    #'A'
        bra     h2a_end
        
h2a_err
        ldaa    #$FF

h2a_end
        rts
        
        
ascii2hex
h_alph  cmpa    #'A'
        blt     h_dec
        cmpa    #'F'
        bgt     h_err
        suba    #'A'
        adda    #$0A
        bra     h_end     

h_dec   cmpa    #'0'
        blt     h_err
        cmpa    #'9'
        bgt     h_err
        suba    #'0'
        bra     h_end
        
h_err   ldaa    #$FF
        
h_end   rts
        

;***********************************************************************
; Character I/O Library Routines for 9S12C32
;
; For flash-based applications created using AsmIDE
;***********************************************************************
;
; ==== CRG - Clock and Reset Generator Definitions

SYNR	EQU	$0034           ;CRG synthesizer register
REFDV	EQU	$0035           ;CRG reference divider register
CTFLG	EQU	$0036		;TEST ONLY
CRGFLG	EQU	$0037		;CRG flags register
CRGINT	EQU	$0038
CLKSEL	EQU	$0039		;CRG clock select register
PLLCTL	EQU	$003A		;CRG PLL control register
RTICTL	EQU	$003B
COPCTL	EQU	$003C
FORBYP	EQU	$003D
CTCTL	EQU	$003E
ARMCOP	EQU	$003F

; ==== SCI Register Definitions

SCIBDH	EQU	$00C8		;SCI0BDH - SCI BAUD RATE CONTROL REGISTER
SCIBDL	EQU	$00C9		;SCI0BDL - SCI BAUD RATE CONTROL REGISTER
SCICR1	EQU	$00CA		;SCI0CR1 - SCI CONTROL REGISTER
SCICR2	EQU	$00CB		;SCI0CR2 - SCI CONTROL REGISTER
SCISR1	EQU	$00CC		;SCI0SR1 - SCI STATUS REGISTER
SCISR2	EQU	$00CD		;SCI0SR2 - SCI STATUS REGISTER
SCIDRH	EQU	$00CE		;SCI0DRH - SCI DATA REGISTER
SCIDRL	EQU	$00CF		;SCI0DRL - SCI DATA REGISTER
PORTB	EQU	$0001		;PORTB - DATA REGISTER
DDRB	EQU	$0003		;PORTB - DATA DIRECTION REGISTER

;
; Initialize system clock serial port (SCI) for 9600 baud
;
; Assumes PLL is engaged -> CPU bus clock is 24 MHz
;

ssinit	bclr	CLKSEL,$80	; disengage PLL to system
	bset	PLLCTL,$40	; turn on PLL
	movb	#$2,SYNR	; set PLL multiplier
	movb	#$0,REFDV	; set PLL divider
	nop
	nop
plllp   brclr CRGFLG,$08,plllp  ; while (!(crg.crgflg.bit.lock==1))
	bset  CLKSEL,$80	; engage PLL to system
;
; Disable watchdog timer (COPCTL register)
;
	movb	#$40,COPCTL	; COP off; RTI and COP stopped in BDM-mode
;
; Initialize SCI (COM port)
;
	movb	#$00,SCIBDH	; set baud rate to 9600
	movb	#$9C,SCIBDL	; 24,000,000 / 16 / 156 = 9600 (approx)
	movb	#$00,SCICR1	; $9C = 156
	movb	#$0C,SCICR2	; initialize SCI for program-driven operation
	movb	#$10,DDRB	; set PB4 for output mode
	movb	#$10,PORTB	; assert DTR pin of COM port
	rts

;
; SCI handshaking status bits
;

rxdrf    equ   $20    ; receive data register full (RDRF) mask pattern
txdre    equ   $80    ; transmit data register empty (TDRE) mask pattern

;***********************************************************************
; Name:         inchar
; Description:  inputs ASCII character from SCI serial port
;                  and returns it in the A register
; Returns:      ASCII character in A register
; Modifies:     A register
;***********************************************************************

inchar  brclr  SCISR1,rxdrf,inchar
        ldaa   SCIDRL ; return ASCII character in A register
        rts


;***********************************************************************
; Name:         outchar
; Description:  outputs ASCII character passed in the A register
;                  to the SCI serial port
;***********************************************************************

outchar brclr  SCISR1,txdre,outchar
        staa   SCIDRL ; output ASCII character to SCI
        rts


;***********************************************************************
; pmsg -- Print string following call to routine.  Note that subroutine
;         return address points to string, and is adjusted to point to
;         next valid instruction after call as string is printed.
;***********************************************************************

pmsg    pulx            ; Get pointer to string (return addr).
ploop   ldaa    1,x+    ; Get next character of string.
        beq     pexit   ; Exit if ASCII null encountered.
        jsr     outchar ; Print character on terminal screen.
        bra     ploop   ; Process next string character.
pexit   pshx            ; Place corrected return address on stack.
        rts             ; Exit routine.


;***********************************************************************
; Subroutine:	htoa
; Description:  converts the hex nibble in the A register to ASCII
; Input:	hex nibble in the A accumualtor
; Output:	ASCII character equivalent of hex nibble
; Reg. Mod.:	A, CC
;***********************************************************************

htoa    adda	 #$90
	daa
	adca	 #$40
	daa
	rts


;***********************************************************************
; Subroutine:	atoh
; Description:  converts ASCII character to a hexadecimal digit
; Input:	ASCII character in the A register
; Output:	converted hexadecimal digit returned in A register
;               CF = 0 if result OK; CF = 1 if error occurred (invalid input)
; Reg. Mod.:	A, CC
;***********************************************************************

atoh       pshb
           pshx
           pshy
           suba    #$30   ; subtract "bias" to get ASCII equivalent
           blt     outhex
           cmpa    #$0a
           bge     cont1
quithx	   clc             ; return with CF = 0 to indicate result OK
           puly
           pulx
           pulb
           rts

cont1      suba    #$07
           cmpa    #$09
           blt     outhex
           cmpa    #$10
           blt     quithx
           suba    #$20
           cmpa    #$09
           blt     outhex
           cmpa    #$10
           blt     quithx

outhex	   sec            ; set CF <- 1 to indicate error
           puly
           pulx
           pulb
           rts


;***********************************************************************
; Subroutine:	getbyte
; Description:  inputs two ASCII characters and converts them to byte integer
; Input:	<none>
; Output:	converted hexadecimal value returned in A register
;               if error in character input, echo "?"
; Reg. Mod.:	A
;***********************************************************************

getbyte    pshc
getblp     jsr    inchar   ; get first ASCII character         
	   cmpa	  #CR
	   beq    gbexit

           jsr    outchar  ; echo character
	   cmpa	  #PERIOD  ; check for exit mode character
	   beq	  gbexit
	   cmpa	  #DASH	   ; check for backup character
	   beq	  gbexit

           jsr    atoh     ; convert ASCII character to hex
           bcs    errhex1  ; if not hex, go to error routine
           asla            ; shift converted hex digit
           asla            ;   to upper nibble
           asla
           asla
           psha            ; save on stack temporarily

get2       jsr    inchar   ; get second ASCII character
           jsr    outchar  ; echo to screen
           jsr    atoh     ; convert ASCII character to hex
           bcs    errhex2  ; if not hex, go to error routine
           oraa   1,sp+    ; OR converted hex digits together

gbexit     pulc
           rts

errhex1    ldaa   #'?'     ; get ? to prompt for new character
           jsr    outchar
           bra    getblp

errhex2    ldaa   #'?'
           jsr    outchar
           bra    get2


;***********************************************************************
; Subroutine:	getword
; Description:  inputs four ASCII characters and converts them to word integer
; Input:	<none>
; Output:	converted hexadecimal value returned in D register
; Reg. Mod.:	A, CC
;***********************************************************************

getword	jsr	getbyte		; get first byte of the data entered
        cmpa    #PERIOD		; check for exit mode character
	bne	getnxw
	rts

getnxw	tfr	a,b		; save MSB in B
	jsr	getbyte		; get second byte of data entered
	exg	a,b		; put MSB in A and LSB in B
	rts


;***********************************************************************
; Subroutine:	disbyte
; Description:  displays 8-bit (binary) value as two ASCII characters
; Input:	8-bit value passed in A register
; Output:	<none>
; Reg. Mod.:	A, CC
;***********************************************************************

disbyte	psha		; save value passed on stack
	anda	#$F0	; get most significant digit of result
	lsra
	lsra
	lsra
	lsra
	jsr	htoa
	jsr	outchar	; display most significant digit
	pula		; restore original value
	anda	#$0F	; get least significant digit of resust
	jsr	htoa	; convert result to ASCII character
	jsr	outchar	; display least significant digit
	rts


;***********************************************************************
; Subroutine:	disword
; Description:  displays 16-bit (binary) value as four ASCII characters
; Input:	16-bit value passed in D register
; Output:	<none>
; Reg. Mod.:	A, CC
;***********************************************************************

disword	pshd		; save value passed on stack
	jsr	disbyte	; display high byte
	puld
	exg	a,b
	jsr	disbyte	; display low byte
	rts


;***********************************************************************
; "Go" / "Register Display" test code.
; Description:  Loads registers with test values and returns to 'main'.
;               The registers should display the values specified here
;               when followed by the "R" command.
; Reg. Mod.:	D, X, Y, CC
;***********************************************************************

  org     $800  ; This is mapped to $3800 at the beginning of 'main'
      
  ;;; Load registers with test values ;;;
  ldd     #$1234  
  ldx     #$ABCD
  ldy     #$CDEF
  andcc   #0
  orcc    #%10101010
  
  jmp     $800b   ; return to 'main'
                  ; ** this assumes that nothing was changed
                  ; ** between "org $8000" and "main"
                  ; ** (which should be a safe assumption)


;***********************************************************************
;
; If get bad interrupt, just return
;
BadInt  rti
;
;***********************************************************************
;
; Define 'where you want to go today' (reset and interrupt vectors)
;
; Note this is the "re-mapped" table in Flash (located outside debug monitor)
;
; ------------------ VECTOR TABLE --------------------

	org	$FF8A
	fdb	BadInt	;$FF8A: VREG LVI
	fdb	BadInt	;$FF8C: PWM emergency shutdown
	fdb	BadInt	;$FF8E: PortP
	fdb	BadInt	;$FF90: Reserved
	fdb	BadInt	;$FF92: Reserved
	fdb	BadInt	;$FF94: Reserved
	fdb	BadInt	;$FF96: Reserved
	fdb	BadInt	;$FF98: Reserved
	fdb	BadInt	;$FF9A: Reserved
	fdb	BadInt	;$FF9C: Reserved
	fdb	BadInt	;$FF9E: Reserved
	fdb	BadInt	;$FFA0: Reserved
	fdb	BadInt	;$FFA2: Reserved
	fdb	BadInt	;$FFA4: Reserved
	fdb	BadInt	;$FFA6: Reserved
	fdb	BadInt	;$FFA8: Reserved
	fdb	BadInt	;$FFAA: Reserved
	fdb	BadInt	;$FFAC: Reserved
	fdb	BadInt	;$FFAE: Reserved
	fdb	BadInt	;$FFB0: CAN transmit
	fdb	BadInt	;$FFB2: CAN receive
	fdb	BadInt	;$FFB4: CAN errors
	fdb	BadInt	;$FFB6: CAN wake-up
	fdb	BadInt	;$FFB8: FLASH
	fdb	BadInt	;$FFBA: Reserved
	fdb	BadInt	;$FFBC: Reserved
	fdb	BadInt	;$FFBE: Reserved
	fdb	BadInt	;$FFC0: Reserved
	fdb	BadInt	;$FFC2: Reserved
	fdb	BadInt	;$FFC4: CRG self-clock-mode
	fdb	BadInt	;$FFC6: CRG PLL Lock
	fdb	BadInt	;$FFC8: Reserved
	fdb	BadInt	;$FFCA: Reserved
	fdb	BadInt	;$FFCC: Reserved
	fdb	BadInt	;$FFCE: PORTJ
	fdb	BadInt	;$FFD0: Reserved
	fdb	BadInt	;$FFD2: ATD
	fdb	BadInt	;$FFD4: Reserved
	fdb	BadInt	;$FFD6: SCI Serial System
	fdb	BadInt	;$FFD8: SPI Serial Transfer Complete
	fdb	BadInt	;$FFDA: Pulse Accumulator Input Edge
	fdb	BadInt	;$FFDC: Pulse Accumulator Overflow
	fdb	BadInt	;$FFDE: Timer Overflow
	fdb	BadInt	;$FFE0: Standard Timer Channel 7
	fdb	BadInt  ;$FFE2: Standard Timer Channel 6
	fdb	BadInt	;$FFE4: Standard Timer Channel 5
	fdb	BadInt	;$FFE6: Standard Timer Channel 4
	fdb	BadInt	;$FFE8: Standard Timer Channel 3
	fdb	BadInt	;$FFEA: Standard Timer Channel 2
	fdb	BadInt	;$FFEC: Standard Timer Channel 1
	fdb	BadInt	;$FFEE: Standard Timer Channel 0
	fdb	BadInt	;$FFF0: Real Time Interrupt (RTI)
	fdb	BadInt	;$FFF2: IRQ (External Pin or Parallel I/O) (IRQ)
	fdb	BadInt	;$FFF4: XIRQ (Pseudo Non-Maskable Interrupt) (XIRQ)
	fdb	BadInt	;$FFF6: Software Interrupt (SWI)
	fdb	BadInt	;$FFF8: Illegal Opcode Trap ()
	fdb	startup_code	;$FFFA: COP Failure (Reset) ()
	fdb	BadInt		;$FFFC: Clock Monitor Fail (Reset) ()
	fdb	startup_code	;$FFFE: /RESET
	end

;***********************************************************************
; ECE 362 - Experiment 4 - Spring 2010
;***********************************************************************