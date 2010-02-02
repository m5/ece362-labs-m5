;***********************************************************************
; ECE 362 - Experiment 3 - Fall 2009
;
; Skeleton file for Steps 1 & 2
;***********************************************************************
;
; Completed by: Micah Fivecoate
;               2442-F
;               < your lab division >
;
;
; Academic Honesty Statement:  In signing this statement, I hereby certify
; that I am the individual who created this HC12 source file and that I have
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
; Step 1: 
;
; Subroutine: Nchars
;
; Inputs: ASCII value of starting character in the A register
;         Number of characters to be printed in the B register
;
; Function: Prints a series of N characters beginning with the ASCII 
;	    character in the A register and followed by the N-1 
;	    characters that follow in the ASCII table
;
; Subroutines called:  outchar
;
;***********************************************************************

	org	0800h
testnc			; main program to use for testing subroutine nchars
	lds	#$1000	;    by itself (have to initialize SP and the
	jsr	sinit	;    target board serial port)
	jsr	nchars
	stop

nchars
        jsr     outchar
        inca
        decb
        bne     nchars

	rts

;***********************************************************************
; Step 2: 
;
; Program: main
;
; Function:  Prompts the user for the beginning character and the 
;            number of characters to be printed.
;            Prints a series of N characters beginning with the ASCII 
;	     character in the A register and followed by the N-1 
;	     characters that follow in the ASCII table.
;	     Entering the character "q" should cause the program to exit.
;
; Subroutines called: pmsg, inchar, nchars
;
;***********************************************************************

	org	0850h
	lds	#$1000	; initialize SP
	jsr	sinit	; initialize serial port

        jsr     pmsg
        fcb     "Enter character: "
        fcb     NULL
        jsr     inchar
        exg     A,B

        jsr     pmsg
        fcb     "Enter character: "
        fcb     NULL
        jsr     inchar
        exg     A,B

        jsr     nchars
        

	stop		; use this location to set breakpoint


;***********************************************************************
; Character I/O Library Routines for 9S12C32
;***********************************************************************
;
; ==== SCI Register Definitions

SCIBDH		EQU	$00C8		;SCI0BDH - SCI BAUD RATE CONTROL REGISTER
SCIBDL		EQU	$00C9		;SCI0BDL - SCI BAUD RATE CONTROL REGISTER
SCICR1		EQU	$00CA		;SCI0CR1 - SCI CONTROL REGISTER
SCICR2		EQU	$00CB		;SCI0CR2 - SCI CONTROL REGISTER
SCISR1		EQU	$00CC		;SCI0SR1 - SCI STATUS REGISTER
SCISR2		EQU	$00CD		;SCI0SR2 - SCI STATUS REGISTER
SCIDRH		EQU	$00CE		;SCI0DRH - SCI DATA REGISTER
SCIDRL		EQU	$00CF		;SCI0DRL - SCI DATA REGISTER
PORTB		EQU	$0001		;PORTB - DATA REGISTER
DDRB		EQU	$0003		;PORTB - DATA DIRECTION REGISTER

;
; Initialize asynchronous serial port (SCI) for 9600 baud
;
; Assumes PLL not engaged -> CPU bus clock is 4 MHz
;

sinit	movb	#$00,SCIBDH	; set baud rate to 9600
	movb	#$1A,SCIBDL	; 4,000,000 / 16 / 26 = 9600 (approx)
	movb	#$00,SCICR1	; $1A = 26
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

	end

;***********************************************************************
; ECE 362 - Experiment 3 - Fall 2009
;***********************************************************************