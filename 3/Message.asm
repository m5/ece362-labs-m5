;***********************************************************************
; ECE 362 - Experiment 3 - Fall 2009
;
; Skeleton file for Steps 4 & 5
;***********************************************************************
;
; Completed by: Micah Fivecoate
;               2442-F
;               
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
;  Declarations
;***********************************************************************

N	equ	04h
NULL	equ	00h
CR	equ	0dh
LF	equ	0ah


;***********************************************************************
; Step 4: 
;
; Subroutine: pmsgx
;
; Inputs: X register contains the starting address of a NULL-terminated
;	  string
;
; Function: Prints the string beginning with the address in the X 
;           register.
;
; Subroutines called:  outchar
;
;***********************************************************************
	org	0800h
pmsgx

pxloop
        ldaa    0,x+
        bne     pxexit
        jsr     outchar
        bra     pxloop
        
pxexit
	rts

;***********************************************************************
; Step 5: 
;
; Program: message
;
; Defined value:  N, which is the number of regular messages allowed. 
;    (In addition, there is an "exit" message and an error message.)
;
; Function:  Prompts the user for a message number, prints the
;	     corresponding message, then repeats the prompt.
;            The program provides an error message if the
;            message number is out-of-range.  If the user selects 0,
;            the program will print an exit message and exit.
;
;	     (also answers the question, "Were you awake in class??")
;
; Subroutines called:  pmsgx, inchar, pmsg
;
;***********************************************************************

	org	0850h
message
	lds	#$1000
	jsr	sinit

mprompt
        pmsg
        fcb     "Enter number: "
        fcb     NULL
        jsr     inchar

        suba    #$30
        blt     merror
        beq     mexit

        cmpa    #$04
        bgt     merror

        ldx     A,msgtbl
        jsr     pmsgx

        bra     mprompt


merror
        ldx     xmsg
        jsr     pmsgx
        bra     mprompt

mexit
        ldx     msg0
        jsr     pmsgx
	stop

;
; This table contains the addresses of the messages 0 - N
;

msgtbl	fdb	msg0
	fdb	msg1
	fdb	msg2
	fdb	msg3
	fdb	msg4

;
; Message storage area
;

msg0	fcb	"Good-bye!"
	fcb	CR,LF,NULL
msg1	fcb	"Message 1."
	fcb	CR,LF,NULL
msg2	fcb	"Message 2."
	fcb	CR,LF,NULL
msg3	fcb	"Message 3."
	fcb	CR,LF,NULL
msg4	fcb	"Message 4."
	fcb	CR,LF,NULL
xmsg	fcb	"Invalid message number.  Try again!"
	fcb	CR,LF,NULL

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