*= 0x000

;       Register usage:
;               Bank 0
;                       R0-R7   user defined
;               Bank 1
;                       R0      tmp-high
;                       R1      tmp-low
;                       R2      tmp
;                       R3      fn param 0 / tmp0
;                       R4      fn param 1 / tmp1
;                       R5      backup of ACC
;                       R6      backup of carry
;                       R7      backup of command register
;
;       4002-1 #0 usage:
;                       SRC reg
;                       CC RR MMMM
;               Output port:
;                       00 00 0000      program memory RAM access
;                                       address bits 11-8
;
;
;       4002-1 #1 usage:
;                       SRC reg
;                       CC RR MMMM
;                       01 00 xxxx      Status 0        => addr bits 0..3
;                       01 00 xxxx      Status 1        => addr bits 4..7
;                       01 00 xxxx      Status 2        => addr bits 8..11
;
;                       01 01 xxxx      Status 0        => OPR
;                       01 01 xxxx      Status 1        => OPA
;
;               Output port:
;                       01 00 0000      disable (0), display select (1-6),
;                                       clear pkeys (7)
;
;       4289 I/O port:
;
;                       CCCC            type            what
;                       111x 0000       input           Keyboard input (!KSEL)
;                       110x 0000       input           Program controls (!PSEL)
;
;       4265 I/O ports:
;                       W0..W3          input (unused)
;                       X0              serial input
;                       X1..X3          input  (unused)
;                       Y0              serial output
;                       Y1..Y3          output (unused)
;                       Z0..Z3          output (unused)
;
;       Memory Map
;                  4289 C3-C0
;                       101x       RAM #1   (0xA00-0xBFF)
;                       100x       RAM #1   (0x800-0x9FF)
;                       011x       RAM #0   (0x600-0x7FF)
;                       010x       RAM #0   (0x400-0x5FF)
;                       001x       ROM      (0x200-0x3FF)
;                       000x       ROM      (0x000-0x1FF)
;
;                       either normal address access during program execution
;                       or via 4002-1 #0 output port lines during write or
;                       read program memory
;
;
;
;       CPU starts here, first thing we do is to jump to init. Init will
;       issue a HLT. Cpu should only be resumed by issuing a reset, which
;       will enter the int handler. The int handler is the monitor program,
;       which is fixed for now.

start:  JUN init
        NOP
        ; ********************************************************************
        ; BEGIN OF INTERRUPT HANDLER
        ; ********************************************************************
inth:   SB1                     ; switch to index reg bank 1
        XCH 5                   ; store ACC to R5, bank 1
        TCC                     ; store carry to A3 bit
        XCH 6                   ; store carry to R6, bank 1
        LCR                     ; load command reg into ACC
        XCH 7                   ; store into R7
        ; ********************************************************************
        ; clear all control keys
mon:    JMS sdelay
        FIM P0, 0x40            ; select 4002-1 chip #1
        SRC P0
        LDM ~7                  ; 7 = !PCLR
        WMP                     ; write to '138
        JMS sdelay
        LDM ~0                  ; 0 = inactive
        WMP                     ; write to '138
        FIM P0, 0xD0            ; C3-C1 = 6 (!PSEL)
        SRC P0
wckey:  RDR
        JCN AZ wckey            ; loop if no key pressed
        ; keys: 1 = DUMP, 2 = DEP, 4 = ADDR, 8 = START
        CLC
                                ; key is in A, check which one
        RAR                     ; bit #0 = dump key
        JCN CZ ndump            ; check dump = 2
        ; *********** dump byte ***********
        JMS inca
dump:   JMS paddr
        JMS srcwr
        JMS rbyte
        JUN mon
ndump:  RAR                     ; bit #1 = deposit byte key
        JCN CZ ndep             ; check dep = 4
        ; *********** deposite byte *******
debkey:	JMS ibyte               ; input byte from keyboard
        JMS wbyte
        JMS rbyte               ; DEBUG: display input byte
        JUN mon
ndep:   RAR                     ; bit #2 = addr input key
        JCN CZ naddr
        
        ; *********** input addr ***********
        JMS iaddr               ; input and print addr
	JUN dump
	JUN mon
naddr:  RAR			; bit #3 = GO key
	JCN CZ mon 		; no other keys left to check
	; *********** GO key ***************
go:     JUN end_rom
        ; ********************************************************************
        ; END OF INTERRUPT HANDLER
        ; ********************************************************************
                                ; end of interrupt routine, restore everything
        XCH 7                   ; read back command reg

        RAL                     ; shift CM-ROM bit into CY
        JCN CZ kbank            ; keep bank
        DB1                     ; select ROM Bank 1
kbank:  RAR                     ; shift back
        DCL                     ; load command reg
        XCH 6                   ; fetch carry
        RAR                     ; restore carry
        XCH 5                   ; fetch acc
        BBS                     ; return from monitor, will switch back to
                                ; bank 0
        ; ********************************************************************
init:   FIM P0, 0x40            ; Select 4002-1 chip #1
        SRC P0
        LDM ~7                  ; reset PKEYs
        WMP                     ; write to RAM #1 I/O port
        LDM ~0                  ; inactive
        WMP                     ; write to RAM #1 I/O port

        LDM 0 ; for test, start with addr 0x000
        WR2                     ; store address 0x400
        LDM 0
        WR1
        WR0

        LDM 4
        DCL                     ; enable CM-RAM3
        FIM P0, 0x80            ; chip #2 = 4265
        SRC P0
        LDM 6                   ; 4265 mode 6 (W,X in; Y,Z out)
        WMP


        LDM 1                   ; init serial output with 1
        WRM                     ; set-bit Y0

        LDM 0
        DCL                     ; reenable CM-RAM0


        JMS paddr
        JMS srcwr
        JMS rbyte
        JUN mon         ; this will handled by int later ?

        JMS paddr
        JMS srcwr
        JMS rbyte               ; read byte and display

        DIN                     ; enable interrupts
        HLT                     ; wait for interrupt
        JUN end_rom             ; after int handler we will be here,
                                ; jumping to end of ROM into user program

        ; ********************************************************************
        ; UTILITY SUB ROUTINES
        ; ********************************************************************
        ; short delay
sdelay: LDM 7
        XCH 4
sdel4:  LDM 0
        XCH 2
sdel2:  LDM 0
        XCH 3
sdel3:  NOP
        ISZ 3 sdel3
        ISZ 2 sdel2
        ISZ 4 sdel4
        BBL 0

        ; increments stored address
inca:   FIM P0, 0x40
        SRC P0
        RD0
        IAC
        WR0
        JCN CZ incanc
        RD1
        IAC
        WR1
        JCN CZ incanc
        RD2
        IAC
        WR2
incanc: BBL 0

        ; paddr: print address
        ; params: / RAM status chars 02..00 has addr A11..A0
        ; returns: /
        ; prints the address A0..A11 to 7-segments #6..#4
paddr:  FIM P0, 0x40            ; RAM 0, register 0
        SRC P0
        LDM 4
        XCH 4                   ; R4 = digit 4
        RD0                     ; read status char 0
        JMS pdig
        INC 4
        RD1                     ; read status char 1
        JMS pdig
        INC 4
        RD2                     ; read status char 2
        JMS pdig
        BBL 0

        ; pdig: print digit
        ; params: A = content to print
        ;         R4 = which digit (1-6)
        ; comment: R4 is restored, A is lost
        ; returns: /
pdig:   FIM P0, 0
        SRC P0
        CMA
        WMP                     ; write display data
        FIM P0, 0x40               ; 4002-1 chip #1
        SRC P0
        XCH 4                   ; LED index from R4 to A
        CMA
        WMP                     ; enable 7seg latch, selected by R4
        CMA
        XCH 4                   ; restore R4
        LDM ~0                  ; select idle state
        WMP                     ; disable LED latch
        BBL 0                   ; return from subroutine

        ; iaddr: input address with keyboard and display
iaddr:  JMS ckey                ; ckey returns with 0x40 in SRC
        JCN AN iaddr            ; returns 1 if no key pressed
        LDM 6                   ; display to digit 6
        XCH 4                   ; R4 = digit index
        XCH 2                   ; R2 to A (pressed key)
        WR2                     ; write to RAM #1, reg 0, status char 2
        JMS pdig
wup0:   JMS ckey
        JCN AZ wup0             ; returns 0 if key pressed
wpd1:   JMS ckey
        JCN AN wpd1             ; returns 1 if no key pressed
        XCH 2
        WR1                     ; write to RAM #1, reg 0, status char 1
        XCH 4
        DAC
        XCH 4                   ; next digit
        JMS pdig
wup1:   JMS ckey
        JCN AZ wup1             ; returns 0 if key pressed
wpd2:   JMS ckey
        JCN AN wpd2             ; returns 1 if no key pressed
        XCH 2
        WR0                     ; write to RAM #1, reg 0, status char 0
        XCH 4
        DAC                     ; next digit
        XCH 4
        JMS pdig
wup2:   JMS ckey
        JCN AZ wup2             ; returns 0 if key pressed
        BBL 0

        ; ibyte: input program byte
ibyte:  JMS ckey
        JCN AN ibyte            ; loop until keypress
        INC 0                   ; increment P0 to 0x50 (reg 1)
        SRC P0                  ; RAM #1, register 1
        XCH 2                   ; load pressed key into A
        WR1                     ; store to RAM #1, reg 1, status char 1
wuopr:  JMS ckey
        JCN AZ wuopr
wopa:   JMS ckey
        JCN AN wopa
        INC 0                   ; increment P0 to 0x50 (reg 1)
        SRC P0                  ; RAM #1, register 1
        XCH 2                   ; load pressed key into A
        WR0                     ; store to Ram #1, reg 1, status char 0
wuopa:  JMS ckey
        JCN AZ wuopa
        BBL 0

        ; ckey: check for key
        ; params: /
        ; returns: keycode in R2 with ACC = 0  or ACC = 1 for no key
ckey:   STC                     ; set carry, we want to rotate a TTL 0,
                                ;       but we use output port of 4002-1 RAM,
                                ;       so a logic 1 is 0V for TTL 0
                                ; as column selector
        LDM 0                   ; begin with ACC = 0000
        RAL                     ; carry comes in from the right,
                                ;       ACC = 0001, CY = 0
ncol:   FIM P0, 0x00
        SRC P0                  ; RAM #0 is enabled

        WMP                     ; enable scan col => i/o out data = 1
        FIM P0, 0xE0
        SRC P0                  ;
        XCH 2                   ; backup current col
        RDR                     ; read rom port => when RDR, data gets
                                ; through keyb matrix because of SRC=E0,
                                ;       hence [C3 C2 C1] = 7
                                ; the TTL 0 that is read by the 4289 must be
        CMA                     ; converted to negative logic by CMA, since 4289
                                ; reads positive logic
        JCN AN gotkey           ; one bit set? got key!
	XCH 2                   ; restore col
        RAL                     ; next col
        JCN CZ ncol             ; if carry is 1, 0000 is in ACC, and we are done
	 
	BBL 1                   ; return error - no key
gotkey: KBP                     ; ACC = 1..4 (the row we read from rom port)
        DAC                     ; ACC = 0..3
        CLC                     ; clear carry
        RAL
        RAL                     ; ACC = RR00 (R = row bit)
        XCH 2                   ; store to R2 and load backed-up column
        KBP
        DAC                     ; ACC = 00CC
        CLC
        ADD 2                   ; ACC now  RRCC which should equal the pressed key
        XCH 2                   ; store it into R2
        FIM P0, 0x40
        SRC P0                  ; RAM chip #1 is enabled for display
        BBL 0                   ; return 0 - got key

        ; srcwr: initiate SRC for WPM and RPM form addr stored in RAM
        ; construct memory address for SRC operation
        ; lower 8-bits will be used for WPM
srcwr:  FIM P0, 0x40
        SRC P0
        RD2                     ; load bits 11..8 into A
        FIM P0, 0x00
        SRC P0
        CMA
        WMP                     ; write RAM port to output addr bits 11..8
        FIM P0, 0x40
        SRC P0
        RD1                     ; load addr 7..4
        XCH 0                   ; store to R0
        RD0                     ; load addr 3..0
        XCH 1                   ; store to R1
        SRC P0                  ; use as src addr
        BBL 0

        ; wbyte: write program byte to program RAM
        ; we load address and content to deposit from RAM status chars
        ; params: none
        ; returns: /
wbyte:  FIM P0, 0x50            ; 4002-1 chip #1
        SRC P0
        RD0                     ; read OPA
        XCH 3
        RD1                     ; read OPR
        XCH 2
        JMS srcwr
        XCH 3                   ; load OPA
        WPM                     ; write program memory
        XCH 2                   ; load OPR
        WPM                     ; write program memory
        BBL 0

        ; rbyte: read program byte from RAM and display
        ; params: none, srcwr must be called before!
        ; returns: /
rbyte:  RPM                     ; read OPA
        XCH 3                   ; store to R3
        RPM                     ; read OPR
        XCH 2                   ; store to R2
        FIM P0, 0x40
        SRC P0
        LDM 1
        XCH 4                   ; R4 = digit 1
        XCH 3                   ; display OPA
        JMS pdig
        INC 4                   ; next digit
        XCH 2                   ; print OPR
        JMS pdig
        BBL 0

;*********************** serial send / recv routines *************************
teststr:
.asciiz "Intel 4040 "
        ; stest1: write the letter 'R' to the serial port repeatedly
        ;         we store it low nibble first because LSB is sent first
stest1: FIM P1, @teststr
        FIM P0, 0x60
        SRC P0
        XCH R3          ; addr 0..3
        WR0
        XCH R2          ; addr 4..7
        WR1
        LDM 1           ; we know that we are in page 1
        WR2
        JMS ssend       ; print string
        BBL  0

        ; sbyte: write byte to serial port with 1200 baud 1-N-8
        ; scratch regs: R0-R5, A
        ; input byte: RAM chip 0, reg 0, main characters 14 and 15
sbyte:  FIM P2, 0x80 ; (4265 on CE-RAM3 has chip ID 2)
        FIM P0, 0x6E ; (RAM chip 1 (R0 = 0 after wait count),
                     ;  register 0, main character [R1 = 14])
        LDM 4
        DCL             ; CM-RAM3
        SRC P2
        LDM 0
        WRM             ; start bit
waits:  ISZ 0, waits    ; hold port value for time
snibb:  LDM 0
        DCL             ; CM-RAM0
        FIM P1, 0x0D    ; (init R2 = 0, R3 = D = 3 repetitions)
        SRC P0          ; enable RAM chip 0, register 0, character [R1]
        RDM             ; read nibble from char [R1]
        XCH 0
        LDM 4
        DCL
        SRC P2          ; enable 4265 I/O
        XCH 0           ;                                                33
bits:   CLC             ; clear carry                                    34   34
        RAR             ; shift LSB to carry                             35   35
        XCH 2           ;  A = 0, R2 = A backup                          36   36
        RAL             ; shift carry into LSB                           37   37
        WRM             ; out to port Y (idx 000b)                       38   38
        RAR             ; shift LSB back out, A = 0                           1
        LDM 3
        XCH 0
        LDM 0
        XCH 2           ; R2 = 0, restore A backup                            5
waits1: ISZ 0, waits1   ; hold port value for time                            31

        ISZ 3, bits     ; repeat for 3 bits                     2             33

bit0:   CLC             ; same as above but adapted hold time   1             34
        RAR             ;                                       1             35
        XCH 2           ;                                       1             36
        RAL             ;                                       1             37
        WRM             ;                                       1             38
        RAR             ;                                                 1
                        ; need not restore A here, finished
        LDM 9
        XCH 0
        FIM P2, 0x90    ;                                                 5

waits2: ISZ 0, waits2   ; hold port value for time                       19
        NOP
        ISZ 1, snibb    ; next 4 bits                                    21

        FIM P0, 0xAE    ;                                                23
waits3: ISZ 0, waits3   ;                                                35
        NOP             ;                                                36
        LDM 1           ; stop bit                                       37
        WRM             ;                                                38
        LDM 0           ;                                                1
        DCL             ; restore CM-RAM0                                2
        FIM P0, 0x0E    ;                                                4
waits4: ISZ 0, waits4   ; hold port value for time                       36
        BBL 0           ;                                                37

        ; ssend: send string, beginning at ADDR SC2..SC0 in RAM chip 1, reg 2
ssend:  FIM P0, 0x60
        SRC P0
        RD0             ; read addr 0..3
        XCH 3
        RD1             ; read addr 4..7
        XCH 2
        RD2             ; read addr 8..11
        FIM P0, 0
        SRC P0
        CMA
        WMP             ; output inverted high addr A11..A8
        SRC P1
        RPM
        XCH 4           ; low nibble in R4
        RPM
        XCH 3           ; high nibble in R3
        LD  3           ; load 3 into A again
        OR4             ; or R4, thanks 4040 :)
        JCN AZ szero    ; 0 terminator reached
        FIM P0, 0x0E    ; (RAM chip 0 (R0 = 0 after wait count),
                        ; register 0, main character [R1 = 14])
        SRC P0
        XCH 4
        WRM             ; write low nibble
        INC 1           ; next main char
        SRC P0
        XCH 3
        WRM             ; write high nibble
        JMS sbyte       ; send byte we just stored
        FIM P0, 0x60
        SRC P0
        RD0
        IAC
        WR0
                        ; increment address with carry
        JCN CZ ssend    ; next byte
        RD1
        IAC
        WR1
        JCN CZ ssend    ; next byte
        RD2
        IAC
        WR2
        JUN ssend       ; next byte
szero:  BBL 0

srepeat:JMS stest1
	JUN srepeat

*= 0x400
end_rom:

