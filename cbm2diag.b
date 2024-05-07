; cbm2 diagnostic-cart 324835-01
; disassembled by Vossi 05/2024, fixed/enhanced 05/2024
; assemble with ACME
!cpu 6502
!ct scr		; standard text/char conversion table -> Screencode (pet = PETSCII, raw)
!to "cbm2diag.bin", plain
; * switches
;STATICFULL	= 1	; Tests full 2kB static RAM
; ***************************************** CONSTANTS *********************************************
FILL			= $ff		; fills free memory areas with $ff
SYSTEMBANK		= $0f		; systembank
; test parameters
TIMERCYCLES		= $0a70		; compare cycles (determined with a LA and a real P500)
; VDC register
ADR			= $0		; address register
DATA			= $1		; data register
; TPI register
PC			= $2		; port c
MIR			= $5		; interrupt mask register
CR			= $6		; control register
; CIA register
TALO			= $4		; timer a lo
TAHI			= $5		; timer a hi
TBLO			= $6		; timer b lo
TBHI			= $7		; timer b hi
TOD10			= $8		; tod 10th of seconds
TODSEC			= $9		; tod seconds
TODMIN			= $a		; tod monutes
TODHR			= $b		; tod hours
ICR			= $d		; interrupt control register
CRA			= $e		; control register b
CRB			= $f		; control register b
; SID register
OSC1			= $00		; oscillator 1
OSC2			= $07		; oscillator 2
OSC3			= $0e		; oscillator 3
FREQLO			= $00		; frequency lo
FREQHI			= $01		; frequency hi
PWLO			= $02		; pulse width lo
PWHI			= $03		; pulse width hi
OSCCTL			= $04		; oscillator control
ATKDCY			= $05		; attack/decay
SUSREL			= $06		; sustain/release
RESFILT			= $17		; resonance/filter
VOLUME			= $18		; volume
; ***************************************** ADDRESSES *********************************************
!addr CodeBank		= $00		; code bank register
!addr IndirectBank	= $01		; indirect bank register
!addr ScreenRAM		= $d000		; Screen RAM
!addr vdc		= $d800		; VDC
!addr sid		= $da00		; SID
!addr cia		= $dc00		; CIA
!addr acia		= $dd00		; ACIA
!addr tpi1		= $de00		; TPI1
!addr tpi2		= $df00		; TPI2
; ***************************************** ZERO PAGE *********************************************
!addr machinetype	= $04		; machine type: $40=LP, $80=HP
!addr temp1		= $05		; temp
!addr temp2		= $08		; temp
!addr pointer1		= $0a		; 16bit pointer
!addr romsize		= $0c		; rom size in pages
!addr pointer_screen	= $2a		; 16bit pointer screen text position
!addr pointer_text	= $2c		; 16bit pointer text
!addr pointer_screen2	= $2e		; 16bit pointer screen text position
 
!addr check		= $10		; check variable ********* PATCHED ********
!addr error		= $11		; error ********* PATCHED ********
!addr bank_state	= $15 ; - $18	; bank faulty state (max 4 banks)
!addr bank_state_full	= $0015
!addr ext_color		= $1b		; exterior color
!addr screen_pointer	= $1c		; 16bit pointer screen
!addr cycles		= $21 ; - $24	; cycles counter decimal for 8 digits
!addr end_high		= $25		; pages to test
!addr faulty_bits	= $27		; faulty test bits
!addr storage1		= $28		; temp storage
!addr storage2		= $29		; temp storage
!addr last_rambank	= $30		; last RAM bank
!addr copy_source_bank	= $32		; copy source bank
!addr copy_source	= $33		; 16bit copy source address
!addr counter		= $35		; counter
!addr color_pointer	= $36		; 16bit colorpointer
!addr test_mask		= $3a		; test mask (to test only 4 bit in color RAM)	
!addr screen_pos	= $3b		; screen pos rom checksums
!addr temp_count_sum	= $3c		; temp rom checksums
!addr temp6		= $3f		; temp6	
!addr start_high	= $41		; test start address highbyte	
!addr start_low		= $42		; test start address lowbyte	
!addr temp4		= $45		; temp
!addr text		= $46		; text to print
!addr temp7		= $48		; temp timer test
!addr temp5		= $49		; temp
!addr banks_counter	= $4a		; counter for banks to test in a cycle
!addr temp_bank		= $4b		; temp bank
!addr temp_dec_value	= $4c		; temp timertest
!addr temp_irq		= $4d		; temp irq
!addr pointer2		= $4e		; 16bit pointer
!addr pointer3		= $50		; 16bit pointer
!addr time1_hours	= $b2		; time 1 hours
!addr time1_minutes	= $b3		; time 1 minutes
!addr time1_seconds	= $b4		; time 1 seconds
!addr time1_10th	= $b5		; time 1 10th seconds
!addr time2_hours	= $b6		; time 2 hours
!addr time2_minutes	= $b7		; time 2 minutes
!addr time2_seconds	= $b8		; time 2 seconds
!addr time2_10th	= $b9		; time 2 10th seconds
!addr tod_count1	= $ba		; tod test counter
!addr tod_count2	= $bb		; tod test counter
!addr tod_count3	= $bc		; tod test counter
!addr tod_state		= $bd		; TOD state - $00 ok, $ff = bad
!addr timer_state	= $bd		; timer state - $00 = ok
; ***************************************** ZONE MAIN *********************************************
!zone main
!initmem FILL
*= $2000
	jmp Start			; jump to start
	jmp Start			; jump to start
	!byte $43, $c2, $cd, "2"	; cbm-rom ident-bytes 'C'= without init, 'BM', '2' = 4k-block 2
Start:	sei
	lda #SYSTEMBANK
	sta IndirectBank
	ldx $ff				; #$ff    ************ BUG ???????????????
	txs				; reset stack pointer
	cld
	ldy #$11			; 18 VDC register
	lda tpi2+PC			; check machine type bit#7
	bmi hpvdclp			; -> HP machine
lpvdclp:lda VDCInitValuesLP,y		; init LP VDC register
	sty vdc+ADR
	sta vdc+DATA
	dey
	bpl lpvdclp
	lda #$40			; type=LP
	jmp Init
; ----------------------------------------------------------------------------
; LP vdc init values
VDCInitValuesLP: 
	!byte $7f, $50, $60, $0a, $26, $01, $19, $1e
	!byte $00, $07, $20, $00, $00, $00, $00, $00
	!byte $00, $00
; ----------------------------------------------------------------------------
; 
hpvdclp:lda VDCInitValuesHP,y
	sty vdc+ADR
	sta vdc+DATA
	dey
	bpl hpvdclp
	lda #$80			; type=HP
	jmp Init
; ----------------------------------------------------------------------------
; HP vdc init values
VDCInitValuesHP:
	!byte $6c, $50, $53, $0f, $19, $03, $19, $19
	!byte $00, $0d, $20, $00, $00, $00, $00, $00
	!byte $00, $00
; ----------------------------------------------------------------------------
; 
Init:	sta machinetype			; store machine type
; clear screen
	ldx #$00
	lda #' '
clrlp:  sta ScreenRAM,x
	sta ScreenRAM+$100,x
	sta ScreenRAM+$200,x
	sta ScreenRAM+$300,x
	sta ScreenRAM+$400,x
	sta ScreenRAM+$500,x
	sta ScreenRAM+$600,x
	sta ScreenRAM+$700,x
	inx
	bne clrlp
; check ram banks
	ldx #$03			; minimum 2 banks / 128kB: testbank = 3
	stx $02				; store last ram bank +1
	stx IndirectBank		; indirect bank=3
	ldx #$24			; title text length
	lda #$60			; test at address $xx60
	sta temp2
	lda #$a5			; test value
	sta temp1
ckramlp:sta (temp2),y			; y already 0
	lda (temp2),y
	cmp temp1			; check testbyte ?
	beq bank3pr			; -> test ok
	iny
	bne ckramlp
	beq ram128
bank3pr:lda #$05			; 4 banks / 256kB
	sta $02				; store last ram bank +1
	bne ram256			; jump always
ram128: lda machinetype			; check lp/hp
	bmi hp128			; -> hp
; print title
lp128:	lda TitleLP128,x		; title 600 128k
	and #$bf
	sta ScreenRAM,x
	dex
	bne lp128
	beq Cycles			; always
;
hp128:	lda TitleHP128,x		; title 700 128k
	and #$bf
	sta ScreenRAM,x
	dex
	bne hp128
	beq Cycles			; always
;
ram256: lda machinetype			; check lp/hp
	bmi hp256			; -> hp
lp256:	lda TitleLP256,x		; title 600 256k
	and #$bf
	sta ScreenRAM,x
	dex
	bne lp256
	beq Cycles			; always
;
hp256:	lda TitleHP256,x		; title 700 256k
	and #$bf
	sta ScreenRAM,x
	dex
	bne hp256
; print cycles
Cycles:	lda #SYSTEMBANK
	sta IndirectBank		; indirect = systembank
	ldx #$08			; text length
prcyclp:lda TextCycles,x		; print "CYCLES"
	and #$bf
	sta ScreenRAM+1*80,x
	lda Text000001,x		; print "000001"
	and #$bf
	ora #$80			; inverse
	sta ScreenRAM+1*80+16,x
	dex
	bne prcyclp
; test zeropage
TestZeropage:
	ldx #$10			; text length
przplp:	lda TextZeropage,x		; print "ZEROPAGE"
	and #$bf
	sta ScreenRAM+3*80,x
	dex
	bne przplp
	ldy #$03			; start at $0003
zplp:	ldx #$00
zpcntlp:txa
	sta $0000,y			; zeropage
	eor $0000,y
	bne zpbad			; -> bad
	inx 
	bne zpcntlp			; count byte up
	tya
	sta $0000,y			; store address for check
	iny
	bne zplp			; next byte
	ldy #$03			; start at $0003
zpchklp:tya
	cmp $0000,y			; check stored address values
	bne zpbad
	iny
	bne zpchklp
	ldx #$03
zpoklp:	lda TextOK,x			; zeropage ok
	and #$bf
	sta ScreenRAM+3*80+16,x
	dex
	bne zpoklp
	jmp TestStaticRam
zpbad:	ldx #$03
zpbadlp:lda TextBad,x			; zeropage bad
	and #$bf
	ora #$80
	sta ScreenRAM+3*80+16,x
	dex
	bne zpbadlp
endless:lda #$01			; shift all bits in faulty byte
zpshft:	sta $0000,y
	eor #$ff
	sta $0000,y
	asl
	bcc zpshft
	jmp endless
; test static RAM
TestStaticRam:
	ldx #$10			; text length
prstalp:lda TextStaticRam,x		; print "STATIC RAM"
	and #$bf
	sta ScreenRAM+4*80,x
	dex
	bne prstalp
	ldx #$00
	stx pointer1
	inx				; start at $0100
	stx pointer1+1
	ldy #$00
statlp:	ldx #$00
statclp:txa
	sta (pointer1),y
	eor (pointer1),y
	bne statbad			; -> bad
	inx
	bne statclp			; count byte up
	tya
	clc
	adc pointer1+1			; add highbyte
	sta (pointer1),y		; store address for check
	iny
	bne statlp			; next byte
	inc pointer1+1			; inc page
	lda pointer1+1
!ifdef STATICFULL{
	cmp #$08			; test full 2kB static RAM		******** PATCHED ********
} else{
	cmp #$04			; original tests only 4 pages = 1kB
}
	bne statlp			; next page
	lda #$00
	sta pointer1
	ldx #$01			; start at $0100
	stx pointer1+1
	ldy #$00
stacklp:  tya
	clc
	adc pointer1+1
	cmp (pointer1),y		; check stored address values
	bne statbad			; -> bad
	iny
	bne stacklp			; next byte
	inx
	stx pointer1+1
!ifdef STATICFULL{
	cpx #$08			; check full 2kB static RAM		******** PATCHED ********
} else{
	cpx #$04			; original checks only 4 pages = 1kB
}
	bne stacklp			; next page
	ldx #$03
staoklp:lda TextOK,x			; static ram ok
	and #$bf
	sta ScreenRAM+4*80+16,x
	dex
	bne staoklp
	jmp MainTest
statbad:ldx #$03
stbadlp:lda TextBad,x			; static ram bad
	and #$bf
	ora #$80			; inverse
	sta ScreenRAM+4*80+16,x
	dex
	bne stbadlp
stashft:lda #$01			; shift all bits in faulty byte
	sta (pointer1),y
	eor #$ff
	sta (pointer1),y
	asl
	bcc stashft
	jmp MainTest
MainTest:
	nop
	nop
	nop
	lda #<(ScreenRAM+5*80)		; screen position for next text
	sta pointer_screen
	lda #>(ScreenRAM+5*80)
	sta pointer_screen+1
	lda #$10
	sta $28
	jsr TestVideoRam
	jsr TestRoms
	jsr TestKeyboard
	jsr TestRS232
	jsr l23b4
	jsr l2421
	jsr l24d7
	jsr l2579
	jsr l25e5
	jsr l26bf
	jsr l2763
	lda #$05
	jsr l28e0
	jsr l28a8
	jmp TestZeropage
; ----------------------------------------------------------------------------
; test video ram
TestVideoRam:
	ldx #>TextVideoRam
	ldy #<TextVideoRam
	jsr PrintText			; print text
	ldy #<ScreenRAM			; set start address
	sty pointer1
	lda #>ScreenRAM
	sta pointer1+1
vidlp:  lda (pointer1),y		; load old value
	sta temp2			; remember
	ldx #$00
vidcnlp:txa
	sta (pointer1),y
	eor (pointer1),y
	bne vidbad			; -> bad
	inx
	bne vidcnlp			; count byte up
	lda temp2
	sta (pointer1),y		; restore byte
	iny
	bne vidlp			; next byte
	inc pointer1+1			; inc page
	lda pointer1+1
	cmp #>(ScreenRAM+$800)		; video ram end ?
	bne vidlp			; next page
	jsr PrintOK
	jsr AddLine
	rts
; video ram bad
vidbad: jsr PrintBad
videndl:clc				; count faulty screen byte up
	adc #$01
	sta (pointer1),y
	jmp videndl			; endless
; ----------------------------------------------------------------------------
; ROM test tables
RomStartHigh:	!byte $80, $a0, $e0
RomSizePages:	!byte $20, $20, $20
RomChecksums:	!byte $80, $a0, $e0
; ----------------------------------------------------------------------------
; test ROM check sums
TestRoms:
	ldx #>TextBasicRomL
	ldy #<TextBasicRomL
	jsr PrintText
	ldx #$00			; Basic ROM low
	jsr TestRomx
	ldx #>TextBasicRomH
	ldy #<TextBasicRomH
	jsr PrintText
	ldx #$01			; Basic ROM high
	jsr TestRomx
	ldx #>TextKernalRom
	ldy #<TextKernalRom
	jsr PrintText
	ldx #$02			; Kernal ROM
	jsr TestRomx
	rts
; ----------------------------------------------------------------------------
; test rom x from table
TestRomx:
	ldy #$00
	sty pointer1
	lda RomStartHigh,x		; ROM start address high from table
	sta pointer1+1
	lda RomSizePages,x		; ROM size in pages
	sta romsize
	clc
	lda #$00
romlp:  adc (pointer1),y		; add rom byte with carry
	iny
	bne romlp			; add next byte
	inc pointer1+1
	dec romsize
	bne romlp			; next page
	adc #$00			; add last carry
	sta temp2			; store checksum
	cmp RomChecksums,x		; compare with table ?
	bne rombad			; -> not ok
	jsr PrintOK			; print ok
	jsr AddLine
	rts
; rom checksum bad
rombad:	jsr PrintBad			; print bad
	lda #24
	jsr AddChars			; add 24 chars
	lda temp2
	jsr PrintRomChecksum		; print wrong checksum
	jsr AddLine
	rts
; ----------------------------------------------------------------------------
; test keyboard
TestKeyboard:
	ldx #$2a
	ldy #$2a
	jsr PrintText
	ldy #$00
	sty $df03
	sty $df04
	sty $df05
	ldx #$01
l22cd:  lda #$3f
	sta $df03,x
	ldy #$c0
l22d4:  tya
	sta $df00,x
	sta $09
	lda $df02
	eor $09
	and #$3f
	bne l2326
	iny
	bne l22d4
	lda #$00
	sta $df03,x
	dex
	bpl l22cd
	lda #$c0
	sta $df00
	sta $df01
	sta $df03
	sta $df04
	lda #$30
	sta $09
	ldy #$07
l2302:  ldx #$01
l2304:  lda $09
	ora #$3f
	sta $df00,x
	lda $df02
	and #$0f
	cmp l232c,y
	bne l2326
	dey
	dex
	beq l2304
	asl $09
	cpy #$ff
	bne l2302
	jsr PrintOK
l2322:  jsr AddLine
	rts
; keyboard bad
l2326:  jsr PrintBad
	jmp l2322
; ----------------------------------------------------------------------------
; table
l232c:  !byte $0a, $0b, $0f, $0d, $05, $04, $00, $03
; ----------------------------------------------------------------------------
; test RS232
TestRS232:
	ldx #$2a
	ldy #$5d
	jsr PrintText
	sta $dd01
	lda $dd03
	ora #$10
	sta $dd03
	lda $dd02
	ora #$19
	sta $dd02
	lda #$0e
	sta temp2
l2352:  lda $dd03
	and #$f0
	ora temp2
	sta $dd03
	ldy temp2
	lda l23a4,y
	sta $dd00
	lda $dd02
	and #$f7
	sta $dd02
l236c:  lda $dd01
	and #$08
	beq l236c
	lda $dd00
	cmp l23a4,y
	bne l239e
	dec temp2
	bne l2352
	lda $dd02
	and #$fe
	sta $dd02
	lda $dd01
	tax
	and #$20
	beq l239e
	txa
	and #$40
	beq l239e
	sta $dd01
	jsr PrintOK
l239a:  jsr AddLine
	rts
; ----------------------------------------------------------------------------
; 
l239e:  jsr PrintBad
	jmp l239a
; ----------------------------------------------------------------------------
; table
l23a4:  !byte $ff, $55, $aa, $00, $01, $02, $04, $08
	!byte $10, $20, $40, $80, $ff, $cc, $33, $ff
; ----------------------------------------------------------------------------
; 
l23b4:  ldx #$2a
	ldy #$6e
	jsr PrintText
	lda $de04
	and #$7f
	ora #$60
	sta $de04
	ldx #$10
	stx $dc0d
	ldx $dc0d
	ldx #$04
	lda $de01
l23d2:  ora #$60
	sta $de01
	and #$df
	pha
	pla
	sta $de01
	pha
	pla
	dex
	bne l23d2
	lda #$f5
l23e5:  adc #$01
	bne l23e5
	lda $de01
	and #$80
	bne l241b
	lda $dc0d
	and #$10
	beq l241b
	lda $de01
	and #$bf
	sta $de01
	lda #$f5
l2401:  adc #$01
	bne l2401
	lda $de01
	and #$80
	beq l241b
	lda $de04
	and #$df
	sta $de04
	jsr PrintOK
l2417:  jsr AddLine
	rts
; ----------------------------------------------------------------------------
; 
l241b:  jsr PrintBad
	jmp l2417
l2421:  ldx #$2a
	ldy #$4c
	jsr PrintText
	lda #$ff
	sta $de00
	sta $de01
	sta $de04
	sta $de03
	lda #$cc
	sta $dc02
	sta $dc03
	eor #$ff
	sta temp2
	jsr l249c
	bcs l24c1
	lda #$33
	sta $dc02
	sta $dc03
	eor #$ff
	sta temp2
	jsr l249c
	bcs l24c1
	ldy $dc01
	nop
	lda $dc0d
	and #$10
	beq l24c1
	lda $dc0e
	and #$bf
	sta $dc0e
	ldx #$10
	clc
l246e:  lda #$04
	adc temp2
	sta temp2
	ora #$10
	sta $de01
	dex
	bne l246e
	lda $dc0d
	and #$08
	beq l24c1
	lda $dc0c
	cmp #$55
	bne l24c1
	jsr PrintOK
l248d:  jsr AddLine
	lda #$ff
	sta $de03
	sta $dc02
	sta $dc03
	rts
; ----------------------------------------------------------------------------
; 
l249c:  ldy #$0f
l249e:  ldx #$01
l24a0:  lda l24c7,y
	sta $dc00,x
	nop
	lda $dc00,x
	and temp2
	sta $09
	lda l24c7,y
	and temp2
	cmp $09
	bne l24bf
	dex
	bpl l24a0
	dey
	bpl l249e
	clc
	rts
l24bf:  sec
	rts
; ----------------------------------------------------------------------------
; 
l24c1:  jsr PrintBad
	jmp l248d
l24c7:  brk
	ora $0a
l24ca:  !byte $0f, $50, $55, $5a, $5f, $a0, $a5, $aa
	!byte $af, $f0, $f5, $fa, $ff
; ----------------------------------------------------------------------------
; 
l24d7:  ldx #$2a
	ldy #$3b
	jsr PrintText
	lda #$ff
	sta $dc02
	lda #$33
	sta $de03
	lda $de04
	and #$fe
	ora #$02
	sta $de04
	ldy #$0f
l24f4:  lda l24c7,y
	sta $dc00
	lda $de00
	and #$cc
	cmp l2569,y
	bne l2563
	dey
	bpl l24f4
	ldx #$fe
	stx $dc00
	lda $de01
	and #$01
	bne l2563
	inx
	stx $dc00
	lda $de01
	and #$01
	beq l2563
	lda $dc00
	and #$f7
	sta $dc00
	lda $de03
	and #$df
	ora #$10
	sta $de03
	lda $de00
	and #$ef
	sta $de00
	lda $de00
	and #$20
	bne l2563
	lda $de00
	ora #$10
	sta $de00
	lda $de00
	and #$20
	beq l2563
	lda #$00
	sta $de03
	sta $de04
	sta $dc02
	sta $dc03
	jsr PrintOK
l255f:  jsr AddLine
	rts
;
l2563:  jsr PrintBad
	jmp l255f
; ----------------------------------------------------------------------------
; 
l2569:  !byte $00, $04, $08, $0c, $40, $44, $48, $4c
	!byte $80, $84, $88, $8c, $c0, $c4, $c8, $cc
; ----------------------------------------------------------------------------
; 
l2579:	ldx #$2a
	ldy #$b2
	jsr PrintText
	lda #$80
	sta $dc0e
	sta $dc0f
	lda #$ff
	sta $dc05
	sta $dc04
	sta $dc07
	sta $dc06
	lda #$81
	sta $dc0e
	sta $dc0f
	lda #$04
	jsr l28e0
	lda #$80
	sta $dc0e
	sta $dc0f
	lda $dc05
	cmp #$ff
	beq l25df
	lda $dc07
	cmp #$ff
	beq l25df
	cmp $dc05
	bne l25df
	lda $dc04
	cmp #$ff
	beq l25df
	lda $dc06
	cmp #$ff
	beq l25df
	sec
	sbc $dc04
	bcs l25d4
	adc #$04
l25d4:  cmp #$02
	bcs l25df
	jsr PrintOK
l25db:  jsr AddLine
	rts
; 
l25df:  jsr PrintBad
	jmp l25db
; ----------------------------------------------------------------------------
; 
l25e5:  ldx #$2a
	ldy #$c3
	jsr PrintText
	lda #$a8
	ldx #$26
	stx $0301
	stx $0303
	sta $0300
	sta $0302
	lda #$bc
	ldx #$26
	stx $0305
	sta $0304
	lda $de06
	and #$fd
	ora #$31
	sta $de06
	lda $de05
	ora #$04
	sta $de05
	lda #$01
	sta $07
	lda #$0e
	sta temp2
	ldy #$04
	jsr l2665
	ldy #$06
	asl $07
	inc temp2
	jsr l2665
	asl $07
	lda #$00
	sta $dc0f
	ldx #$01
	jsr l2656
	lda #$80
	sta $dc0f
	ldx #$04
	jsr l2656
	jsr l267e
	lda #$02
	jsr l28e0
	jsr l268b
	jsr PrintOK
	jsr AddLine
	rts
; ----------------------------------------------------------------------------
; 
l2656:  lda #$00
	sta $dc0b
	sta $dc0a
	sta $dc09
	stx $dc08
	rts
; ----------------------------------------------------------------------------
; 
l2665:  lda #$ff
	sta $dc00,y
	iny
	lda #$02
	sta $dc00,y
	jsr l267e
	ldy temp2
	lda #$89
	sta $dc00,y
	jsr l268b
	rts
; ----------------------------------------------------------------------------
; 
l267e:  lda $07
	ora #$80
	sta $dc0d
	lda #$00
	sta $06
	cli
	rts
; ----------------------------------------------------------------------------
; 
l268b:  lda $dc0d
	and $07
	beq l268b
	lda #$00
	sta $dc0d
	sei
	lda $06
	beq l269e
	clc
	rts
; ----------------------------------------------------------------------------
; 
l269e:  pla
	pla
	jsr PrintBad
	jsr AddLine
	sec
	rts
; ----------------------------------------------------------------------------
; 
	lda $de07
	dec $06
	tsx
	lda $0104,x
	ora #$04
	sta $0104,x
	pla
	tay
	pla
	tax
	pla
	rti
	dec $06
	rti
l26bf:  lda #$01
	sta $03
l26c3:  ldx #$2a
	ldy #$a1
	jsr PrintText
	ldy $03
	lda Number,y
	and #$bf
	ldy #$0e
	sta (pointer_screen),y
	jsr l26e1
	inc $03
	lda $03
	cmp $02
	bne l26c3
	rts
; ----------------------------------------------------------------------------
; 
l26e1:  lda #$00
	sta pointer1
	sta pointer1+1
	lda $03
	sta IndirectBank
	ldy #$02
l26ed:  lda #$55
	sta (pointer1),y
	lda (pointer1),y
	eor #$55
	bne l2732
	lda #$aa
	sta (pointer1),y
	lda (pointer1),y
	eor #$aa
	bne l2732
	tya
	clc
	adc pointer1+1
	sta (pointer1),y
l2707:  iny
	bne l26ed
	inc pointer1+1
	bne l26ed
	lda #$00
	sta pointer1+1
	ldy #$02
l2714:  tya
	clc
	adc pointer1+1
	sta temp2
	lda (pointer1),y
	eor temp2
	bne l2737
l2720:  iny
	bne l2714
	inc pointer1+1
	bne l2714
	lda #$0f
	sta IndirectBank
	jsr PrintOK
	jsr AddLine
	rts
; ----------------------------------------------------------------------------
; 
l2732:  jsr l273c
	beq l2707
l2737:  jsr l273c
	beq l2720
l273c:  sty pointer1
	sta temp1
	lda #$0f
	sta IndirectBank
	jsr l2835
	jsr l28f0
	ldy pointer1
	lda $03
	sta IndirectBank
	lda #$00
	sta pointer1
	rts
; ----------------------------------------------------------------------------
; 
	lda $03
	sta IndirectBank
	ldy #$00
l275b:  clc
	adc #$01
	sta (pointer1),y
	jmp l275b
l2763:  ldx #$2a
	ldy #$7f
	jsr PrintText
	ldy #$14
l276c:  lda l27db,y
	sta $da00,y
	dey
	bpl l276c
	lda #$0f
	sta $da18
	lda #$11
	jsr l27c1
	lda #$21
	jsr l27c1
	lda #$41
	jsr l27c1
	lda #$01
	sta $da17
	lda #$2f
	sta $da18
	lda #$00
	sta $da05
	lda #$f0
	sta $da06
	lda #$81
	sta $da04
	ldx #$00
l27a4:  ldy #$00
l27a6:  sty $da15
	pha
	pla
	pha
	pla
	pha
	pla
	iny
	cpy #$80
	bne l27a6
	stx $da16
	inx
	bne l27a4
	stx $da18
	jsr AddLine
	rts
; ----------------------------------------------------------------------------
; 
l27c1:  sta $da04
	sta $da06
	sta $da12
	lda #$01
	jsr l28e0
	lda #$00
	sta $da04
	sta $da06
	sta $da12
	rts
; ----------------------------------------------------------------------------
; 
l27db:  !byte $1c, $d6, $ff, $00, $10, $09, $00, $24
	!byte $55, $ff, $00, $10, $09, $00, $2b
; ----------------------------------------------------------------------------
; 
l27ea:  !byte $34, $ff, $00, $10, $09, $00
; ----------------------------------------------------------------------------
; print text 16 chars
PrintText:
	stx pointer_text+1				; store text address to pointer
	sty pointer_text
	ldy #$10
prtlp:	lda (pointer_text),y
	and #$bf
	sta (pointer_screen),y
	dey
	bpl prtlp
	rts
; ----------------------------------------------------------------------------
;  print text with length in a
PrintTexta:
	stx pointer_text+1
	sty pointer_text
	tay
l2805:  lda (pointer_text),y
	and #$bf
	sta (pointer_screen2),y
	dey
	bpl l2805
	rts
; ----------------------------------------------------------------------------
; print ok
PrintOK:
	lda #$10
	jsr AddChars			; add 16 chars for ok
	ldy #$03
proklp:	lda TextOK,y
	and #$bf
	sta (pointer_screen2),y
	dey
	bne proklp
	rts
; ----------------------------------------------------------------------------
; print bad
PrintBad:
	lda #$10
	jsr AddChars			; add 16 chars for bad
	ldy #$03
prbadlp:lda TextBad,y
	and #$bf
	ora #$80			; inverse
	sta (pointer_screen2),y
	dey
	bne prbadlp
	rts
; ----------------------------------------------------------------------------
; 
l2835:  jsr l2872
	jsr PrintBad
	lda #$18
	jsr AddChars
	ldx #$2a
	ldy #$e5
	lda #$08
	jsr PrintTexta
	lda #$23
	jsr AddChars
	ldx #$00
	ldy #$10
	lda #$07
	jsr PrintTexta
	rts
; ----------------------------------------------------------------------------
; add line to screen pointer
AddLine:
	clc
	lda pointer_screen
	adc #80
	sta pointer_screen
	lda pointer_screen+1
	adc #$00
	sta pointer_screen+1
	rts
; ----------------------------------------------------------------------------
;  add a chars to screen pointer and save to screen pointer2
AddChars:
	clc
	adc pointer_screen
	sta pointer_screen2
	lda pointer_screen+1
	adc #$00
	sta pointer_screen2+1
	rts
; ----------------------------------------------------------------------------
; 
l2872:  ldy #$07
	ldx temp1
l2876:  txa
	lsr
	tax
	bcs l2880
	lda Number
	bcc l2883
l2880:  lda Number+1
l2883:  and #$bf
	sta $0010,y
	dey
	bpl l2876
	rts
; ----------------------------------------------------------------------------
; print rom checksum
PrintRomChecksum:
	pha				; remember byte
	lsr				; isolate high nibble
	lsr
	lsr
	lsr
	ldy #$00
	jsr prnibbl			; print nibble
	pla
	and #$0f			; isolate low nibble
prnibbl:clc
	adc #$f6			; calculate screen code a-f
	bcs alpha			; greater 9 -> a-f
	adc #$39			; calculate screen code 0-9
alpha:	adc #$00
	and #$bf
	sta (pointer_screen2),y		; print to screen
	iny				; next position
	rts
; ----------------------------------------------------------------------------
; 
l28a8:  ldx #$07
l28aa:  inc $d060,x
	lda $d060,x
	and #$7f
	cmp #$3a
	bcc l28c0
	lda #$b0
	sta $d060,x
	dex
	bpl l28aa
	bmi l28a8
l28c0:  ldx #$00
	lda #$20
l28c4:  sta $d0a0,x
	sta $d100,x
	sta $d200,x
	sta $d300,x
	sta $d400,x
	sta $d500,x
	sta $d600,x
	sta $d700,x
	inx
	bne l28c4
	rts
; ----------------------------------------------------------------------------
; 
l28e0:  ldx #$ff
	ldy #$ff
l28e4:  dex
	bne l28e4
	dey
	bne l28e4
	sec
	sbc #$01
	bne l28e4
	rts
; ----------------------------------------------------------------------------
; 
l28f0:  lda #$2d
	jsr AddChars
	ldx #$2a
	ldy #$d4
	lda #$08
	jsr PrintTexta
	lda #$37
	jsr AddChars
	lda pointer1+1
	jsr PrintRomChecksum
	lda #$39
	jsr AddChars
	lda pointer1
	jsr PrintRomChecksum
	rts
; ************************************* ZONE TABLES ***********************************************
!zone tables
; messages
TitleHP256:	!scr " COMMODORE CBM 700 (256K) DIAGNOSTIC"

TitleHP128:	!scr " COMMODORE CBM 700 (128K) DIAGNOSTIC"

TitleLP256:	!scr " COMMODORE CBM 600 (256K) DIAGNOSTIC"

TitleLP128:	!scr " COMMODORE CBM 600 (128K) DIAGNOSTIC"

TextCycles:	!scr "  CYCLE "

Text000001:	!scr "  000001"

TextZeropage:	!scr " ZEROPAGE        "
		!scr " STACKPAGE       "
!ifdef STATICFULL{
TextStaticRam:	!scr " STATIC RAM 2KB  "	; enhanced full 2kB test
} else{
TextStaticRam:	!scr " STATIC RAM      "
}
TextVideoRam:	!scr " VIDEO  RAM      "
TextBasicRomL:	!scr " BASIC  ROM (L)  "
TextBasicRomH:	!scr " BASIC  ROM (H)  "
TextKernalRom:	!scr " KERNAL ROM      "
		!scr " KEYBOARD        "
		!scr " IEEE PORT       "
		!scr " USER PORT       "
		!scr " RS-232          "
		!scr " CASSETTE        "
		!scr " SOUND CHIP      "
		!scr " VDC   CHIP      "
		!scr " DRAM SEGMENT    "
		!scr " TIMERS          "
		!scr " INTERRUPT       "
		!scr " ADDRESS         "
		!scr " DATABITS:       "

TextOK:	!scr " OK "

TextBad:!scr " BAD"

Number:	!scr "0123456789ABCDEF"

	!byte $aa, $aa
; ----------------------------------------------------------------------------
; end of cart
*= $3fff
	!byte $ff
