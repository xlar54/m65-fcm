;=======================================================================================
; set_color - Change a color within a character definition
; Input:  A = character index (0=space, 1=box, 2=letter_a, etc.)
;         X = old color value to find
;         Y = new color value to replace with
; Destroys: A, Z, PTR
;=======================================================================================
set_color:
        ; Save inputs
        sta _sc_char
        stx _sc_old
        sty _sc_new
        
        ; Calculate character address: CHAR_DATA + (char_index × 64)
        lda _sc_char
        sta _sc_lo
        lda #0
        sta _sc_hi
        
        ; Shift left 6 times for × 64
        asl _sc_lo
        rol _sc_hi
        asl _sc_lo
        rol _sc_hi
        asl _sc_lo
        rol _sc_hi
        asl _sc_lo
        rol _sc_hi
        asl _sc_lo
        rol _sc_hi
        asl _sc_lo
        rol _sc_hi
        
        ; Set up pointer: CHAR_DATA + offset
        clc
        lda #<CHAR_DATA
        adc _sc_lo
        sta PTR
        lda #>CHAR_DATA
        adc _sc_hi
        sta PTR+1
        lda #`CHAR_DATA
        adc #0
        sta PTR+2
        lda #0
        sta PTR+3
        
        ; Loop through 64 bytes
        ldz #0
_sc_loop:
        lda [PTR],z
        cmp _sc_old
        bne _sc_skip
        lda _sc_new
        sta [PTR],z
_sc_skip:
        inz
        cpz #64
        bne _sc_loop
        
        rts

_sc_char: .byte 0
_sc_old:  .byte 0
_sc_new:  .byte 0
_sc_lo:   .byte 0
_sc_hi:   .byte 0

;=======================================================================================
; draw_char - Draw a character at screen position
; Input:  A = character index (0=space, 1=box, 2=letter_a, 3=block)
;         X = row (0-24)
;         Y = column (0-39 for 40-col, 0-79 for 80-col)
; Destroys: A, Z, PTR
;=======================================================================================
draw_char:
        ; Save inputs
        sta _dc_char
        sty _dc_col
        stx _dc_row
        
        ; Calculate offset = row × LINESTEP + col × 2
        ; 40-col: LINESTEP = 80  (40 columns × 2 bytes)
        ; 80-col: LINESTEP = 160 (80 columns × 2 bytes)
        
        ; Start with row in 16-bit
        lda _dc_row
        sta _dc_lo
        lda #0
        sta _dc_hi
        
        ; Shift left 4 times for × 16
        asl _dc_lo
        rol _dc_hi
        asl _dc_lo
        rol _dc_hi
        asl _dc_lo
        rol _dc_hi
        asl _dc_lo
        rol _dc_hi
        
        ; Save row × 16
        lda _dc_lo
        sta _dc_tmp
        lda _dc_hi
        sta _dc_tmp+1
        
        ; Shift left 2 more for × 64
        asl _dc_lo
        rol _dc_hi
        asl _dc_lo
        rol _dc_hi
        
        ; Now _dc_lo/hi = row × 64, _dc_tmp = row × 16
        ; Add them: row × 64 + row × 16 = row × 80
        clc
        lda _dc_lo
        adc _dc_tmp
        sta _dc_lo
        lda _dc_hi
        adc _dc_tmp+1
        sta _dc_hi
        
        ; Check if 80-column mode - if so, double it (row × 160)
        lda screen_mode
        cmp #80
        bne _dc_not_80
        
        ; Double: row × 80 → row × 160
        asl _dc_lo
        rol _dc_hi
        
_dc_not_80:
        ; Add col × 2
        lda _dc_col
        asl                     ; col × 2
        clc
        adc _dc_lo
        sta _dc_lo
        bcc +
        inc _dc_hi
+
        ; Set up 32-bit pointer: SCREEN_RAM + offset
        clc
        lda #<SCREEN_RAM
        adc _dc_lo
        sta PTR
        lda #>SCREEN_RAM
        adc _dc_hi
        sta PTR+1
        lda #`SCREEN_RAM
        adc #0
        sta PTR+2
        lda #0
        sta PTR+3
        
        ; Write screen code: CHAR_DATA/64 + char_index
        ; CHAR_DATA = $40000, so $40000/64 = $1000
        ldz #0
        lda _dc_char
        clc
        adc #<(CHAR_DATA/64)    ; low byte = $00
        sta [PTR],z
        
        inz
        lda #>(CHAR_DATA/64)    ; high byte = $14
        adc #0                  ; add carry if char index overflowed
        sta [PTR],z
        
        rts

_dc_char:   .byte 0
_dc_col:    .byte 0
_dc_row:    .byte 0
_dc_lo:     .byte 0
_dc_hi:     .byte 0
_dc_tmp:    .word 0


;=======================================================================================
; draw_petscii - Draw a PETSCII character at screen position with color
; Input:  A = PETSCII screen code (0-255)
;         X = row (0-24)
;         Y = column (0-39 for 40-col, 0-79 for 80-col)
;         Z = foreground color (0-15)
; Destroys: A, Z, PTR
;=======================================================================================
draw_petscii:
        ; Save inputs
        sta _dp_char
        sty _dp_col
        stx _dp_row
        stz _dp_color

; Default high byte (no attributes)
        lda #$00
        sta _dp_hibyte
        
        ; Convert PETSCII/ASCII to screen code
        lda _dp_char

        cmp #$40
        bcc _dp_have_code       ; $00-$3F unchanged

        beq _dp_at              ; $40 '@' -> $00

        cmp #$5B
        bcc _dp_lower_alpha     ; $41-$5A -> 1..26  (a..z)

        cmp #$60
        bcc _dp_brackets        ; $5B-$5F -> $1B-$1F

        cmp #$C1
        bcc _dp_have_code       ; $60-$C0 leave as-is (includes graphics range)

        cmp #$DB
        bcc _dp_upper_alpha     ; $C1-$DA -> 65..90 (A..Z)

        jmp _dp_have_code       ; $DB-$FF leave as-is


_dp_at:
        lda #$00
        sta _dp_char
        jmp _dp_have_code

_dp_lower_alpha:
        sec
        sbc #$40                ; $41->1, $5A->26
        sta _dp_char
        jmp _dp_have_code

_dp_upper_alpha:
        sec
        sbc #$80                ; $C1->65, $DA->90
        sta _dp_char
        jmp _dp_have_code

_dp_brackets:
        sec
        sbc #$40                ; $5B->1B ... $5F->1F
        sta _dp_char
        jmp _dp_have_code
        
_dp_have_code:
        
        ; Calculate offset = row × LINESTEP + col × 2
        lda _dp_row
        sta _dp_lo
        lda #0
        sta _dp_hi
        
        ; Shift left 4 times for × 16
        asl _dp_lo
        rol _dp_hi
        asl _dp_lo
        rol _dp_hi
        asl _dp_lo
        rol _dp_hi
        asl _dp_lo
        rol _dp_hi
        
        ; Save row × 16
        lda _dp_lo
        sta _dp_tmp
        lda _dp_hi
        sta _dp_tmp+1
        
        ; Shift left 2 more for × 64
        asl _dp_lo
        rol _dp_hi
        asl _dp_lo
        rol _dp_hi
        
        ; Add: row × 64 + row × 16 = row × 80
        clc
        lda _dp_lo
        adc _dp_tmp
        sta _dp_lo
        lda _dp_hi
        adc _dp_tmp+1
        sta _dp_hi
        
        ; Check if 80-column mode
        lda screen_mode
        cmp #80
        bne _dp_not_80
        asl _dp_lo
        rol _dp_hi
        
_dp_not_80:
        ; Add col × 2
        lda _dp_col
        asl
        clc
        adc _dp_lo
        sta _dp_lo
        bcc +
        inc _dp_hi
+
        ; Set up pointer: SCREEN_RAM + offset
        clc
        lda #<SCREEN_RAM
        adc _dp_lo
        sta PTR
        lda #>SCREEN_RAM
        adc _dp_hi
        sta PTR+1
        lda #`SCREEN_RAM
        adc #0
        sta PTR+2
        lda #0
        sta PTR+3
        
        ; Write screen code (0-255, high byte = 0 for PETSCII)
        ldz #0
        lda _dp_char
        sta [PTR],z
        inz
        lda #$00                ; high byte = 0 for PETSCII
        sta [PTR],z

        ; Set up pointer: $FF80000 + offset (color RAM)
        lda _dp_lo
        sta PTR
        lda _dp_hi
        sta PTR+1
        lda #$F8
        sta PTR+2
        lda #$0F
        sta PTR+3
        
        ; Write color bytes
        ldz #0
        lda #$00                ; Byte 0: NCM=0 for traditional charset
        sta [PTR],z
        inz
        lda _dp_color           ; Byte 1: foreground color
        sta [PTR],z
        
        rts

_dp_char:   .byte 0
_dp_col:    .byte 0
_dp_row:    .byte 0
_dp_lo:     .byte 0
_dp_hi:     .byte 0
_dp_tmp:    .word 0
_dp_color:  .byte 0
_dp_hibyte: .byte 0

;=======================================================================================
; draw_petscii_string - Draw a null-terminated string at screen position with wrapping
; Input:  str_ptr   = 16-bit pointer to null-terminated string (in bank 0)
;         str_row   = starting row (0-24)
;         str_col   = starting column (0-39 or 0-79)
;         str_color = foreground color (0-15)
; Destroys: A, X, Y, Z, PTR
;=======================================================================================
str_ptr:    .word 0
str_row:    .byte 0
str_col:    .byte 0
str_color:  .byte 0

STR_ZP = $FA                    ; 2-byte zero page pointer (NOT $FC - that's PTR!)

draw_petscii_string:
        ; Copy str_ptr to zero page for indirect addressing
        lda str_ptr
        sta STR_ZP
        lda str_ptr+1
        sta STR_ZP+1
        
        ; Copy starting position to working variables
        lda str_col
        sta _ds_cur_col
        lda str_row
        sta _ds_cur_row
        
        ; Get screen width based on mode
        lda #40
        ldx screen_mode
        cpx #80
        bne _ds_got_width
        lda #80
_ds_got_width:
        sta _ds_width
        
        ; String index
        lda #0
        sta _ds_index
        
_ds_loop:
        ; Get next character
        ldy _ds_index
        lda (STR_ZP),y
        beq _ds_done            ; Null terminator - done
        
        ; Save character
        sta _ds_char
        
        ; Set up draw_petscii parameters
        lda _ds_char            ; A = character
        ldx _ds_cur_row         ; X = row
        ldy _ds_cur_col         ; Y = column
        ldz #0
        lda str_color
        sta _ds_save_color
        ldz _ds_save_color      ; Z = color (load from ZP-accessible location)
        lda _ds_char            ; Reload A with character
        jsr draw_petscii
        
        ; Increment string index
        inc _ds_index
        
        ; Increment column
        inc _ds_cur_col
        
        ; Check for wrap
        lda _ds_cur_col
        cmp _ds_width
        bcc _ds_loop            ; Still on same line
        
        ; Wrap to next line
        lda #0
        sta _ds_cur_col
        inc _ds_cur_row
        
        ; Check for bottom of screen
        lda _ds_cur_row
        cmp #25
        bcc _ds_loop            ; Still on screen
        
_ds_done:
        rts

_ds_cur_col:    .byte 0
_ds_cur_row:    .byte 0
_ds_width:      .byte 0
_ds_index:      .byte 0
_ds_char:       .byte 0
_ds_save_color: .byte 0

;=======================================================================================
; load chars - copies chars to CHAR_DATA memory
;;=======================================================================================
load_chars:
        ; DMA copy chars to CHAR_PTR
        lda #$00
        sta $D707
        .byte $80,$00,$81,$00,$00
        .byte $00                                       ; copy                                 
        .byte <(custom_chars_end-custom_chars_start)    ; length lsb
        .byte >(custom_chars_end-custom_chars_start)    ; length msb
        .byte <custom_chars_start                       ; src lsb
        .byte >custom_chars_start                       ; src msb
        .byte $00                                       ; src bank
        .byte <CHAR_DATA, >CHAR_DATA, `CHAR_DATA        ; dest lsb, msb, bank 5
        .byte $00                                       ; command high byte
        .word $0000                                     ; modulo (ignored)

        rts

;=======================================================================================
; clear screen ram
; Used to clear the entire screen quickly, with the index byte selected in A
;=======================================================================================
clear_screen_ram:
        ; Fill screen RAM with character index in A
        ; Input: A = character index (0=space, 1=box, 2=letter_a, etc.)
        
        ; Calculate screen code: CHAR_DATA/64 + index
        clc
        adc #<(CHAR_DATA/64)
        sta _csr_lo
        lda #>(CHAR_DATA/64)
        adc #0
        sta _csr_hi

        ; Set count based on mode
        lda screen_mode
        cmp #80
        bne +
        ; 80-col: 2000 positions
        lda #<2000
        sta _csr_dma1_cnt
        sta _csr_dma2_cnt
        lda #>2000
        sta _csr_dma1_cnt+1
        sta _csr_dma2_cnt+1
        jmp _csr_go
+       ; 40-col: 1000 positions
        lda #<1000
        sta _csr_dma1_cnt
        sta _csr_dma2_cnt
        lda #>1000
        sta _csr_dma1_cnt+1
        sta _csr_dma2_cnt+1

_csr_go:
        ; DMA 1: Fill even bytes with low byte of screen code, step=2
        lda _csr_lo
        sta _csr_val1
        lda #$00
        sta $D707
        .byte $81, $00          ; dest MB
        .byte $85, $02          ; dest step integer = 2
        .byte $84, $00          ; dest step fraction = 0
        .byte $00               ; end options
        .byte $03               ; fill
_csr_dma1_cnt:
        .word 1000              ; count (self-modified)
_csr_val1:
        .byte $00, $00          ; fill value (low byte self-modified)
        .byte $00               ; src bank
        .word $0000             ; dest = $10000
        .byte $01               ; dest bank
        .byte $00               ; cmd high
        .word $0000             ; modulo

        ; DMA 2: Fill odd bytes with high byte of screen code, step=2
        lda _csr_hi
        sta _csr_val2
        lda #$00
        sta $D707
        .byte $81, $00          ; dest MB
        .byte $85, $02          ; dest step integer = 2
        .byte $84, $00          ; dest step fraction = 0
        .byte $00               ; end options
        .byte $03               ; fill
_csr_dma2_cnt:
        .word 1000              ; count (self-modified)
_csr_val2:
        .byte $00, $00          ; fill value (high byte self-modified)
        .byte $00               ; src bank
        .word $0001             ; dest = $10001
        .byte $01               ; dest bank
        .byte $00               ; cmd high
        .word $0000             ; modulo
        rts

_csr_lo: .byte 0
_csr_hi: .byte 0

;=======================================================================================
; clear_color_ram_text - Clear color RAM for text mode (NCM=0)
; Sets foreground to specified color
; Input: A = foreground color
;=======================================================================================
clear_color_ram_text:

        sta _ccrt_fill_val      ; Self-modify fill value for step 2
        
        ; Step 1: Fill all 5500 bytes at $0FF80000 with $00
        lda #$00
        sta $D707
        .byte $0b                ; use 11-byte F018 list format 
        .byte $81, $FF           ; destination address bits 20–27 (MB select) :contentReference[oaicite:5]{index=5}
        ;.byte $85, $02           ; dest skip whole = 2 bytes (every other byte) :contentReference[oaicite:7]{index=7}
        .byte $00                ; end of option list :contentReference[oaicite:8]{index=8}

        .byte $03               ; fill command
        .word 5500              ; count
        .word $0000             ; fill value = $00
        .byte $00               ; source bank
        .word $0000             ; dest = $FF80000
        .byte $08               ; dest bank
        .byte $00               ; command high
        .word $0000             ; modulo

        
        ; Step 2: Fill color at every other byte (2750 positions)
        lda #$00
        sta $D707
        .byte $0b                ; use 11-byte F018 list format :contentReference[oaicite:4]{index=4}
        .byte $81, $FF           ; destination address bits 20–27 (MB select) :contentReference[oaicite:5]{index=5}
        .byte $85, $02           ; dest skip whole = 2 bytes (every other byte) :contentReference[oaicite:7]{index=7}
        .byte $00                ; end of option list :contentReference[oaicite:8]{index=8}

        .byte $03               ; fill command
        .word 2750              ; count
_ccrt_fill_val:
        .byte $05, $00          ; fill value (low byte self-modified with color)
        .byte $00               ; source bank
        .word $0001             ; dest = $FF80001 (odd bytes)
        .byte $08               ; dest bank
        .byte $00               ; command high
        .word $0000             ; modulo

        rts



;===========================================================================================
; CUSTOM CHARACTER DEFINITIONS
;===========================================================================================

; note that the pixel value $ff will not select the corresponding colour code
; directly. Instead, it will select the colour code defined by the colour RAM
custom_chars_start:

space:
.byte $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00, $00, $00, $00

f1:
.byte $00, $00, $00, $AA, $AA, $AA, $00, $00
.byte $00, $00, $AA, $0F, $0F, $0F, $AA, $00
.byte $00, $00, $AA, $0F, $0F, $0F, $AA, $00
.byte $00, $00, $AA, $0F, $0F, $0F, $AA, $00
.byte $00, $00, $00, $AA, $0F, $AA, $00, $00
.byte $00, $00, $00, $AA, $0F, $AA, $00, $00
.byte $00, $00, $00, $AA, $0F, $AA, $00, $00
.byte $00, $00, $00, $AA, $0F, $AA, $00, $00

f2:
.byte $00, $00, $00, $AA, $0F, $AA, $00, $00
.byte $00, $00, $AA, $0F, $0F, $0F, $AA, $00
.byte $00, $AA, $0F, $0F, $0F, $0F, $0F, $AA
.byte $00, $AA, $0F, $0F, $0F, $0F, $0F, $AA
.byte $00, $AA, $0F, $0F, $0F, $0F, $0F, $AA
.byte $00, $AA, $AA, $AA, $AA, $AA, $AA, $AA
.byte $00, $00, $00, $AA, $00, $AA, $00, $00
.byte $00, $00, $00, $AA, $00, $AA, $00, $00


box:
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA
.byte $AA, $00, $00, $00, $00, $00, $00, $AA
.byte $AA, $00, $00, $00, $00, $00, $00, $AA
.byte $AA, $00, $00, $00, $00, $00, $00, $AA
.byte $AA, $00, $00, $00, $00, $00, $00, $AA
.byte $AA, $00, $00, $00, $00, $00, $00, $AA
.byte $AA, $00, $00, $00, $00, $00, $00, $AA
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA

letter_a:
.byte $00, $00, $00, $AA, $AA, $00, $00, $00
.byte $00, $AA, $AA, $00, $00, $AA, $AA, $00
.byte $00, $AA, $00, $00, $00, $00, $AA, $00
.byte $00, $AA, $AA, $AA, $AA, $AA, $AA, $00
.byte $00, $AA, $00, $00, $00, $00, $AA, $00
.byte $00, $AA, $00, $00, $00, $00, $AA, $00
.byte $00, $AA, $00, $00, $00, $00, $AA, $00
.byte $00, $00, $00, $00, $00, $00, $00, $00

block:
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA
.byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA

folder_tl:                                      ; char 6
.byte $00,$AA,$AA,$AA,$AA,$AA,$AA,$AA
.byte $AA,$A1,$A1,$A1,$A1,$A1,$A1,$A1
.byte $AA,$A1,$A1,$A1,$A1,$A1,$AA,$AA
.byte $AA,$A1,$A1,$A1,$A1,$A1,$A1,$A1
.byte $AA,$A1,$A1,$A1,$A1,$A1,$A1,$AA
.byte $AA,$A1,$A1,$A1,$A1,$A1,$A1,$AA
.byte $AA,$A1,$A1,$A1,$A1,$A1,$A1,$AA
.byte $AA,$A1,$A1,$A1,$A1,$A1,$A1,$AA

folder_tc:                                      ; char 7
.byte $00,$00,$00,$00,$00,$00,$00,$00
.byte $AA,$00,$00,$00,$00,$00,$00,$00
.byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
.byte $A1,$A1,$A1,$A1,$A1,$A1,$A1,$A1
.byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$A1
.byte $A3,$A3,$A3,$A3,$A4,$A4,$AA,$A1
.byte $A3,$A3,$A3,$A3,$A3,$A4,$AA,$A1
.byte $A3,$A3,$A3,$A3,$A3,$A3,$AA,$A1

folder_tr:                                      ; char 8
.byte $00,$00,$00,$00,$00,$00,$00,$00
.byte $00,$00,$00,$00,$00,$00,$00,$00
.byte $AA,$AA,$AA,$AA,$AA,$00,$00,$00
.byte $A1,$A1,$A1,$A1,$AA,$00,$00,$00
.byte $A1,$A1,$A1,$A1,$AA,$00,$00,$00
.byte $A1,$A1,$A1,$A1,$AA,$00,$00,$00
.byte $A1,$A1,$A1,$A1,$AA,$00,$00,$00
.byte $A1,$A1,$A1,$A1,$AA,$00,$00,$00

folder_ml:                                      ; char 9
.byte $AA,$A1,$A1,$A1,$A1,$A1,$A1,$AA
.byte $AA,$A1,$A1,$A1,$A1,$A1,$A1,$AA
.byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
.byte $AA,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $AA,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $AA,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $AA,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $AA,$A2,$A2,$A2,$A2,$A2,$A2,$A2

folder_mc:                                      ; char 10
.byte $A3,$A3,$A3,$A3,$A3,$A3,$AA,$A1
.byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$A1
.byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA
.byte $A2,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $A2,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $A2,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $A2,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $A2,$A2,$A2,$A2,$A2,$A2,$A2,$A2

folder_mr:                                      ; char 11
.byte $A1,$A1,$A1,$A1,$AA,$00,$00,$00
.byte $A1,$A1,$A1,$A1,$AA,$00,$00,$00
.byte $AA,$AA,$AA,$AA,$AA,$AA,$00,$00
.byte $A2,$A2,$A2,$A2,$A2,$AA,$00,$00
.byte $A2,$A2,$A2,$A2,$A2,$AA,$00,$00
.byte $A2,$A2,$A2,$A2,$A2,$AA,$00,$00
.byte $A2,$A2,$A2,$A2,$A2,$AA,$00,$00
.byte $A2,$A2,$A2,$A2,$A2,$AA,$00,$00

folder_bl:                                      ; char 12
.byte $AA,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $AA,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $AA,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $AA,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $AA,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $AA,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $AA,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $00,$AA,$AA,$AA,$AA,$AA,$AA,$AA

folder_bc:                                      ; char 13
.byte $A2,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $A2,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $A2,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $A2,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $A2,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $A2,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $A2,$A2,$A2,$A2,$A2,$A2,$A2,$A2
.byte $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA

folder_br:                                      ; char 14
.byte $A2,$A2,$A2,$A2,$A2,$AA,$00,$00
.byte $A2,$A2,$A2,$A2,$A2,$AA,$00,$00
.byte $A2,$A2,$A2,$A2,$A2,$AA,$00,$00
.byte $A2,$A2,$A2,$A2,$A2,$AA,$00,$00
.byte $A2,$A2,$A2,$A2,$A2,$AA,$00,$00
.byte $A2,$A2,$A2,$A2,$A2,$AA,$00,$00
.byte $A2,$A2,$A2,$A2,$A2,$AA,$00,$00
.byte $AA,$AA,$AA,$AA,$AA,$AA,$00,$00


custom_chars_end: