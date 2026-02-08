;=======================================================================================
; NCM (Nibble Color Mode) Support for MEGA65
; 
; NCM is a PER-CHARACTER feature that allows 16 colors per character.
; NCM characters are 16 pixels wide x 8 pixels tall.
; Each byte contains 2 pixels (4 bits each):
; - Low nibble (bits 0-3) = left pixel
; - High nibble (bits 4-7) = right pixel
;
; To enable NCM for a character:
; - Set bit 3 of color RAM byte 1 for that screen position
; - Upper 4 bits of color RAM byte 1 = palette base (added to pixel nibble)
;
; Because NCM chars are 16 pixels wide (double normal), they visually
; occupy 2 character positions on screen, but only use 1 screen RAM entry.
;=======================================================================================

;=======================================================================================
; init_ncm - Fill screen RAM with sequential character codes
;
; Sets up screen RAM so each position points to a unique character.
; This allows each screen position to display different content.
;
; NCM40: 40 positions x 25 rows = 1000 screen entries
; NCM80: 80 positions x 25 rows = 2000 screen entries
;=======================================================================================
init_ncm:
        lda #<SCREEN_RAM
        sta PTR
        lda #>SCREEN_RAM
        sta PTR+1
        lda #`SCREEN_RAM
        sta PTR+2
        lda #0
        sta PTR+3
        
        lda #<CHAR_CODE_BASE
        sta _in_code
        lda #>CHAR_CODE_BASE
        sta _in_code+1
        
        ; Set count based on screen mode (40 or 80)
        ldx #0                  ; Default: 1000 for mode 40
        lda screen_mode
        cmp #40
        bne +
        ldx #2                  ; 2000 for mode 80
+       lda _in_counts,x
        sta _in_cnt
        lda _in_counts+1,x
        sta _in_cnt+1
        
_in_loop:
        ; Write 16-bit screen code
        ldz #0
        lda _in_code
        sta [PTR],z
        inz
        lda _in_code+1
        sta [PTR],z
        
        ; Next screen code
        inc _in_code
        bne +
        inc _in_code+1
+
        ; Advance pointer by 2
        clc
        lda PTR
        adc #2
        sta PTR
        bcc +
        inc PTR+1
        bne +
        inc PTR+2
+
        ; Decrement count
        lda _in_cnt
        bne +
        dec _in_cnt+1
+       dec _in_cnt
        lda _in_cnt
        ora _in_cnt+1
        bne _in_loop
        
        rts

_in_code:   .word 0
_in_cnt:    .word 0
_in_counts: .word 1000, 2000    ; 40-col (40x25), 80-col (80x25)

;=======================================================================================
; clear_ncm - Clear all NCM character data to a single color
;
; Each character is 64 bytes. Fills both nibbles with same color.
;
; NCM40: 1000 chars x 64 bytes = 64000 bytes
; NCM80: 2000 chars x 64 bytes = 128000 bytes
;
; Input: A = color nibble (0-15)
;=======================================================================================
clear_ncm:
        ; Expand nibble to full byte
        and #$0F
        sta _cn_nibble
        asl
        asl
        asl
        asl
        ora _cn_nibble
        sta _cn_fill

        lda screen_mode
        cmp #40                 ; ncm 80 has screen mode 40
        beq _cn_80col

        ; 40-col: 64000 bytes at $40000
        lda _cn_fill
        sta _cn_40_val
        lda #$00
        sta $D707
        .byte $80, $00          ; Source MB
        .byte $81, $00          ; Dest MB
        .byte $00               ; End options
        .byte $03               ; Fill command
        .word 64000             ; Count
_cn_40_val:
        .byte $00, $00          ; Fill value
        .byte $00               ; Source bank
        .word $0000             ; Dest address
        .byte $04               ; Dest bank ($40000)
        .byte $00               ; Command high
        .word $0000             ; Modulo
        rts

_cn_80col:
        ; 80-col: 128000 bytes = 65536 + 62464
        ; DMA 1: First 65536 bytes
        lda _cn_fill
        sta _cn_80_val1
        lda #$00
        sta $D707
        .byte $80, $00
        .byte $81, $00
        .byte $00
        .byte $03
        .word $0000             ; 0 = 65536
_cn_80_val1:
        .byte $00, $00
        .byte $00
        .word $0000
        .byte $04               ; Bank 4
        .byte $00
        .word $0000

        ; DMA 2: Remaining 62464 bytes
        lda _cn_fill
        sta _cn_80_val2
        lda #$00
        sta $D707
        .byte $80, $00
        .byte $81, $00
        .byte $00
        .byte $03
        .word 62464
_cn_80_val2:
        .byte $00, $00
        .byte $00
        .word $0000
        .byte $05               ; Bank 5
        .byte $00
        .word $0000
        rts

_cn_fill:   .byte 0
_cn_nibble: .byte 0

;=======================================================================================
; clear_color_ram_ncm - Initialize color RAM for NCM mode
;
; Sets NCM bit (bit 3) and palette base for all screen positions.
; Color RAM uses 2 bytes per position in CHR16 mode.
;
; Input: A = palette base (0-15)
;=======================================================================================
clear_color_ram_ncm:
        ; Build color byte: palette in upper nibble + NCM bit
        and #$0F
        asl
        asl
        asl
        asl
        ora #$08                ; Set NCM bit (bit 3)
        sta _ccrn_color
        
        ; Set counts based on mode
        ldx #0
        lda screen_mode
        cmp #40
        bne +
        ldx #2
+       lda _ccrn_byte_counts,x
        sta _ccrn_dma1_cnt
        lda _ccrn_byte_counts+1,x
        sta _ccrn_dma1_cnt+1
        lda _ccrn_pos_counts,x
        sta _ccrn_dma2_cnt
        lda _ccrn_pos_counts+1,x
        sta _ccrn_dma2_cnt+1

        ; DMA 1: Clear all bytes to $00
        lda #$00
        sta $D707
        .byte $81, $FF          ; Dest MB = $FF
        .byte $00               ; End options
        .byte $03               ; Fill
_ccrn_dma1_cnt:
        .word 2000              ; Count (2000 or 4000)
        .byte $00, $00          ; Fill = $00
        .byte $00               ; Source bank
        .word $0000             ; Dest = $FF80000
        .byte $08               ; Dest bank
        .byte $00
        .word $0000

        ; DMA 2: Set odd bytes with NCM bit + palette, step=2
        lda #$00
        sta $D707
        .byte $81, $FF
        .byte $85, $02          ; Dest step = 2
        .byte $84, $00          ; Step fraction = 0
        .byte $00
        .byte $03
_ccrn_dma2_cnt:
        .word 1000              ; Count (1000 or 2000)
_ccrn_color:
        .byte $08, $00          ; Fill = NCM bit + palette
        .byte $00
        .word $0001             ; Dest = $FF80001 (odd bytes)
        .byte $08
        .byte $00
        .word $0000
        rts

_ccrn_byte_counts: .word 2000, 4000     ; Total bytes
_ccrn_pos_counts:  .word 1000, 2000     ; Positions

;=======================================================================================
; create_ncm_char - Create an NCM character with specified pattern
;
; Input: A = character index (0-999 for 40-col, 0-1999 for 80-col)
;        X = low byte of pattern data address
;        Y = high byte of pattern data address
;
; Pattern data is 64 bytes (8 rows x 8 bytes per row)
; Uses PTR2 ($FA) as source pointer temporarily
;=======================================================================================
PTR2 = $FA                      ; 2-byte source pointer (zero page)

create_ncm_char:
        ; Save character index and source address
        sta _cnc_char_idx
        stx PTR2                ; Source low byte to zero page
        sty PTR2+1              ; Source high byte to zero page
        
        ; Calculate destination: CHAR_DATA + (char_index x 64)
        ; Use hardware multiplier
        lda _cnc_char_idx
        sta MULTINA
        lda #0
        sta MULTINA+1
        sta MULTINA+2
        sta MULTINA+3
        
        lda #64
        sta MULTINB
        lda #0
        sta MULTINB+1
        sta MULTINB+2
        sta MULTINB+3
        
        ; Destination = CHAR_DATA + result
        clc
        lda MULTOUT
        adc #<CHAR_DATA
        sta PTR
        lda MULTOUT+1
        adc #>CHAR_DATA
        sta PTR+1
        lda MULTOUT+2
        adc #`CHAR_DATA
        sta PTR+2
        lda #0
        sta PTR+3
        
        ; Copy 64 bytes from (PTR2),y to [PTR],z
        ldy #0
_cnc_loop:
        lda (PTR2),y            ; Load from source (zero page indirect)
        tya                     ; Y -> A
        taz                     ; A -> Z (now Z = Y = offset)
        lda (PTR2),y            ; Reload source byte (A was clobbered)
        sta [PTR],z             ; Store to destination
        iny
        cpy #64
        bne _cnc_loop
        
        rts

_cnc_char_idx: .byte 0

;=======================================================================================
; set_ncm_char - Place an NCM character at screen position
;
; Input: A = character index (which character pattern to use)
;        X = NCM column (0-19 for NCM40, 0-39 for NCM80)
;        Y = screen row (0-24)
;
; NCM characters are 16 pixels wide but use a single screen position.
; They visually overlap the next position's space.
; For NCM40: 20 NCM chars fill 320 pixels, using positions 0-19 of 40-position row
;=======================================================================================
set_ncm_char:
        sta _snc_char
        stx _snc_col
        sty _snc_row
        
        ; Calculate screen position = (row x screen_positions_per_row) + col
        ; NCM40: screen has 40 positions per row, we use positions 0-19
        ; NCM80: screen has 80 positions per row, we use positions 0-39
        
        lda _snc_row
        sta MULTINA
        lda #0
        sta MULTINA+1
        sta MULTINA+2
        sta MULTINA+3
        
        ; screen_mode x 2 = actual screen positions per row (40 or 80)
        lda screen_mode
        asl                     ; x 2
        sta MULTINB
        lda #0
        sta MULTINB+1
        sta MULTINB+2
        sta MULTINB+3
        
        ; Position = (row x positions_per_row) + col
        ; DON'T multiply col by 2 - NCM chars use single positions
        clc
        lda MULTOUT
        adc _snc_col
        sta _snc_pos
        lda MULTOUT+1
        adc #0
        sta _snc_pos+1
        
        ; Multiply by 2 for byte offset (2 bytes per screen position)
        asl _snc_pos
        rol _snc_pos+1
        
        ; Screen RAM address
        clc
        lda #<SCREEN_RAM
        adc _snc_pos
        sta PTR
        lda #>SCREEN_RAM
        adc _snc_pos+1
        sta PTR+1
        lda #`SCREEN_RAM
        adc #0
        sta PTR+2
        lda #0
        sta PTR+3
        
        ; Write screen code = CHAR_CODE_BASE + char_index
        ldz #0
        clc
        lda #<CHAR_CODE_BASE
        adc _snc_char
        sta [PTR],z
        inz
        lda #>CHAR_CODE_BASE
        adc #0
        sta [PTR],z
        
        rts

_snc_char: .byte 0
_snc_col:  .byte 0
_snc_row:  .byte 0
_snc_pos:  .word 0

;=======================================================================================
; set_ncm_palette - Set palette entry for NCM character at screen position
;
; Input: X = NCM column (0-19 for NCM40, 0-39 for NCM80)
;        Y = screen row
;        A = palette base (0-15, upper nibble of final color)
;=======================================================================================
set_ncm_palette:
        pha                     ; Save palette
        stx _snp_col
        sty _snp_row
        
        ; Calculate position (same as set_ncm_char)
        lda _snp_row
        sta MULTINA
        lda #0
        sta MULTINA+1
        sta MULTINA+2
        sta MULTINA+3
        
        ; screen_mode x 2 = actual screen positions per row
        lda screen_mode
        asl
        sta MULTINB
        lda #0
        sta MULTINB+1
        sta MULTINB+2
        sta MULTINB+3
        
        ; Position = (row x positions_per_row) + col
        clc
        lda MULTOUT
        adc _snp_col
        sta _snp_pos
        lda MULTOUT+1
        adc #0
        sta _snp_pos+1
        
        ; Multiply by 2 for byte offset
        asl _snp_pos
        rol _snp_pos+1
        
        ; Color RAM address = $FF80000 + position
        clc
        lda _snp_pos
        sta PTR
        lda _snp_pos+1
        sta PTR+1
        lda #$FF
        sta PTR+2
        lda #$08
        sta PTR+3
        
        ; Write color: byte 0 = $00, byte 1 = (palette << 4) | NCM bit
        ldz #0
        lda #$00
        sta [PTR],z
        inz
        pla                     ; Get palette
        and #$0F
        asl
        asl
        asl
        asl
        ora #$08                ; NCM bit
        sta [PTR],z
        
        rts

_snp_col:  .byte 0
_snp_row:  .byte 0
_snp_pos:  .word 0