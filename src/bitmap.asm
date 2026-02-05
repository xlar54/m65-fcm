
;=======================================================================================
; init_bitmap - Fill screen RAM with sequential codes for bitmap mode
; 40-col: 1000 positions (40×25), 80-col: 2750 positions (80×25 + TEXTYPOS overflow)
;=======================================================================================
init_bitmap:
        lda #<SCREEN_RAM
        sta PTR
        lda #>SCREEN_RAM
        sta PTR+1
        lda #`SCREEN_RAM
        sta PTR+2
        lda #0
        sta PTR+3
        
        lda #<CHAR_CODE_BASE
        sta _ib_code
        lda #>CHAR_CODE_BASE
        sta _ib_code+1
        
        ; Set count based on screen mode
        ldx #0
        lda screen_mode
        cmp #80
        bne +
        ldx #2
+       lda _ib_counts,x
        sta _ib_cnt
        lda _ib_counts+1,x
        sta _ib_cnt+1
        
_ib_loop:
        ldz #0
        lda _ib_code
        sta [PTR],z
        inz
        lda _ib_code+1
        sta [PTR],z
        
        inc _ib_code
        bne +
        inc _ib_code+1
+
        clc
        lda PTR
        adc #2
        sta PTR
        bcc +
        inc PTR+1
        bne +
        inc PTR+2
+
        lda _ib_cnt
        bne +
        dec _ib_cnt+1
+       dec _ib_cnt
        lda _ib_cnt
        ora _ib_cnt+1
        bne _ib_loop
        
        rts

_ib_code:   .word 0
_ib_cnt:    .word 0
_ib_counts: .word 1000, 3000    ; 40-col, 80-col

;=======================================================================================
; clear_bitmap - Clear all pixel data to a single color
; 40-col: 250×256 bytes, 80-col: 500×256 bytes
;=======================================================================================
clear_bitmap:
        sta _cb_fill

        lda screen_mode
        cmp #80
        beq _cb_80col

        ; 40-col: single DMA, 64000 bytes at $40000
        lda _cb_fill
        sta _cb_40_val
        lda #$00
        sta $D707
        .byte $80, $00          ; source MB = $00
        .byte $81, $00          ; dest MB
        .byte $00               ; end options
        .byte $03               ; fill
        .word 64000             ; count
_cb_40_val:
        .byte $00, $00          ; fill value (self-modified)
        .byte $00               ; src bank
        .word $0000             ; dest addr
        .byte $04               ; dest bank ($40000)
        .byte $00               ; cmd high
        .word $0000             ; modulo
        rts

_cb_80col:
        ; 80-col: 128000 bytes = 65536 + 62464 (banks 4+5 only)

        ; DMA 1: 65536 bytes at $40000
        lda _cb_fill
        sta _cb_80_val1
        lda #$00
        sta $D707
        .byte $81, $00
        .byte $00
        .byte $03
        .word $0000             ; count 0 = 65536
_cb_80_val1:
        .byte $00, $00
        .byte $00
        .word $0000
        .byte $04               ; bank 4
        .byte $00
        .word $0000

        ; DMA 2: 62464 bytes at $50000 (128000 - 65536)
        lda _cb_fill
        sta _cb_80_val2
        lda #$00
        sta $D707
        .byte $81, $00
        .byte $00
        .byte $03
        .word 62464
_cb_80_val2:
        .byte $00, $00
        .byte $00
        .word $0000
        .byte $05               ; bank 5
        .byte $00
        .word $0000
        rts

_cb_fill: .byte 0


;=======================================================================================
; clear_color_ram - Clear color RAM for bitmap mode
;=======================================================================================
clear_color_ram:

        ; Self-modify counts based on mode
        ldx #0
        lda screen_mode
        cmp #80
        bne +
        ldx #2
+       lda _ccr_byte_counts,x
        sta _ccr_dma1_cnt
        lda _ccr_byte_counts+1,x
        sta _ccr_dma1_cnt+1
        lda _ccr_pos_counts,x
        sta _ccr_dma2_cnt
        lda _ccr_pos_counts+1,x
        sta _ccr_dma2_cnt+1

        ; DMA 1: Fill all bytes with $00 (clears NCM/GOTO bits)
        lda #$00
        sta $D707
        .byte $81, $FF          ; dest MB
        .byte $00               ; end options
        .byte $03               ; fill command
_ccr_dma1_cnt:
        .word 5500              ; count (self-modified)
        .byte $00, $00          ; fill value = $00
        .byte $00               ; src bank
        .word $0000             ; dest = $FF80000
        .byte $08               ; dest bank
        .byte $00               ; cmd high
        .word $0000             ; modulo

        ; DMA 2: Fill odd bytes with $01 (foreground), step=2
        lda #$00
        sta $D707
        .byte $81, $FF          ; dest MB
        .byte $85, $02          ; dest step integer = 2
        .byte $84, $00          ; dest step fraction = 0
        .byte $00               ; end options
        .byte $03               ; fill command
_ccr_dma2_cnt:
        .word 2750              ; count (self-modified)
        .byte $01, $00          ; fill value = $01
        .byte $00               ; src bank
        .word $0001             ; dest = $FF80001 (odd bytes)
        .byte $08               ; dest bank
        .byte $00               ; cmd high
        .word $0000             ; modulo
        rts

_ccr_byte_counts: .word 2000, 5500     ; total bytes: 40-col, 80-col
_ccr_pos_counts:  .word 1000, 2750     ; positions: 40-col, 80-col


;=======================================================================================
; plot_pixel - Draw a pixel at x,y with color
; Input:  plot_x (16-bit), plot_y (8-bit), plot_col (8-bit)
;=======================================================================================
plot_x:     .word 0
plot_y:     .byte 0
plot_col:   .byte 0

plot_pixel:
        ; char_col = x / 8
        lda plot_x
        sta _pp_char_col
        lda plot_x+1
        sta _pp_char_col+1
        
        lsr _pp_char_col+1
        ror _pp_char_col
        lsr _pp_char_col+1
        ror _pp_char_col
        lsr _pp_char_col+1
        ror _pp_char_col
        
        ; char_row = y / 8
        lda plot_y
        lsr
        lsr
        lsr
        sta _pp_char_row
        
        ; pixel_x = x AND 7
        lda plot_x
        and #$07
        sta _pp_pixel_x
        
        ; pixel_y = y AND 7
        lda plot_y
        and #$07
        sta _pp_pixel_y
        
        ; char_index = char_row × columns + char_col
        ; Start with char_row × 8
        lda _pp_char_row
        sta _pp_tmp
        lda #0
        sta _pp_tmp+1
        
        asl _pp_tmp
        rol _pp_tmp+1
        asl _pp_tmp
        rol _pp_tmp+1
        asl _pp_tmp
        rol _pp_tmp+1           ; × 8
        
        lda _pp_tmp
        sta _pp_tmp2
        lda _pp_tmp+1
        sta _pp_tmp2+1          ; Save × 8
        
        asl _pp_tmp
        rol _pp_tmp+1           ; × 16
        
        lda screen_mode
        cmp #80
        beq _pp_80col
        
        ; 40-col: × 40 = × 32 + × 8
        asl _pp_tmp
        rol _pp_tmp+1           ; × 32
        jmp _pp_add_base
        
_pp_80col:
        ; 80-col: × 80 = × 64 + × 16
        lda _pp_tmp
        sta _pp_tmp2
        lda _pp_tmp+1
        sta _pp_tmp2+1          ; Save × 16
        
        asl _pp_tmp
        rol _pp_tmp+1
        asl _pp_tmp
        rol _pp_tmp+1           ; × 64

_pp_add_base:
        ; Add saved value (× 8 for 40-col, × 16 for 80-col)
        clc
        lda _pp_tmp
        adc _pp_tmp2
        sta _pp_tmp
        lda _pp_tmp+1
        adc _pp_tmp2+1
        sta _pp_tmp+1

        ; Add char_col
        clc
        lda _pp_tmp
        adc _pp_char_col
        sta _pp_char_idx
        lda _pp_tmp+1
        adc _pp_char_col+1
        sta _pp_char_idx+1
        
        ; char_base = CHAR_DATA + char_index × 64
        lda _pp_char_idx
        sta _pp_tmp
        lda _pp_char_idx+1
        sta _pp_tmp+1
        lda #0
        sta _pp_tmp+2
        
        ; × 64
        asl _pp_tmp
        rol _pp_tmp+1
        rol _pp_tmp+2
        asl _pp_tmp
        rol _pp_tmp+1
        rol _pp_tmp+2
        asl _pp_tmp
        rol _pp_tmp+1
        rol _pp_tmp+2
        asl _pp_tmp
        rol _pp_tmp+1
        rol _pp_tmp+2
        asl _pp_tmp
        rol _pp_tmp+1
        rol _pp_tmp+2
        asl _pp_tmp
        rol _pp_tmp+1
        rol _pp_tmp+2
        
        ; Add CHAR_DATA base
        clc
        lda _pp_tmp
        adc #<CHAR_DATA
        sta PTR
        lda _pp_tmp+1
        adc #>CHAR_DATA
        sta PTR+1
        lda _pp_tmp+2
        adc #`CHAR_DATA
        sta PTR+2
        lda #0
        sta PTR+3
        
        ; pixel_offset = pixel_y × 8 + pixel_x
        lda _pp_pixel_y
        asl
        asl
        asl
        clc
        adc _pp_pixel_x
        taz
        
        ; Write pixel
        lda plot_col
        sta [PTR],z
        
        rts

_pp_char_col:   .word 0
_pp_char_row:   .byte 0
_pp_pixel_x:    .byte 0
_pp_pixel_y:    .byte 0
_pp_char_idx:   .word 0
_pp_tmp:        .byte 0, 0, 0
_pp_tmp2:       .word 0


;=======================================================================================
; draw_line - Bresenham line drawing (same for both modes)
;=======================================================================================
line_x0:    .word 0
line_y0:    .byte 0
line_x1:    .word 0
line_y1:    .byte 0
line_col:   .byte 0

draw_line:
        sec
        lda line_x1
        sbc line_x0
        sta _ln_dx
        lda line_x1+1
        sbc line_x0+1
        sta _ln_dx+1
        bpl _ln_dx_pos
        
        lda #$FF
        sta _ln_sx
        sec
        lda #0
        sbc _ln_dx
        sta _ln_dx
        lda #0
        sbc _ln_dx+1
        sta _ln_dx+1
        jmp _ln_do_dy
        
_ln_dx_pos:
        lda #1
        sta _ln_sx

_ln_do_dy:
        lda #0
        sta _ln_dy+1
        
        lda line_y1
        cmp line_y0
        bcs _ln_dy_pos
        
        lda #$FF
        sta _ln_sy
        sec
        lda line_y0
        sbc line_y1
        sta _ln_dy
        jmp _ln_setup
        
_ln_dy_pos:
        lda #1
        sta _ln_sy
        sec
        lda line_y1
        sbc line_y0
        sta _ln_dy

_ln_setup:
        lda line_x0
        sta _ln_x
        lda line_x0+1
        sta _ln_x+1
        lda line_y0
        sta _ln_y
        
        ; steps = max(dx, dy) + 1
        lda _ln_dx+1
        bne _ln_dx_bigger
        lda _ln_dy
        cmp _ln_dx
        bcc _ln_dx_bigger
        beq _ln_dx_bigger
        
        lda _ln_dy
        sta _ln_steps
        lda #0
        sta _ln_steps+1
        jmp _ln_init_err
        
_ln_dx_bigger:
        lda _ln_dx
        sta _ln_steps
        lda _ln_dx+1
        sta _ln_steps+1

_ln_init_err:
        inc _ln_steps
        bne _ln_err_setup
        inc _ln_steps+1

_ln_err_setup:
        sec
        lda _ln_dx
        sbc _ln_dy
        sta _ln_err
        lda _ln_dx+1
        sbc _ln_dy+1
        sta _ln_err+1

_ln_loop:
        lda _ln_x
        sta plot_x
        lda _ln_x+1
        sta plot_x+1
        lda _ln_y
        sta plot_y
        lda line_col
        sta plot_col
        jsr plot_pixel

        lda _ln_steps
        bne _ln_dec_steps
        dec _ln_steps+1
_ln_dec_steps:
        dec _ln_steps
        
        lda _ln_steps
        ora _ln_steps+1
        beq _ln_done

        lda _ln_err
        asl
        sta _ln_e2
        lda _ln_err+1
        rol
        sta _ln_e2+1

        clc
        lda _ln_e2
        adc _ln_dy
        sta _ln_tmp
        lda _ln_e2+1
        adc _ln_dy+1
        bmi _ln_skip_x
        ora _ln_tmp
        beq _ln_skip_x
        
        sec
        lda _ln_err
        sbc _ln_dy
        sta _ln_err
        lda _ln_err+1
        sbc _ln_dy+1
        sta _ln_err+1
        
        lda _ln_sx
        bmi _ln_x_dec
        inc _ln_x
        bne _ln_skip_x
        inc _ln_x+1
        jmp _ln_skip_x
_ln_x_dec:
        lda _ln_x
        bne _ln_x_dec2
        dec _ln_x+1
_ln_x_dec2:
        dec _ln_x

_ln_skip_x:
        sec
        lda _ln_dx
        sbc _ln_e2
        sta _ln_tmp
        lda _ln_dx+1
        sbc _ln_e2+1
        bmi _ln_loop
        ora _ln_tmp
        beq _ln_loop
        
        clc
        lda _ln_err
        adc _ln_dx
        sta _ln_err
        lda _ln_err+1
        adc _ln_dx+1
        sta _ln_err+1
        
        lda _ln_sy
        bmi _ln_y_dec
        inc _ln_y
        jmp _ln_loop
_ln_y_dec:
        dec _ln_y
        jmp _ln_loop

_ln_done:
        rts

_ln_e2:     .word 0
_ln_tmp:    .word 0
_ln_dx:     .word 0
_ln_dy:     .word 0
_ln_sx:     .byte 0
_ln_sy:     .byte 0
_ln_err:    .word 0
_ln_x:      .word 0
_ln_y:      .byte 0
_ln_steps:  .word 0

