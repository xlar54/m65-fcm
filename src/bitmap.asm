
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
;        sta _cb_color
        
;        lda #<CHAR_DATA
;        sta PTR
;        lda #>CHAR_DATA
;        sta PTR+1
;        lda #`CHAR_DATA
;        sta PTR+2
;        lda #0
;        sta PTR+3
        
        ; Set iteration count based on mode
;        ldx #0
;        lda screen_mode
;        cmp #80
;        bne +
;        ldx #2
;+       lda _clear_counts,x
;        sta _cb_outer
;        lda _clear_counts+1,x
;        sta _cb_outer+1
        
;_cb_outer_loop:
;        ldz #0
;_cb_inner_loop:
;        lda _cb_color
;        sta [PTR],z
;        inz
;        bne _cb_inner_loop
        
;       inc PTR+1
;        bne +
;        inc PTR+2
;+
;        lda _cb_outer
;        bne +
;        dec _cb_outer+1
;+       dec _cb_outer
;        lda _cb_outer
;        ora _cb_outer+1
;        bne _cb_outer_loop
        
;        rts

;_cb_color:      .byte 0
;_cb_outer:      .word 0
;_clear_counts:  .word 250, 688          ; 40-col: 250×256=64KB, 80-col: 688×256=176KB


; DMA works, but returning to basic causes cursor to disappear!

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
;        lda #$00
;        sta PTR
;        lda #$00
;        sta PTR+1
;        lda #$F8
;        sta PTR+2
;        lda #$0F
;        sta PTR+3

        ; Always clear 2750 positions (5500 bytes) for 80-col
        ; or 1000 for 40-col
;        ldx #0
;        lda screen_mode
;        cmp #80
;        bne +
;        ldx #2
;+       lda _ccr_counts,x
;        sta _ccr_cnt
;        lda _ccr_counts+1,x
;        sta _ccr_cnt+1

;_ccr_loop:
;        ldz #0
;        lda #$00
;        sta [PTR],z

;        inc PTR
;        bne +
;        inc PTR+1
;        bne +
;        inc PTR+2
;+
;        lda #$01
;        sta [PTR],z

;        inc PTR
;        bne +
;        inc PTR+1
;        bne +
;        inc PTR+2
;+
;        lda _ccr_cnt
;        bne +
;        dec _ccr_cnt+1
;+       dec _ccr_cnt
;        lda _ccr_cnt
;        ora _ccr_cnt+1
;        bne _ccr_loop
;        rts

;_ccr_cnt: .word 0
;_ccr_counts: .word 1000, 2750    ; 40-col, 80-col

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


;=======================================================================================
; draw_rect - Draw rectangle (outline or filled)
;=======================================================================================
rect_x:     .word 0
rect_y:     .byte 0
rect_w:     .word 0
rect_h:     .byte 0
rect_col:   .byte 0

draw_rect:
        lda #0
        rol
        sta _rc_fill

        clc
        lda rect_x
        adc rect_w
        sta _rc_x1
        lda rect_x+1
        adc rect_w+1
        sta _rc_x1+1
        
        lda _rc_x1
        bne +
        dec _rc_x1+1
+       dec _rc_x1

        clc
        lda rect_y
        adc rect_h
        sec
        sbc #1
        sta _rc_y1

        lda rect_col
        sta line_col

        lda _rc_fill
        bne _rc_do_fill

        ; Top line
        lda rect_x
        sta line_x0
        lda rect_x+1
        sta line_x0+1
        lda rect_y
        sta line_y0
        lda _rc_x1
        sta line_x1
        lda _rc_x1+1
        sta line_x1+1
        lda rect_y
        sta line_y1
        jsr draw_line

        ; Bottom line
        lda rect_x
        sta line_x0
        lda rect_x+1
        sta line_x0+1
        lda _rc_y1
        sta line_y0
        lda _rc_x1
        sta line_x1
        lda _rc_x1+1
        sta line_x1+1
        lda _rc_y1
        sta line_y1
        jsr draw_line

        ; Left line
        lda rect_x
        sta line_x0
        sta line_x1
        lda rect_x+1
        sta line_x0+1
        sta line_x1+1
        lda rect_y
        sta line_y0
        lda _rc_y1
        sta line_y1
        jsr draw_line

        ; Right line
        lda _rc_x1
        sta line_x0
        sta line_x1
        lda _rc_x1+1
        sta line_x0+1
        sta line_x1+1
        lda rect_y
        sta line_y0
        lda _rc_y1
        sta line_y1
        jsr draw_line

        rts

_rc_do_fill:
        lda rect_x
        sta line_x0
        lda rect_x+1
        sta line_x0+1
        lda _rc_x1
        sta line_x1
        lda _rc_x1+1
        sta line_x1+1

        lda rect_y
        sta _rc_cur_y

_rc_fill_loop:
        lda _rc_cur_y
        sta line_y0
        sta line_y1
        jsr draw_line

        inc _rc_cur_y
        lda _rc_cur_y
        cmp _rc_y1
        beq _rc_fill_last
        bcc _rc_fill_loop
        rts

_rc_fill_last:
        lda _rc_cur_y
        sta line_y0
        sta line_y1
        jsr draw_line
        rts

_rc_x1:     .word 0
_rc_y1:     .byte 0
_rc_fill:   .byte 0
_rc_cur_y:  .byte 0

;=======================================================================================
; draw_circle - Draw circle (outline or filled) using midpoint algorithm
; Input: circ_cx (16-bit), circ_cy (8-bit), circ_r (8-bit), circ_col (8-bit)
;        Carry: clear=outline, set=filled
; Automatically doubles x offsets in 80-col mode for correct aspect ratio
;=======================================================================================
circ_cx:    .word 0
circ_cy:    .byte 0
circ_r:     .byte 0
circ_col:   .byte 0

draw_circle:
        lda #0
        rol
        sta _cr_fill

        lda #0
        sta _cr_x
        lda circ_r
        sta _cr_y

        sec
        lda #1
        sbc circ_r
        sta _cr_d
        lda #0
        sbc #0
        sta _cr_d+1

_cr_loop:
        lda _cr_x
        cmp _cr_y
        beq +
        bcs _cr_done
+
        ; Compute aspect-corrected horizontal offsets
        lda _cr_x
        sta _cr_xh
        lda #0
        sta _cr_xh+1
        lda _cr_y
        sta _cr_yh
        lda #0
        sta _cr_yh+1

        lda screen_mode
        cmp #80
        bne _cr_no_scale
        asl _cr_xh
        rol _cr_xh+1
        asl _cr_yh
        rol _cr_yh+1
_cr_no_scale:

        lda _cr_fill
        bne _cr_do_fill
        jsr _cr_plot_8
        jmp _cr_update

_cr_do_fill:
        jsr _cr_fill_lines

_cr_update:
        lda _cr_d+1
        bmi _cr_d_neg

        lda _cr_x
        sec
        sbc _cr_y
        sta _cr_tmp
        lda #0
        sbc #0
        sta _cr_tmp+1
        asl _cr_tmp
        rol _cr_tmp+1
        clc
        lda _cr_tmp
        adc #5
        sta _cr_tmp
        lda _cr_tmp+1
        adc #0
        sta _cr_tmp+1
        clc
        lda _cr_d
        adc _cr_tmp
        sta _cr_d
        lda _cr_d+1
        adc _cr_tmp+1
        sta _cr_d+1
        dec _cr_y
        jmp _cr_inc_x

_cr_d_neg:
        lda _cr_x
        asl
        clc
        adc #3
        sta _cr_tmp
        lda #0
        adc #0
        sta _cr_tmp+1
        clc
        lda _cr_d
        adc _cr_tmp
        sta _cr_d
        lda _cr_d+1
        adc _cr_tmp+1
        sta _cr_d+1

_cr_inc_x:
        inc _cr_x
        jmp _cr_loop

_cr_done:
        rts

;---------------------------------------------------------------------------------------
; _cr_plot_8 - Plot 8 symmetric points
; Uses _cr_xh/_cr_yh (16-bit, aspect-corrected) for horizontal offsets
; Uses _cr_x/_cr_y (8-bit) for vertical offsets
;---------------------------------------------------------------------------------------
_cr_plot_8:
        lda circ_col
        sta plot_col

        ; Point 1: (cx+xh, cy+y)
        clc
        lda circ_cx
        adc _cr_xh
        sta plot_x
        lda circ_cx+1
        adc _cr_xh+1
        sta plot_x+1
        bmi +
        clc
        lda circ_cy
        adc _cr_y
        bcs +
        cmp #200
        bcs +
        sta plot_y
        jsr plot_pixel
+
        ; Point 2: (cx-xh, cy+y)
        sec
        lda circ_cx
        sbc _cr_xh
        sta plot_x
        lda circ_cx+1
        sbc _cr_xh+1
        sta plot_x+1
        bmi +
        clc
        lda circ_cy
        adc _cr_y
        bcs +
        cmp #200
        bcs +
        sta plot_y
        jsr plot_pixel
+
        ; Point 3: (cx+xh, cy-y)
        clc
        lda circ_cx
        adc _cr_xh
        sta plot_x
        lda circ_cx+1
        adc _cr_xh+1
        sta plot_x+1
        bmi +
        sec
        lda circ_cy
        sbc _cr_y
        bcc +
        cmp #200
        bcs +
        sta plot_y
        jsr plot_pixel
+
        ; Point 4: (cx-xh, cy-y)
        sec
        lda circ_cx
        sbc _cr_xh
        sta plot_x
        lda circ_cx+1
        sbc _cr_xh+1
        sta plot_x+1
        bmi +
        sec
        lda circ_cy
        sbc _cr_y
        bcc +
        cmp #200
        bcs +
        sta plot_y
        jsr plot_pixel
+
        ; Point 5: (cx+yh, cy+x)
        clc
        lda circ_cx
        adc _cr_yh
        sta plot_x
        lda circ_cx+1
        adc _cr_yh+1
        sta plot_x+1
        bmi +
        clc
        lda circ_cy
        adc _cr_x
        bcs +
        cmp #200
        bcs +
        sta plot_y
        jsr plot_pixel
+
        ; Point 6: (cx-yh, cy+x)
        sec
        lda circ_cx
        sbc _cr_yh
        sta plot_x
        lda circ_cx+1
        sbc _cr_yh+1
        sta plot_x+1
        bmi +
        clc
        lda circ_cy
        adc _cr_x
        bcs +
        cmp #200
        bcs +
        sta plot_y
        jsr plot_pixel
+
        ; Point 7: (cx+yh, cy-x)
        clc
        lda circ_cx
        adc _cr_yh
        sta plot_x
        lda circ_cx+1
        adc _cr_yh+1
        sta plot_x+1
        bmi +
        sec
        lda circ_cy
        sbc _cr_x
        bcc +
        cmp #200
        bcs +
        sta plot_y
        jsr plot_pixel
+
        ; Point 8: (cx-yh, cy-x)
        sec
        lda circ_cx
        sbc _cr_yh
        sta plot_x
        lda circ_cx+1
        sbc _cr_yh+1
        sta plot_x+1
        bmi +
        sec
        lda circ_cy
        sbc _cr_x
        bcc +
        cmp #200
        bcs +
        sta plot_y
        jsr plot_pixel
+
        rts

;---------------------------------------------------------------------------------------
; _cr_fill_lines - Draw 4 horizontal lines for filled circle
; Uses _cr_xh/_cr_yh for horizontal extents, _cr_x/_cr_y for vertical offsets
;---------------------------------------------------------------------------------------
_cr_fill_lines:
        lda circ_col
        sta line_col

        ; Line 1: cy+y from cx-xh to cx+xh
        clc
        lda circ_cy
        adc _cr_y
        bcs _cr_fl_skip1
        cmp #200
        bcs _cr_fl_skip1
        sta line_y0
        sta line_y1
        sec
        lda circ_cx
        sbc _cr_xh
        sta line_x0
        lda circ_cx+1
        sbc _cr_xh+1
        sta line_x0+1
        bpl +
        lda #0
        sta line_x0
        sta line_x0+1
+       clc
        lda circ_cx
        adc _cr_xh
        sta line_x1
        lda circ_cx+1
        adc _cr_xh+1
        sta line_x1+1
        bmi _cr_fl_skip1
        jsr draw_line
_cr_fl_skip1:

        ; Line 2: cy-y from cx-xh to cx+xh
        sec
        lda circ_cy
        sbc _cr_y
        bcc _cr_fl_skip2
        cmp #200
        bcs _cr_fl_skip2
        sta line_y0
        sta line_y1
        sec
        lda circ_cx
        sbc _cr_xh
        sta line_x0
        lda circ_cx+1
        sbc _cr_xh+1
        sta line_x0+1
        bpl +
        lda #0
        sta line_x0
        sta line_x0+1
+       clc
        lda circ_cx
        adc _cr_xh
        sta line_x1
        lda circ_cx+1
        adc _cr_xh+1
        sta line_x1+1
        bmi _cr_fl_skip2
        jsr draw_line
_cr_fl_skip2:

        ; Line 3: cy+x from cx-yh to cx+yh
        clc
        lda circ_cy
        adc _cr_x
        bcs _cr_fl_skip3
        cmp #200
        bcs _cr_fl_skip3
        sta line_y0
        sta line_y1
        sec
        lda circ_cx
        sbc _cr_yh
        sta line_x0
        lda circ_cx+1
        sbc _cr_yh+1
        sta line_x0+1
        bpl +
        lda #0
        sta line_x0
        sta line_x0+1
+       clc
        lda circ_cx
        adc _cr_yh
        sta line_x1
        lda circ_cx+1
        adc _cr_yh+1
        sta line_x1+1
        bmi _cr_fl_skip3
        jsr draw_line
_cr_fl_skip3:

        ; Line 4: cy-x from cx-yh to cx+yh
        sec
        lda circ_cy
        sbc _cr_x
        bcc _cr_fl_skip4
        cmp #200
        bcs _cr_fl_skip4
        sta line_y0
        sta line_y1
        sec
        lda circ_cx
        sbc _cr_yh
        sta line_x0
        lda circ_cx+1
        sbc _cr_yh+1
        sta line_x0+1
        bpl +
        lda #0
        sta line_x0
        sta line_x0+1
+       clc
        lda circ_cx
        adc _cr_yh
        sta line_x1
        lda circ_cx+1
        adc _cr_yh+1
        sta line_x1+1
        bmi _cr_fl_skip4
        jsr draw_line
_cr_fl_skip4:
        rts

;---------------------------------------------------------------------------------------
; Circle working variables
;---------------------------------------------------------------------------------------
_cr_fill:   .byte 0
_cr_x:      .byte 0
_cr_y:      .byte 0
_cr_d:      .word 0
_cr_tmp:    .word 0
_cr_xh:     .word 0             ; x offset scaled for aspect ratio
_cr_yh:     .word 0             ; y offset scaled for aspect ratio

;=======================================================================================
; draw_gradient_rect - Draw a filled rectangle with color gradient
; Input: grad_x (16-bit), grad_y (8-bit), grad_w (16-bit), grad_h (8-bit)
;        grad_r1, grad_g1, grad_b1 - start color RGB (0-15 each)
;        grad_r2, grad_g2, grad_b2 - end color RGB (0-15 each)
;        grad_pal - starting palette index
;        grad_dir - 0 = horizontal gradient, 1 = vertical gradient
; Uses only as many palette entries as unique colors needed (max 16)
; Destroys: A, X, Y, Z, PTR
;=======================================================================================
grad_x:     .word 0
grad_y:     .byte 0
grad_w:     .word 0             ; 16-bit for widths > 255
grad_h:     .byte 0
grad_r1:    .byte 0
grad_g1:    .byte 0
grad_b1:    .byte 0
grad_r2:    .byte 0
grad_g2:    .byte 0
grad_b2:    .byte 0
grad_pal:   .byte 0
grad_dir:   .byte 0

draw_gradient_rect:
        ; Pick iterations based on direction
        lda grad_dir
        bne _grd_vert_setup
        lda grad_w
        sta _grd_iterations
        lda grad_w+1
        sta _grd_iterations+1
        jmp _grd_check_zero
_grd_vert_setup:
        lda grad_h
        sta _grd_iterations
        lda #0
        sta _grd_iterations+1
_grd_check_zero:
        lda _grd_iterations
        ora _grd_iterations+1
        beq _grd_rts

        ; --- Compute absolute deltas and directions ---

        ; Red
        sec
        lda grad_r2
        sbc grad_r1
        bcs _grd_r_pos
        eor #$FF
        clc
        adc #1
        sta _grd_del_r
        lda #$FF
        sta _grd_dir_r
        jmp _grd_g_delta
_grd_r_pos:
        sta _grd_del_r
        lda #$01
        sta _grd_dir_r

_grd_g_delta:
        sec
        lda grad_g2
        sbc grad_g1
        bcs _grd_g_pos
        eor #$FF
        clc
        adc #1
        sta _grd_del_g
        lda #$FF
        sta _grd_dir_g
        jmp _grd_b_delta
_grd_g_pos:
        sta _grd_del_g
        lda #$01
        sta _grd_dir_g

_grd_b_delta:
        sec
        lda grad_b2
        sbc grad_b1
        bcs _grd_b_pos
        eor #$FF
        clc
        adc #1
        sta _grd_del_b
        lda #$FF
        sta _grd_dir_b
        jmp _grd_calc_max
_grd_b_pos:
        sta _grd_del_b
        lda #$01
        sta _grd_dir_b

_grd_calc_max:
        ; max_delta = max(del_r, del_g, del_b)
        lda _grd_del_r
        sta _grd_max_delta
        lda _grd_del_g
        cmp _grd_max_delta
        bcc +
        sta _grd_max_delta
+       lda _grd_del_b
        cmp _grd_max_delta
        bcc +
        sta _grd_max_delta
+
        ; num_colors = min(max_delta + 1, iterations)
        lda _grd_max_delta
        clc
        adc #1
        sta _grd_num_colors
        lda _grd_iterations+1
        bne _grd_nc_ok          ; iterations >= 256, always > num_colors
        lda _grd_num_colors
        cmp _grd_iterations
        bcc _grd_nc_ok
        beq _grd_nc_ok
        lda _grd_iterations     ; iterations smaller
        sta _grd_num_colors
_grd_nc_ok:

    ;=======================================================================
    ; Phase 1: Set up palette entries (only num_colors needed, max 16)
    ;=======================================================================
        lda _grd_num_colors
        sec
        sbc #1
        bne +
        lda #1
+       sta _grd_divisor

        ; Red color step
        lda _grd_del_r
        sta _grd_div_num+1
        lda #0
        sta _grd_div_num
        jsr _grd_div16x8
        lda _grd_div_num
        sta _grd_step_r
        lda _grd_div_num+1
        sta _grd_step_r+1

        ; Green color step
        lda _grd_del_g
        sta _grd_div_num+1
        lda #0
        sta _grd_div_num
        jsr _grd_div16x8
        lda _grd_div_num
        sta _grd_step_g
        lda _grd_div_num+1
        sta _grd_step_g+1

        ; Blue color step
        lda _grd_del_b
        sta _grd_div_num+1
        lda #0
        sta _grd_div_num
        jsr _grd_div16x8
        lda _grd_div_num
        sta _grd_step_b
        lda _grd_div_num+1
        sta _grd_step_b+1

        ; Init color accumulators
        lda #0
        sta _grd_acc_r
        sta _grd_acc_r+1
        sta _grd_acc_g
        sta _grd_acc_g+1
        sta _grd_acc_b
        sta _grd_acc_b+1
        sta _grd_iter
        sta _grd_iter+1

        lda grad_pal
        sta _grd_cur_pal

;--- Palette setup loop ---
_grd_pal_loop:
        ; Compute red
        lda _grd_dir_r
        bmi +
        clc
        lda grad_r1
        adc _grd_acc_r+1
        jmp _grd_pr
+       sec
        lda grad_r1
        sbc _grd_acc_r+1
_grd_pr:
        sta _grd_cur_r

        ; Compute green
        lda _grd_dir_g
        bmi +
        clc
        lda grad_g1
        adc _grd_acc_g+1
        jmp _grd_pg
+       sec
        lda grad_g1
        sbc _grd_acc_g+1
_grd_pg:
        sta _grd_cur_g

        ; Compute blue
        lda _grd_dir_b
        bmi +
        clc
        lda grad_b1
        adc _grd_acc_b+1
        jmp _grd_pb
+       sec
        lda grad_b1
        sbc _grd_acc_b+1
_grd_pb:
        sta _grd_cur_b

        ; Set palette entry
        lda _grd_cur_pal
        ldx _grd_cur_r
        ldy _grd_cur_g
        ldz _grd_cur_b
        jsr set_palette_color

        ; Advance color accumulators
        clc
        lda _grd_acc_r
        adc _grd_step_r
        sta _grd_acc_r
        lda _grd_acc_r+1
        adc _grd_step_r+1
        sta _grd_acc_r+1

        clc
        lda _grd_acc_g
        adc _grd_step_g
        sta _grd_acc_g
        lda _grd_acc_g+1
        adc _grd_step_g+1
        sta _grd_acc_g+1

        clc
        lda _grd_acc_b
        adc _grd_step_b
        sta _grd_acc_b
        lda _grd_acc_b+1
        adc _grd_step_b+1
        sta _grd_acc_b+1

        inc _grd_cur_pal
        inc _grd_iter
        lda _grd_iter
        cmp _grd_num_colors
        bne _grd_pal_loop

    ;=======================================================================
    ; Phase 2: Draw lines, mapping iterations to palette entries
    ; Uses 16÷16 division since iterations can exceed 255
    ;=======================================================================
        ; pal_step = (num_colors - 1) * 256 / max(iterations - 1, 1)
        lda _grd_num_colors
        sec
        sbc #1
        sta _grd_div_num+1
        lda #0
        sta _grd_div_num

        ; 16-bit: iterations - 1
        lda _grd_iterations
        sec
        sbc #1
        sta _grd_div16_den
        lda _grd_iterations+1
        sbc #0
        sta _grd_div16_den+1
        ; Clamp to 1 if zero
        ora _grd_div16_den
        bne +
        lda #1
        sta _grd_div16_den
+       jsr _grd_div16x16
        lda _grd_div_num
        sta _grd_step_pal
        lda _grd_div_num+1
        sta _grd_step_pal+1

        ; Init draw loop
        lda #0
        sta _grd_acc_pal
        sta _grd_acc_pal+1
        sta _grd_iter
        sta _grd_iter+1

        ; Precompute endpoints
        clc
        lda grad_x
        adc grad_w
        sta _grd_x1
        lda grad_x+1
        adc grad_w+1
        sta _grd_x1+1
        lda _grd_x1
        bne +
        dec _grd_x1+1
+       dec _grd_x1             ; x1 = x + w - 1

        clc
        lda grad_y
        adc grad_h
        sec
        sbc #1
        sta _grd_y1             ; y1 = y + h - 1

;--- Draw loop ---
_grd_draw_loop:
        ; palette entry = grad_pal + acc_pal.hi
        clc
        lda grad_pal
        adc _grd_acc_pal+1
        sta line_col

        lda grad_dir
        bne _grd_draw_hline

        ; Horizontal gradient: vertical line at this column
        clc
        lda grad_x
        adc _grd_iter
        sta line_x0
        lda grad_x+1
        adc _grd_iter+1
        sta line_x0+1
        lda line_x0
        sta line_x1
        lda line_x0+1
        sta line_x1+1
        lda grad_y
        sta line_y0
        lda _grd_y1
        sta line_y1
        jmp _grd_draw_it

_grd_draw_hline:
        ; Vertical gradient: horizontal line at this row
        lda grad_x
        sta line_x0
        lda grad_x+1
        sta line_x0+1
        lda _grd_x1
        sta line_x1
        lda _grd_x1+1
        sta line_x1+1
        clc
        lda grad_y
        adc _grd_iter           ; max 200 for vertical, fits in 8-bit
        sta line_y0
        sta line_y1

_grd_draw_it:
        jsr draw_line

        ; Advance palette accumulator
        clc
        lda _grd_acc_pal
        adc _grd_step_pal
        sta _grd_acc_pal
        lda _grd_acc_pal+1
        adc _grd_step_pal+1
        sta _grd_acc_pal+1

        ; 16-bit increment and compare
        inc _grd_iter
        bne +
        inc _grd_iter+1
+       lda _grd_iter
        cmp _grd_iterations
        bne _grd_draw_loop
        lda _grd_iter+1
        cmp _grd_iterations+1
        bne _grd_draw_loop

_grd_rts:
        rts

    ;---------------------------------------------------------------------------------------
    ; 16-bit / 8-bit unsigned division
    ; Input:  _grd_div_num (16-bit), _grd_divisor (8-bit)
    ; Output: _grd_div_num (16-bit quotient)
    ;---------------------------------------------------------------------------------------
_grd_div16x8:
        lda #0
        sta _grd_div_rem
        ldx #16
-       asl _grd_div_num
        rol _grd_div_num+1
        rol _grd_div_rem
        lda _grd_div_rem
        cmp _grd_divisor
        bcc +
        sbc _grd_divisor
        sta _grd_div_rem
        inc _grd_div_num
+       dex
        bne -
        rts

    ;---------------------------------------------------------------------------------------
    ; 16-bit / 16-bit unsigned division
    ; Input:  _grd_div_num (16-bit), _grd_div16_den (16-bit)
    ; Output: _grd_div_num (16-bit quotient)
    ;---------------------------------------------------------------------------------------
_grd_div16x16:
        lda #0
        sta _grd_div_rem
        sta _grd_div_rem+1
        ldx #16
-       asl _grd_div_num
        rol _grd_div_num+1
        rol _grd_div_rem
        rol _grd_div_rem+1
        sec
        lda _grd_div_rem
        sbc _grd_div16_den
        tay
        lda _grd_div_rem+1
        sbc _grd_div16_den+1
        bcc +
        sta _grd_div_rem+1
        sty _grd_div_rem
        inc _grd_div_num
+       dex
        bne -
        rts

    ;---------------------------------------------------------------------------------------
    ; Working variables
    ;---------------------------------------------------------------------------------------
_grd_divisor:       .byte 0
_grd_iterations:    .word 0
_grd_num_colors:    .byte 0
_grd_max_delta:     .byte 0
_grd_iter:          .word 0
_grd_cur_pal:       .byte 0
_grd_x1:            .word 0
_grd_y1:            .byte 0
_grd_cur_r:         .byte 0
_grd_cur_g:         .byte 0
_grd_cur_b:         .byte 0
_grd_del_r:         .byte 0
_grd_del_g:         .byte 0
_grd_del_b:         .byte 0
_grd_dir_r:         .byte 0
_grd_dir_g:         .byte 0
_grd_dir_b:         .byte 0
_grd_step_r:        .word 0
_grd_step_g:        .word 0
_grd_step_b:        .word 0
_grd_step_pal:      .word 0
_grd_acc_r:         .word 0
_grd_acc_g:         .word 0
_grd_acc_b:         .word 0
_grd_acc_pal:       .word 0
_grd_div_num:       .word 0
_grd_div_rem:       .word 0
_grd_div16_den:     .word 0