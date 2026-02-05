;=======================================================================================
; draw_ellipse - Draw an ellipse (outline or filled)
; Input: elip_cx (16-bit), elip_cy (8-bit), elip_rx (8-bit), elip_ry (8-bit)
;        elip_col (8-bit)
;        Carry: clear=outline, set=filled
; Automatically doubles x offsets in 80-col mode for correct aspect ratio
; Outline: 128 parametric line segments via sine table
; Filled:  scanline fill using integer square root
;=======================================================================================
elip_cx:    .word 0
elip_cy:    .byte 0
elip_rx:    .byte 0             ; X radius (in square-pixel units)
elip_ry:    .byte 0             ; Y radius
elip_col:   .byte 0

draw_ellipse:
        ; Save fill flag from carry FIRST
        lda #0
        rol
        sta _el_fill

        lda _el_fill
        bne _el_do_fill
        jsr _el_draw_outline
        rts

_el_do_fill:
        ; Filled mode needs ry > 0 to avoid divide-by-zero
        lda elip_ry
        beq _el_exit
        jsr _el_draw_filled
_el_exit:
        rts


;---------------------------------------------------------------------------------------
; _el_draw_outline - 128 parametric line segments using sine table
; Computes 128 points on the ellipse (angle step = 2) and connects them
;---------------------------------------------------------------------------------------
_el_draw_outline:
        lda elip_col
        sta line_col

        ; Compute first point at angle 0
        lda #0
        sta _el_angle
        jsr _el_compute_point

        ; Save as first point for closing the loop
        lda _el_px
        sta _el_first_x
        lda _el_px+1
        sta _el_first_x+1
        lda _el_py
        sta _el_first_y

        ; Current becomes previous
        lda _el_px
        sta _el_prev_x
        lda _el_px+1
        sta _el_prev_x+1
        lda _el_py
        sta _el_prev_y

        ; Start loop at angle 2
        lda #2
        sta _el_angle

_el_ol_loop:
        jsr _el_compute_point

        ; Draw line from previous point to current point
        lda _el_prev_x
        sta line_x0
        lda _el_prev_x+1
        sta line_x0+1
        lda _el_prev_y
        sta line_y0
        lda _el_px
        sta line_x1
        lda _el_px+1
        sta line_x1+1
        lda _el_py
        sta line_y1
        jsr draw_line

        ; Current becomes previous
        lda _el_px
        sta _el_prev_x
        lda _el_px+1
        sta _el_prev_x+1
        lda _el_py
        sta _el_prev_y

        ; Advance angle by 2 (wraps 0 at 256)
        clc
        lda _el_angle
        adc #2
        sta _el_angle
        bne _el_ol_loop

        ; Close the loop: last point back to first point
        lda _el_prev_x
        sta line_x0
        lda _el_prev_x+1
        sta line_x0+1
        lda _el_prev_y
        sta line_y0
        lda _el_first_x
        sta line_x1
        lda _el_first_x+1
        sta line_x1+1
        lda _el_first_y
        sta line_y1
        jsr draw_line
        rts


;---------------------------------------------------------------------------------------
; _el_compute_point - Compute screen coordinates for a point on the ellipse
; Input:  _el_angle (0-255, full circle)
; Output: _el_px (16-bit screen X), _el_py (8-bit screen Y, clamped 0-199)
;---------------------------------------------------------------------------------------
_el_compute_point:
        ; --- X offset: rx * cos(angle) ---
        clc
        lda _el_angle
        adc #64                 ; cos = sin(angle + 64)
        jsr _el_get_sin         ; A = |cos| (0-255), _el_sin_neg = sign

        ldx elip_rx
        jsr _el_mul8x8          ; result+1 = rx * |cos| / 256

        ; Apply 80-col aspect ratio doubling
        lda _el_mul_result+1
        sta _el_offset
        lda #0
        sta _el_offset+1

        lda screen_mode
        cmp #80
        bne +
        asl _el_offset
        rol _el_offset+1
+
        ; screen X = cx +/- offset
        lda _el_sin_neg
        bne _el_cp_x_sub

        clc
        lda elip_cx
        adc _el_offset
        sta _el_px
        lda elip_cx+1
        adc _el_offset+1
        sta _el_px+1
        jmp _el_cp_do_y

_el_cp_x_sub:
        sec
        lda elip_cx
        sbc _el_offset
        sta _el_px
        lda elip_cx+1
        sbc _el_offset+1
        sta _el_px+1
        bpl _el_cp_do_y
        lda #0
        sta _el_px
        sta _el_px+1

_el_cp_do_y:
        ; --- Y offset: ry * sin(angle) ---
        lda _el_angle
        jsr _el_get_sin

        ldx elip_ry
        jsr _el_mul8x8

        lda _el_sin_neg
        bne _el_cp_y_sub

        clc
        lda elip_cy
        adc _el_mul_result+1
        bcs _el_cp_y_clamp_hi
        cmp #200
        bcc _el_cp_y_store
_el_cp_y_clamp_hi:
        lda #199
        jmp _el_cp_y_store

_el_cp_y_sub:
        sec
        lda elip_cy
        sbc _el_mul_result+1
        bcs _el_cp_y_store
        lda #0

_el_cp_y_store:
        sta _el_py
        rts


;---------------------------------------------------------------------------------------
; _el_draw_filled - Scanline fill using integer square root
; For dy = 0 to ry:
;   half_w = rx * isqrt(ry^2 - dy^2) / ry  (with rounded sqrt for precision)
;   Draw horizontal line at cy-dy and cy+dy from cx-half_w to cx+half_w
;---------------------------------------------------------------------------------------
_el_draw_filled:
        ; Precompute ry^2
        lda elip_ry
        ldx elip_ry
        jsr _el_mul8x8
        lda _el_mul_result
        sta _el_ry2
        lda _el_mul_result+1
        sta _el_ry2+1

        lda elip_col
        sta line_col

        lda #0
        sta _el_dy

_el_fill_loop:
        ; Compute dy^2
        lda _el_dy
        ldx _el_dy
        jsr _el_mul8x8

        ; remainder = ry^2 - dy^2 (always >= 0 since dy <= ry)
        sec
        lda _el_ry2
        sbc _el_mul_result
        sta _el_sqrt_in
        lda _el_ry2+1
        sbc _el_mul_result+1
        sta _el_sqrt_in+1

        ; s = isqrt(remainder) with rounding
        jsr _el_isqrt           ; A = s (rounded to nearest)

        ; half_w = rx * s / ry
        ldx elip_rx
        jsr _el_mul8x8          ; result = rx * s (16-bit)

        lda _el_mul_result
        sta _el_div_num
        lda _el_mul_result+1
        sta _el_div_num+1
        lda elip_ry
        sta _el_divisor
        jsr _el_div16x8         ; quotient in _el_div_num

        ; Apply 80-col aspect ratio doubling
        lda screen_mode
        cmp #80
        bne +
        asl _el_div_num
        rol _el_div_num+1
+
        lda _el_div_num
        sta _el_half_w
        lda _el_div_num+1
        sta _el_half_w+1

        ; Precompute x0 = cx - half_w (clamped >= 0)
        sec
        lda elip_cx
        sbc _el_half_w
        sta _el_x0
        lda elip_cx+1
        sbc _el_half_w+1
        sta _el_x0+1
        bpl +
        lda #0
        sta _el_x0
        sta _el_x0+1
+
        ; Precompute x1 = cx + half_w
        clc
        lda elip_cx
        adc _el_half_w
        sta _el_x1
        lda elip_cx+1
        adc _el_half_w+1
        sta _el_x1+1

        ; --- Top half: hline at cy - dy ---
        sec
        lda elip_cy
        sbc _el_dy
        bcc _el_skip_top        ; underflow = off screen
        cmp #200
        bcs _el_skip_top
        sta line_y0
        sta line_y1
        lda _el_x0
        sta line_x0
        lda _el_x0+1
        sta line_x0+1
        lda _el_x1
        sta line_x1
        lda _el_x1+1
        sta line_x1+1
        jsr draw_line

_el_skip_top:
        ; --- Bottom half: hline at cy + dy (only if dy > 0) ---
        lda _el_dy
        beq _el_next_dy

        clc
        lda elip_cy
        adc _el_dy
        bcs _el_next_dy         ; overflow = off screen
        cmp #200
        bcs _el_next_dy
        sta line_y0
        sta line_y1
        lda _el_x0
        sta line_x0
        lda _el_x0+1
        sta line_x0+1
        lda _el_x1
        sta line_x1
        lda _el_x1+1
        sta line_x1+1
        jsr draw_line

_el_next_dy:
        inc _el_dy
        lda _el_dy
        cmp elip_ry
        bcc _el_fill_loop
        beq _el_fill_loop       ; include dy = ry (single pixel at poles)
        rts


;---------------------------------------------------------------------------------------
; _el_get_sin - Quarter-sine lookup with quadrant handling
; Input:  A = angle (0-255, where 256 = full circle)
; Output: A = |sin(angle)| (0-255), _el_sin_neg = 0 if positive, $FF if negative
;---------------------------------------------------------------------------------------
_el_get_sin:
        sta _el_sin_angle
        lda #0
        sta _el_sin_neg

        lda _el_sin_angle
        cmp #128
        bcc _el_sin_pos_half

        ; Angles 128-255: sin is negative
        lda #$FF
        sta _el_sin_neg
        lda _el_sin_angle
        and #$7F                ; map 128-255 -> 0-127
        sta _el_sin_angle

_el_sin_pos_half:
        ; Angle is now 0-127
        lda _el_sin_angle
        cmp #64
        bcc _el_sin_q1

        ; Q2 (64-127): mirror using table[128 - angle]
        lda #128
        sec
        sbc _el_sin_angle
        tax
        lda _el_sin_table,x
        rts

_el_sin_q1:
        ; Q1 (0-63): direct lookup
        tax
        lda _el_sin_table,x
        rts


;---------------------------------------------------------------------------------------
; _el_isqrt - Integer square root of 16-bit value (rounded to nearest)
; Input:  _el_sqrt_in (16-bit, destroyed during computation)
; Output: A = 8-bit square root
; Method: Digit-by-digit binary algorithm, processes 2 input bits per iteration
;---------------------------------------------------------------------------------------
_el_isqrt:
        lda #0
        sta _el_sqrt_root
        sta _el_sqrt_rem
        sta _el_sqrt_rem+1
        ldx #8                  ; 8 iterations for 16-bit input

_el_sq_loop:
        ; Shift top 2 bits of input into remainder
        asl _el_sqrt_in
        rol _el_sqrt_in+1
        rol _el_sqrt_rem
        rol _el_sqrt_rem+1
        asl _el_sqrt_in
        rol _el_sqrt_in+1
        rol _el_sqrt_rem
        rol _el_sqrt_rem+1

        ; root <<= 1
        asl _el_sqrt_root

        ; test = 2 * root + 1
        lda _el_sqrt_root
        asl
        clc
        adc #1
        sta _el_sqrt_test
        lda #0
        adc #0
        sta _el_sqrt_test+1

        ; if remainder >= test: subtract and set low bit of root
        lda _el_sqrt_rem+1
        cmp _el_sqrt_test+1
        bcc _el_sq_skip
        bne _el_sq_sub
        lda _el_sqrt_rem
        cmp _el_sqrt_test
        bcc _el_sq_skip

_el_sq_sub:
        sec
        lda _el_sqrt_rem
        sbc _el_sqrt_test
        sta _el_sqrt_rem
        lda _el_sqrt_rem+1
        sbc _el_sqrt_test+1
        sta _el_sqrt_rem+1
        inc _el_sqrt_root

_el_sq_skip:
        dex
        bne _el_sq_loop

        ; Round to nearest: if remainder > root, root is closer to (root+1)
        lda _el_sqrt_rem+1
        bne _el_sq_round        ; high byte > 0 means rem > 255 > any root
        lda _el_sqrt_rem
        cmp _el_sqrt_root
        bcc _el_sq_done
        beq _el_sq_done

_el_sq_round:
        inc _el_sqrt_root

_el_sq_done:
        lda _el_sqrt_root
        rts


;---------------------------------------------------------------------------------------
; _el_mul8x8 - Unsigned 8x8 -> 16-bit multiply
; Input:  A = multiplicand, X = multiplier
; Output: _el_mul_result (16-bit, +1 = high byte)
;---------------------------------------------------------------------------------------
_el_mul8x8:
        sta _el_mul_a
        stx _el_mul_b
        lda #0
        sta _el_mul_result
        sta _el_mul_result+1
        ldx #8
-       lsr _el_mul_b
        bcc +
        clc
        lda _el_mul_result+1
        adc _el_mul_a
        sta _el_mul_result+1
+       ror _el_mul_result+1
        ror _el_mul_result
        dex
        bne -
        rts


;---------------------------------------------------------------------------------------
; _el_div16x8 - 16-bit / 8-bit unsigned division
; Input:  _el_div_num (16-bit), _el_divisor (8-bit)
; Output: _el_div_num (16-bit quotient)
;---------------------------------------------------------------------------------------
_el_div16x8:
        lda #0
        sta _el_div_rem
        ldx #16
-       asl _el_div_num
        rol _el_div_num+1
        rol _el_div_rem
        lda _el_div_rem
        cmp _el_divisor
        bcc +
        sbc _el_divisor
        sta _el_div_rem
        inc _el_div_num
+       dex
        bne -
        rts


;---------------------------------------------------------------------------------------
; Quarter-sine lookup table (65 entries: indices 0-64)
; _el_sin_table[i] = round(sin(i * pi / 128) * 255)
; Covers 0 to 90 degrees; full circle handled by quadrant mirroring
;---------------------------------------------------------------------------------------
_el_sin_table:
        .byte   0,   6,  13,  19,  25,  31,  37,  44
        .byte  50,  56,  62,  68,  74,  80,  86,  92
        .byte  98, 103, 109, 115, 120, 126, 131, 136
        .byte 142, 147, 152, 157, 162, 167, 171, 176
        .byte 180, 185, 189, 193, 197, 201, 205, 208
        .byte 212, 215, 219, 222, 225, 228, 231, 233
        .byte 236, 238, 240, 242, 244, 246, 247, 249
        .byte 250, 251, 252, 253, 254, 254, 255, 255
        .byte 255                                       ; entry 64 = sin(90)


;---------------------------------------------------------------------------------------
; Working variables
;---------------------------------------------------------------------------------------
_el_fill:       .byte 0
_el_angle:      .byte 0
_el_sin_angle:  .byte 0
_el_sin_neg:    .byte 0
_el_offset:     .word 0
_el_px:         .word 0         ; computed point X
_el_py:         .byte 0         ; computed point Y
_el_prev_x:     .word 0         ; previous point X (outline)
_el_prev_y:     .byte 0         ; previous point Y
_el_first_x:    .word 0         ; first point X (for closing outline)
_el_first_y:    .byte 0         ; first point Y
_el_dy:         .byte 0         ; current scanline offset (filled)
_el_ry2:        .word 0         ; ry^2 (filled)
_el_half_w:     .word 0         ; half-width at current scanline
_el_x0:         .word 0         ; left edge of hline
_el_x1:         .word 0         ; right edge of hline
_el_mul_a:      .byte 0
_el_mul_b:      .byte 0
_el_mul_result: .word 0
_el_div_num:    .word 0
_el_div_rem:    .byte 0
_el_divisor:    .byte 0
_el_sqrt_in:    .word 0
_el_sqrt_root:  .byte 0
_el_sqrt_rem:   .word 0
_el_sqrt_test:  .word 0