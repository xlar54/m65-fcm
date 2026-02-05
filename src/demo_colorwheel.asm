;=======================================================================================
; demo_colorwheel.asm - HSV Color Wheel for MEGA65 FCM bitmap mode
;
; Draws a full HSV color wheel:
;   - Hue varies by angle around the circle (0°=red at right, CCW)
;   - Saturation varies from center (white) to edge (full color)
;   - Value (brightness) is always maximum (15)
;
; Uses the existing plot_pixel, set_palette_color infrastructure.
; Palette entries are pre-built for the full HSV wheel.
;
; For 320×200 mode: circle centered at (160, 100), radius 90
;=======================================================================================

demo_colorwheel:
        lda #MODE_BITMAP40
        jsr set_screen_mode
        jsr init_palette

        ; Set background to dark grey
        lda #$06
        sta BORDERCOL
        lda #$06
        sta BACKCOL

        ; Set palette entry $01 to dark grey for background fill
        lda #$01
        ldx #4
        ldy #4
        ldz #4
        jsr set_palette_color

        ; Fill screen with dark grey
        lda #$01
        jsr clear_bitmap

        ; ---------------------------------------------------------------
        ; Phase 1: Build the HSV palette
        ;
        ; We pre-build 128 palette entries covering 16 hue steps × 8
        ; saturation steps.  Palette indices $02..$81.
        ;
        ; For each pixel we quantize:
        ;   hue16 = hue >> 4      (0..15)
        ;   sat8  = sat >> 1      (0..7)
        ;   palette index = hue16 * 8 + sat8 + 2
        ; ---------------------------------------------------------------

        lda #0
        sta _cw_build_hue       ; hue16 counter 0..15

_cw_build_hue_loop:
        lda #0
        sta _cw_build_sat       ; sat8 counter 0..7

_cw_build_sat_loop:
        ; hue = hue16 * 16
        lda _cw_build_hue
        asl
        asl
        asl
        asl
        sta _cw_hue

        ; sat = sat8 * 2 (+ 1 for centering, except sat8=0 -> 0)
        lda _cw_build_sat
        beq _cw_build_sat_zero
        asl
        clc
        adc #1
        cmp #16
        bcc +
        lda #15
+       sta _cw_sat
        jmp _cw_build_do_hsv
_cw_build_sat_zero:
        lda #0
        sta _cw_sat

_cw_build_do_hsv:
        jsr _cw_hsv_to_rgb

        ; palette index = hue16 * 8 + sat8 + 2
        lda _cw_build_hue
        asl
        asl
        asl
        clc
        adc _cw_build_sat
        clc
        adc #2

        ldx _cw_r
        ldy _cw_g
        ldz _cw_b
        jsr set_palette_color

        inc _cw_build_sat
        lda _cw_build_sat
        cmp #8
        bne _cw_build_sat_loop

        inc _cw_build_hue
        lda _cw_build_hue
        cmp #16
        bne _cw_build_hue_loop

        ; ---------------------------------------------------------------
        ; Phase 2: Draw the color wheel pixel by pixel
        ; ---------------------------------------------------------------

        lda #<160
        sta _cw_cx
        lda #>160
        sta _cw_cx+1
        lda #95
        sta _cw_cy
        lda #90
        sta _cw_radius

        ; Precompute r²
        lda _cw_radius
        sta _cw_mul_a
        sta _cw_mul_b
        jsr _cw_mul8x8u
        lda _cw_mul_res16
        sta _cw_rsq
        lda _cw_mul_res16+1
        sta _cw_rsq+1

        ; y_start = cy - radius
        sec
        lda _cw_cy
        sbc _cw_radius
        sta _cw_scan_y

        ; y_end = cy + radius (clamped to 199)
        clc
        lda _cw_cy
        adc _cw_radius
        cmp #200
        bcc +
        lda #199
+       sta _cw_y_end

; === Main scanline loop ===
_cw_scanline_loop:
        ; dy = scan_y - cy (signed)
        sec
        lda _cw_scan_y
        sbc _cw_cy
        sta _cw_dy

        ; |dy|
        bpl +
        eor #$FF
        clc
        adc #1
+       sta _cw_abs_dy

        ; dy²
        sta _cw_mul_a
        sta _cw_mul_b
        jsr _cw_mul8x8u
        lda _cw_mul_res16
        sta _cw_dysq
        lda _cw_mul_res16+1
        sta _cw_dysq+1

        ; diff = r² - dy²
        sec
        lda _cw_rsq
        sbc _cw_dysq
        sta _cw_diff
        lda _cw_rsq+1
        sbc _cw_dysq+1
        sta _cw_diff+1
        bcc _cw_next_scanline

        ; half_x = isqrt(diff)
        lda _cw_diff
        sta _cw_sqrt_in
        lda _cw_diff+1
        sta _cw_sqrt_in+1
        jsr _cw_isqrt16
        sta _cw_half_x

        ; *** Use 16-bit X to avoid signed-byte overflow ***
        ; x_left = cx - half_x
        sec
        lda _cw_cx
        sbc _cw_half_x
        sta _cw_cur_x
        lda _cw_cx+1
        sbc #0
        sta _cw_cur_x+1

        ; x_right = cx + half_x
        clc
        lda _cw_cx
        adc _cw_half_x
        sta _cw_x_right
        lda _cw_cx+1
        adc #0
        sta _cw_x_right+1

; === Pixel loop (left to right using 16-bit X) ===
_cw_pixel_loop:
        ; Check cur_x <= x_right (16-bit unsigned)
        lda _cw_x_right+1
        cmp _cw_cur_x+1
        bcc _cw_next_scanline   ; right.hi < cur.hi -> done
        bne _cw_do_pixel        ; right.hi > cur.hi -> keep going
        lda _cw_x_right
        cmp _cw_cur_x
        bcc _cw_next_scanline   ; right.lo < cur.lo -> done

_cw_do_pixel:
        ; dx = cur_x - cx (signed, fits in 8-bit since |dx| <= 90)
        sec
        lda _cw_cur_x
        sbc _cw_cx
        sta _cw_dx

        ; |dx|
        bpl +
        eor #$FF
        clc
        adc #1
+       sta _cw_abs_dx

        ; --- Distance from center ---
        lda _cw_abs_dx
        sta _cw_mul_a
        sta _cw_mul_b
        jsr _cw_mul8x8u
        clc
        lda _cw_mul_res16
        adc _cw_dysq
        sta _cw_sqrt_in
        lda _cw_mul_res16+1
        adc _cw_dysq+1
        sta _cw_sqrt_in+1
        jsr _cw_isqrt16
        sta _cw_dist

        ; Skip if outside circle
        cmp _cw_radius
        bcc _cw_inside
        beq _cw_inside
        jmp _cw_next_pixel
_cw_inside:

        ; --- Saturation = dist * 15 / radius ---
        lda _cw_dist
        sta _cw_mul_a
        lda #15
        sta _cw_mul_b
        jsr _cw_mul8x8u
        lda _cw_mul_res16
        sta _cw_div_num
        lda _cw_mul_res16+1
        sta _cw_div_num+1
        lda _cw_radius
        sta _cw_div_den
        jsr _cw_div16x8
        lda _cw_div_num
        cmp #16
        bcc +
        lda #15
+       sta _cw_sat

        ; --- Hue via atan2 ---
        jsr _cw_atan2

        ; --- Map to palette ---
        ; hue16 = hue >> 4
        lda _cw_hue
        lsr
        lsr
        lsr
        lsr
        sta _cw_hue16

        ; sat8 = sat >> 1
        lda _cw_sat
        lsr
        sta _cw_sat8

        ; palette = hue16*8 + sat8 + 2
        lda _cw_hue16
        asl
        asl
        asl
        clc
        adc _cw_sat8
        clc
        adc #2
        sta plot_col

        ; Plot
        lda _cw_cur_x
        sta plot_x
        lda _cw_cur_x+1
        sta plot_x+1
        lda _cw_scan_y
        sta plot_y
        jsr plot_pixel

_cw_next_pixel:
        inc _cw_cur_x
        bne +
        inc _cw_cur_x+1
+       jmp _cw_pixel_loop

_cw_next_scanline:
        lda _cw_scan_y
        cmp _cw_y_end
        beq _cw_done
        inc _cw_scan_y
        jmp _cw_scanline_loop

_cw_done:
        jsr WAIT_SPACEBAR
        rts


;=======================================================================================
; _cw_atan2 - Compute angle from (dx, dy) using octant-based atan2
;
; Input: _cw_dx (signed), _cw_abs_dx, _cw_abs_dy
; Output: _cw_hue (0..255)
;
; 0=right, 64=down, 128=left, 192=up (clockwise)
;
; Octant bits:  bit2=dy<0, bit1=dx<0, bit0=|dy|>|dx|
;=======================================================================================
_cw_atan2:
        lda _cw_abs_dx
        ora _cw_abs_dy
        bne +
        lda #0
        sta _cw_hue
        rts
+
        ; Build octant
        lda #0
        sta _cw_at_octant

        lda _cw_dy
        bpl +
        lda #4
        sta _cw_at_octant
+
        lda _cw_dx
        bpl +
        lda _cw_at_octant
        ora #2
        sta _cw_at_octant
+
        lda _cw_abs_dy
        cmp _cw_abs_dx
        bcc +
        beq +
        lda _cw_at_octant
        ora #1
        sta _cw_at_octant
+
        ; min/max
        lda _cw_abs_dy
        cmp _cw_abs_dx
        bcs _cw_at_dy_big

        ; |dx| >= |dy|
        lda _cw_abs_dy
        sta _cw_at_min
        lda _cw_abs_dx
        sta _cw_at_max
        jmp _cw_at_do_ratio

_cw_at_dy_big:
        lda _cw_abs_dx
        sta _cw_at_min
        lda _cw_abs_dy
        sta _cw_at_max

_cw_at_do_ratio:
        lda _cw_at_max
        bne +
        lda #0
        sta _cw_hue
        rts
+
        ; base = min * 32 / max  (range 0..32)
        lda _cw_at_min
        sta _cw_mul_a
        lda #32
        sta _cw_mul_b
        jsr _cw_mul8x8u

        lda _cw_mul_res16
        sta _cw_div_num
        lda _cw_mul_res16+1
        sta _cw_div_num+1
        lda _cw_at_max
        sta _cw_div_den
        jsr _cw_div16x8

        lda _cw_div_num
        cmp #33
        bcc +
        lda #32
+       sta _cw_at_base

        ; Map octant to full angle
        lda _cw_at_octant
        cmp #0
        beq _cw_oct0
        cmp #1
        beq _cw_oct1
        cmp #2
        beq _cw_oct2
        cmp #3
        beq _cw_oct3
        cmp #4
        beq _cw_oct4
        cmp #5
        beq _cw_oct5
        cmp #6
        beq _cw_oct6
        jmp _cw_oct7

_cw_oct0:                       ; dx+ dy+ |dx|>=|dy| -> 0 + base
        lda _cw_at_base
        jmp _cw_at_store
_cw_oct1:                       ; dx+ dy+ |dy|>|dx|  -> 64 - base
        lda #64
        sec
        sbc _cw_at_base
        jmp _cw_at_store
_cw_oct3:                       ; dx- dy+ |dy|>|dx|  -> 64 + base
        lda #64
        clc
        adc _cw_at_base
        jmp _cw_at_store
_cw_oct2:                       ; dx- dy+ |dx|>=|dy| -> 128 - base
        lda #128
        sec
        sbc _cw_at_base
        jmp _cw_at_store
_cw_oct6:                       ; dx- dy- |dx|>=|dy| -> 128 + base
        lda #128
        clc
        adc _cw_at_base
        jmp _cw_at_store
_cw_oct7:                       ; dx- dy- |dy|>|dx|  -> 192 - base
        lda #192
        sec
        sbc _cw_at_base
        jmp _cw_at_store
_cw_oct5:                       ; dx+ dy- |dy|>|dx|  -> 192 + base
        lda #192
        clc
        adc _cw_at_base
        jmp _cw_at_store
_cw_oct4:                       ; dx+ dy- |dx|>=|dy| -> 256 - base
        lda #0
        sec
        sbc _cw_at_base

_cw_at_store:
        sta _cw_hue
        rts


;=======================================================================================
; _cw_hsv_to_rgb
; Input: _cw_hue (0..255), _cw_sat (0..15), V=15 implicit
; Output: _cw_r, _cw_g, _cw_b (each 0..15)
;=======================================================================================
_cw_hsv_to_rgb:
        lda #15
        sec
        sbc _cw_sat
        sta _cw_min_val

        lda _cw_hue
        cmp #43
        bcc _cw_hsv_s0
        cmp #86
        bcc _cw_hsv_s1
        cmp #128
        bcc _cw_hsv_s2
        cmp #171
        bcc _cw_hsv_s3
        cmp #213
        bcc _cw_hsv_s4
        jmp _cw_hsv_s5

_cw_hsv_s0:                     ; R=15, G rises, B=min
        lda _cw_hue
        sta _cw_hsv_pos
        lda #43
        sta _cw_hsv_width
        jsr _cw_calc_ramp
        lda #15
        sta _cw_r
        lda _cw_ramp_val
        sta _cw_g
        lda _cw_min_val
        sta _cw_b
        rts

_cw_hsv_s1:                     ; R falls, G=15, B=min
        sec
        lda _cw_hue
        sbc #43
        sta _cw_hsv_pos
        lda #43
        sta _cw_hsv_width
        jsr _cw_calc_ramp_inv
        lda _cw_ramp_val
        sta _cw_r
        lda #15
        sta _cw_g
        lda _cw_min_val
        sta _cw_b
        rts

_cw_hsv_s2:                     ; R=min, G=15, B rises
        sec
        lda _cw_hue
        sbc #86
        sta _cw_hsv_pos
        lda #42
        sta _cw_hsv_width
        jsr _cw_calc_ramp
        lda _cw_min_val
        sta _cw_r
        lda #15
        sta _cw_g
        lda _cw_ramp_val
        sta _cw_b
        rts

_cw_hsv_s3:                     ; R=min, G falls, B=15
        sec
        lda _cw_hue
        sbc #128
        sta _cw_hsv_pos
        lda #43
        sta _cw_hsv_width
        jsr _cw_calc_ramp_inv
        lda _cw_min_val
        sta _cw_r
        lda _cw_ramp_val
        sta _cw_g
        lda #15
        sta _cw_b
        rts

_cw_hsv_s4:                     ; R rises, G=min, B=15
        sec
        lda _cw_hue
        sbc #171
        sta _cw_hsv_pos
        lda #42
        sta _cw_hsv_width
        jsr _cw_calc_ramp
        lda _cw_ramp_val
        sta _cw_r
        lda _cw_min_val
        sta _cw_g
        lda #15
        sta _cw_b
        rts

_cw_hsv_s5:                     ; R=15, G=min, B falls
        sec
        lda _cw_hue
        sbc #213
        sta _cw_hsv_pos
        lda #43
        sta _cw_hsv_width
        jsr _cw_calc_ramp_inv
        lda #15
        sta _cw_r
        lda _cw_min_val
        sta _cw_g
        lda _cw_ramp_val
        sta _cw_b
        rts


;---------------------------------------------------------------------------------------
; _cw_calc_ramp: ramp = min_val + sat * pos / width
;---------------------------------------------------------------------------------------
_cw_calc_ramp:
        lda _cw_sat
        sta _cw_mul_a
        lda _cw_hsv_pos
        sta _cw_mul_b
        jsr _cw_mul8x8u
        lda _cw_mul_res16
        sta _cw_div_num
        lda _cw_mul_res16+1
        sta _cw_div_num+1
        lda _cw_hsv_width
        sta _cw_div_den
        jsr _cw_div16x8
        clc
        lda _cw_min_val
        adc _cw_div_num
        cmp #16
        bcc +
        lda #15
+       sta _cw_ramp_val
        rts

;---------------------------------------------------------------------------------------
; _cw_calc_ramp_inv: ramp = 15 - sat * pos / width
;---------------------------------------------------------------------------------------
_cw_calc_ramp_inv:
        lda _cw_sat
        sta _cw_mul_a
        lda _cw_hsv_pos
        sta _cw_mul_b
        jsr _cw_mul8x8u
        lda _cw_mul_res16
        sta _cw_div_num
        lda _cw_mul_res16+1
        sta _cw_div_num+1
        lda _cw_hsv_width
        sta _cw_div_den
        jsr _cw_div16x8
        sec
        lda #15
        sbc _cw_div_num
        bcs +
        lda #0
+       sta _cw_ramp_val
        rts


;=======================================================================================
; _cw_mul8x8u - Unsigned 8×8 -> 16-bit multiply
;=======================================================================================
_cw_mul8x8u:
        lda #0
        sta _cw_mul_res16
        sta _cw_mul_res16+1
        ldx #8
-       lsr _cw_mul_b
        bcc +
        clc
        lda _cw_mul_res16+1
        adc _cw_mul_a
        sta _cw_mul_res16+1
+       ror _cw_mul_res16+1
        ror _cw_mul_res16
        dex
        bne -
        rts


;=======================================================================================
; _cw_div16x8 - 16-bit / 8-bit unsigned division
;=======================================================================================
_cw_div16x8:
        lda _cw_div_den
        beq _cw_div_zero
        lda #0
        sta _cw_div_rem
        ldx #16
-       asl _cw_div_num
        rol _cw_div_num+1
        rol _cw_div_rem
        lda _cw_div_rem
        cmp _cw_div_den
        bcc +
        sbc _cw_div_den
        sta _cw_div_rem
        inc _cw_div_num
+       dex
        bne -
        rts
_cw_div_zero:
        lda #0
        sta _cw_div_num
        sta _cw_div_num+1
        rts


;=======================================================================================
; _cw_isqrt16 - Integer square root (bit-by-bit, no multiply)
; Input: _cw_sqrt_in (16-bit)
; Output: A = floor(sqrt(input))
;=======================================================================================
_cw_isqrt16:
        lda #0
        sta _cw_sqrt_root
        sta _cw_sqrt_rem
        sta _cw_sqrt_rem+1
        ldx #8

_cw_sqrt_loop:
        ; Shift 2 bits from input into remainder
        asl _cw_sqrt_in
        rol _cw_sqrt_in+1
        rol _cw_sqrt_rem
        rol _cw_sqrt_rem+1
        asl _cw_sqrt_in
        rol _cw_sqrt_in+1
        rol _cw_sqrt_rem
        rol _cw_sqrt_rem+1

        ; trial = root*2 + 1
        lda _cw_sqrt_root
        asl
        ora #1
        sta _cw_sqrt_trial

        ; If remainder >= trial, subtract and set bit
        lda _cw_sqrt_rem+1
        bne _cw_sqrt_set        ; hi byte nonzero -> rem > trial (8-bit)
        lda _cw_sqrt_rem
        cmp _cw_sqrt_trial
        bcc _cw_sqrt_no

_cw_sqrt_set:
        sec
        lda _cw_sqrt_rem
        sbc _cw_sqrt_trial
        sta _cw_sqrt_rem
        lda _cw_sqrt_rem+1
        sbc #0
        sta _cw_sqrt_rem+1

        lda _cw_sqrt_root
        asl
        ora #1
        sta _cw_sqrt_root
        jmp _cw_sqrt_next

_cw_sqrt_no:
        asl _cw_sqrt_root

_cw_sqrt_next:
        dex
        bne _cw_sqrt_loop

        lda _cw_sqrt_root
        rts


;=======================================================================================
; Working variables
;=======================================================================================
_cw_cx:         .word 0
_cw_cy:         .byte 0
_cw_radius:     .byte 0
_cw_scan_y:     .byte 0
_cw_y_end:      .byte 0
_cw_dy:         .byte 0
_cw_abs_dy:     .byte 0
_cw_rsq:        .word 0
_cw_dysq:       .word 0
_cw_diff:       .word 0
_cw_half_x:     .byte 0
_cw_cur_x:      .word 0
_cw_x_right:    .word 0
_cw_dx:         .byte 0
_cw_abs_dx:     .byte 0
_cw_dist:       .byte 0
_cw_hue:        .byte 0
_cw_sat:        .byte 0
_cw_hue16:      .byte 0
_cw_sat8:       .byte 0
_cw_r:          .byte 0
_cw_g:          .byte 0
_cw_b:          .byte 0
_cw_min_val:    .byte 0
_cw_ramp_val:   .byte 0
_cw_hsv_pos:    .byte 0
_cw_hsv_width:  .byte 0
_cw_build_hue:  .byte 0
_cw_build_sat:  .byte 0
_cw_mul_a:      .byte 0
_cw_mul_b:      .byte 0
_cw_mul_res16:  .word 0
_cw_div_num:    .word 0
_cw_div_den:    .byte 0
_cw_div_rem:    .byte 0
_cw_sqrt_in:    .word 0
_cw_sqrt_root:  .byte 0
_cw_sqrt_rem:   .word 0
_cw_sqrt_trial: .byte 0
_cw_at_octant:  .byte 0
_cw_at_min:     .byte 0
_cw_at_max:     .byte 0
_cw_at_base:    .byte 0