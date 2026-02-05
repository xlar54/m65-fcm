;=======================================================================================
; draw_polygon - Draw a regular polygon (outline or filled)
; Input: poly_cx (16-bit), poly_cy (8-bit), poly_r (8-bit), poly_col (8-bit)
;        poly_sides (8-bit, must be 3-32)
;        Carry: clear=outline, set=filled
; Automatically doubles x offsets in 80-col mode for correct aspect ratio
; First vertex points up (12 o'clock position)
;=======================================================================================
poly_cx:    .word 0
poly_cy:    .byte 0
poly_r:     .byte 0
poly_col:   .byte 0
poly_sides: .byte 0

draw_polygon:
        ; Save fill flag from carry FIRST (before any CMP trashes it)
        lda #0
        rol
        sta _pg_fill

        ; Now validate sides (3-32)
        lda poly_sides
        cmp #3
        bcc _pg_exit
        cmp #33
        bcs _pg_exit

        ; Compute all vertex positions
        jsr _pg_calc_vertices

        ; Draw outline or filled
        lda _pg_fill
        bne +
        jsr _pg_draw_outline
        rts
+       jsr _pg_draw_filled
_pg_exit:
        rts


;---------------------------------------------------------------------------------------
; _pg_calc_vertices - Compute vertex positions using sine/cosine lookup
; For vertex i: angle = i * 256 / sides - 64 (rotated so vertex 0 is at top)
;   vx[i] = cx + r * cos(angle)    (with aspect ratio correction in 80-col)
;   vy[i] = cy + r * sin(angle)    (clamped to 0-199)
;---------------------------------------------------------------------------------------
_pg_calc_vertices:
        lda #0
        sta _pg_cur_vertex

_pg_vert_loop:
        ; --- Compute angle = cur_vertex * 256 / sides - 64 ---
        lda _pg_cur_vertex
        sta _pg_div_num+1       ; vertex_idx in high byte = idx * 256
        lda #0
        sta _pg_div_num
        lda poly_sides
        sta _pg_divisor
        jsr _pg_div16x8
        lda _pg_div_num         ; angle (0-255)
        sec
        sbc #64                 ; rotate -90° so vertex 0 points up (12 o'clock)
        sta _pg_cur_angle       ; wraps naturally in 8-bit unsigned

        ; --- X offset: r * cos(angle) ---
        clc
        lda _pg_cur_angle
        adc #64                 ; cos = sin(angle + 64)
        jsr _pg_get_sin         ; A = |sin|, _pg_sin_neg = sign

        ldx poly_r
        jsr _pg_mul8x8          ; _pg_mul_result+1 = r * |cos| / 256

        ; Apply aspect ratio doubling for 80-col
        lda _pg_mul_result+1
        sta _pg_offset
        lda #0
        sta _pg_offset+1

        lda screen_mode
        cmp #80
        bne +
        asl _pg_offset
        rol _pg_offset+1
+
        ; vx = cx +/- offset
        lda _pg_sin_neg
        bne _pg_vx_sub

        clc
        lda poly_cx
        adc _pg_offset
        sta _pg_tmp_vx
        lda poly_cx+1
        adc _pg_offset+1
        sta _pg_tmp_vx+1
        jmp _pg_do_vy

_pg_vx_sub:
        sec
        lda poly_cx
        sbc _pg_offset
        sta _pg_tmp_vx
        lda poly_cx+1
        sbc _pg_offset+1
        sta _pg_tmp_vx+1
        ; Clamp to 0 if negative
        bpl _pg_do_vy
        lda #0
        sta _pg_tmp_vx
        sta _pg_tmp_vx+1

_pg_do_vy:
        ; --- Y offset: r * sin(angle) ---
        lda _pg_cur_angle
        jsr _pg_get_sin

        ldx poly_r
        jsr _pg_mul8x8

        ; vy = cy +/- offset (clamped to 0-199)
        lda _pg_sin_neg
        bne _pg_vy_sub

        clc
        lda poly_cy
        adc _pg_mul_result+1
        bcs _pg_vy_clamp_hi     ; 8-bit overflow -> clamp
        cmp #200
        bcc _pg_store_vert
_pg_vy_clamp_hi:
        lda #199
        jmp _pg_store_vert

_pg_vy_sub:
        sec
        lda poly_cy
        sbc _pg_mul_result+1
        bcs _pg_store_vert      ; no underflow, value is valid
        lda #0                  ; underflow -> clamp to 0

_pg_store_vert:
        ; Store Y coordinate
        ldx _pg_cur_vertex
        sta _pg_vy,x

        ; Store X coordinate (word array, index = vertex * 2)
        txa
        asl
        tax
        lda _pg_tmp_vx
        sta _pg_vx,x
        lda _pg_tmp_vx+1
        sta _pg_vx+1,x

        ; Next vertex
        inc _pg_cur_vertex
        lda _pg_cur_vertex
        cmp poly_sides
        bne _pg_vert_loop
        rts


;---------------------------------------------------------------------------------------
; _pg_get_sin - Quarter-sine lookup with quadrant handling
; Input:  A = angle (0-255, where 256 = full circle)
; Output: A = |sin(angle)| (0-255), _pg_sin_neg = 0 if positive, $FF if negative
;---------------------------------------------------------------------------------------
_pg_get_sin:
        sta _pg_sin_angle
        lda #0
        sta _pg_sin_neg

        lda _pg_sin_angle
        cmp #128
        bcc _pg_sin_pos_half

        ; Angles 128-255: sin is negative
        lda #$FF
        sta _pg_sin_neg
        lda _pg_sin_angle
        and #$7F                ; map 128-255 -> 0-127
        sta _pg_sin_angle

_pg_sin_pos_half:
        ; Angle is now 0-127
        lda _pg_sin_angle
        cmp #64
        bcc _pg_sin_q1

        ; Q2 (64-127): mirror using table[128-angle]
        lda #128
        sec
        sbc _pg_sin_angle
        tax
        lda _pg_sin_table,x
        rts

_pg_sin_q1:
        ; Q1 (0-63): direct lookup
        tax
        lda _pg_sin_table,x
        rts


;---------------------------------------------------------------------------------------
; _pg_draw_outline - Draw lines between consecutive vertices
;---------------------------------------------------------------------------------------
_pg_draw_outline:
        lda poly_col
        sta line_col

        lda #0
        sta _pg_edge_idx

_pg_ol_loop:
        ; Start vertex = edge_idx
        ldx _pg_edge_idx
        lda _pg_vy,x
        sta line_y0
        txa
        asl
        tax
        lda _pg_vx,x
        sta line_x0
        lda _pg_vx+1,x
        sta line_x0+1

        ; End vertex = (edge_idx + 1) % sides
        ldx _pg_edge_idx
        inx
        cpx poly_sides
        bne +
        ldx #0
+       lda _pg_vy,x
        sta line_y1
        txa
        asl
        tax
        lda _pg_vx,x
        sta line_x1
        lda _pg_vx+1,x
        sta line_x1+1

        jsr draw_line

        inc _pg_edge_idx
        lda _pg_edge_idx
        cmp poly_sides
        bne _pg_ol_loop
        rts


;---------------------------------------------------------------------------------------
; _pg_draw_filled - Convex polygon scanline fill
; For each scanline Y from min_vy to max_vy:
;   Find leftmost and rightmost edge intersection X
;   Draw horizontal line between them
;---------------------------------------------------------------------------------------
_pg_draw_filled:
        lda poly_col
        sta line_col

        ; Find min_y and max_y across all vertices
        lda #199
        sta _pg_min_y
        lda #0
        sta _pg_max_y
        ldx #0

_pg_find_yrange:
        lda _pg_vy,x
        cmp _pg_min_y
        bcs +
        sta _pg_min_y
+       cmp _pg_max_y
        bcc +
        beq +
        sta _pg_max_y
+       inx
        cpx poly_sides
        bne _pg_find_yrange

        ; Clamp max_y to 199
        lda _pg_max_y
        cmp #200
        bcc +
        lda #199
        sta _pg_max_y
+
        ; Scanline loop: min_y to max_y inclusive
        lda _pg_min_y
        sta _pg_scan_y

_pg_scan_loop:
        ; Init min/max X for this scanline
        lda #$FF
        sta _pg_min_sx
        sta _pg_min_sx+1        ; min starts at $FFFF
        lda #0
        sta _pg_max_sx
        sta _pg_max_sx+1        ; max starts at $0000

        ; Test each edge for intersection
        lda #0
        sta _pg_edge_idx

_pg_edge_test:
        ; Load vertex i
        ldx _pg_edge_idx
        lda _pg_vy,x
        sta _pg_vy0
        txa
        asl
        tax
        lda _pg_vx,x
        sta _pg_vx0
        lda _pg_vx+1,x
        sta _pg_vx0+1

        ; Load vertex (i+1) % sides
        ldx _pg_edge_idx
        inx
        cpx poly_sides
        bne +
        ldx #0
+       lda _pg_vy,x
        sta _pg_vy1
        txa
        asl
        tax
        lda _pg_vx,x
        sta _pg_vx1
        lda _pg_vx+1,x
        sta _pg_vx1+1

        ; Orient edge so vy0 <= vy1
        lda _pg_vy0
        cmp _pg_vy1
        bcc _pg_edge_oriented   ; vy0 < vy1, OK
        beq _pg_edge_next       ; horizontal edge, skip

        ; Swap endpoints
        lda _pg_vx0
        ldy _pg_vx1
        sty _pg_vx0
        sta _pg_vx1
        lda _pg_vx0+1
        ldy _pg_vx1+1
        sty _pg_vx0+1
        sta _pg_vx1+1
        lda _pg_vy0
        ldy _pg_vy1
        sty _pg_vy0
        sta _pg_vy1

_pg_edge_oriented:
        ; Check scan_y in [vy0, vy1]
        lda _pg_scan_y
        cmp _pg_vy0
        bcc _pg_edge_next       ; scan_y < vy0
        lda _pg_vy1
        cmp _pg_scan_y
        bcc _pg_edge_next       ; vy1 < scan_y

        ; Compute intersection X at this scanline
        jsr _pg_calc_intersect

        ; Clamp negative X to 0
        lda _pg_ix+1
        bpl _pg_ix_nonneg
        lda #0
        sta _pg_ix
        sta _pg_ix+1
_pg_ix_nonneg:

        ; Update min_sx (unsigned 16-bit compare)
        lda _pg_ix+1
        cmp _pg_min_sx+1
        bcc _pg_do_update_min   ; ix.hi < min.hi
        bne _pg_check_max       ; ix.hi > min.hi
        lda _pg_ix
        cmp _pg_min_sx
        bcs _pg_check_max       ; ix.lo >= min.lo

_pg_do_update_min:
        lda _pg_ix
        sta _pg_min_sx
        lda _pg_ix+1
        sta _pg_min_sx+1

_pg_check_max:
        ; Update max_sx
        lda _pg_ix+1
        cmp _pg_max_sx+1
        bcc _pg_edge_next       ; ix.hi < max.hi
        bne _pg_do_update_max   ; ix.hi > max.hi
        lda _pg_ix
        cmp _pg_max_sx
        bcc _pg_edge_next       ; ix.lo < max.lo
        beq _pg_edge_next       ; ix.lo = max.lo

_pg_do_update_max:
        lda _pg_ix
        sta _pg_max_sx
        lda _pg_ix+1
        sta _pg_max_sx+1

_pg_edge_next:
        inc _pg_edge_idx
        lda _pg_edge_idx
        cmp poly_sides
        bne _pg_edge_test

        ; Draw horizontal line if valid intersection found
        lda _pg_min_sx+1
        cmp _pg_max_sx+1
        bcc _pg_do_hline        ; min.hi < max.hi
        bne _pg_scan_next       ; min.hi > max.hi, skip
        lda _pg_min_sx
        cmp _pg_max_sx
        beq _pg_do_hline        ; equal = single pixel
        bcs _pg_scan_next       ; min.lo > max.lo, skip

_pg_do_hline:
        lda _pg_min_sx
        sta line_x0
        lda _pg_min_sx+1
        sta line_x0+1
        lda _pg_max_sx
        sta line_x1
        lda _pg_max_sx+1
        sta line_x1+1
        lda _pg_scan_y
        sta line_y0
        sta line_y1
        jsr draw_line

_pg_scan_next:
        lda _pg_scan_y
        cmp _pg_max_y
        bcs _pg_fill_done       ; scan_y >= max_y, done
        inc _pg_scan_y
        jmp _pg_scan_loop

_pg_fill_done:
        rts


;---------------------------------------------------------------------------------------
; _pg_calc_intersect - Compute X intersection of an edge with a scanline
; Input:  _pg_vx0/vy0, _pg_vx1/vy1 (oriented so vy0 <= vy1), _pg_scan_y
; Output: _pg_ix (16-bit X)
; Method: x = vx0 + (scan_y - vy0) * (vx1 - vx0) / (vy1 - vy0)
;         Uses fractional interpolation to avoid large intermediates
;---------------------------------------------------------------------------------------
_pg_calc_intersect:
        ; t = scan_y - vy0
        sec
        lda _pg_scan_y
        sbc _pg_vy0
        sta _pg_t
        beq _pg_ix_v0           ; t=0 -> x = vx0

        ; dy = vy1 - vy0
        sec
        lda _pg_vy1
        sbc _pg_vy0
        sta _pg_dy
        cmp _pg_t
        beq _pg_ix_v1           ; t=dy -> x = vx1

        ; frac = t * 256 / dy  (0-255 representing 0.0 to ~1.0)
        lda _pg_t
        sta _pg_div_num+1       ; t in high byte = t * 256
        lda #0
        sta _pg_div_num
        lda _pg_dy
        sta _pg_divisor
        jsr _pg_div16x8
        lda _pg_div_num
        sta _pg_frac

        ; dx = vx1 - vx0 (signed 16-bit)
        sec
        lda _pg_vx1
        sbc _pg_vx0
        sta _pg_dx
        lda _pg_vx1+1
        sbc _pg_vx0+1
        sta _pg_dx+1

        ; Get |dx| and remember sign
        lda #0
        sta _pg_dx_neg
        lda _pg_dx+1
        bpl _pg_dx_pos

        ; Negate dx
        lda #$FF
        sta _pg_dx_neg
        sec
        lda #0
        sbc _pg_dx
        sta _pg_dx
        lda #0
        sbc _pg_dx+1
        sta _pg_dx+1

_pg_dx_pos:
        ; offset = |dx| * frac / 256
        ;        = (dx_hi * frac) + (dx_lo * frac) >> 8

        ; Low part: (dx_lo * frac) >> 8
        lda _pg_dx
        ldx _pg_frac
        jsr _pg_mul8x8
        lda _pg_mul_result+1    ; high byte of product
        sta _pg_offset

        ; High part: dx_hi * frac (full 16-bit)
        lda _pg_dx+1
        ldx _pg_frac
        jsr _pg_mul8x8

        ; Combine: offset = high_part + low_part_hi
        clc
        lda _pg_offset
        adc _pg_mul_result
        sta _pg_offset
        lda _pg_mul_result+1
        adc #0
        sta _pg_offset+1

        ; Apply sign: ix = vx0 +/- offset
        lda _pg_dx_neg
        bne _pg_ix_sub

        clc
        lda _pg_vx0
        adc _pg_offset
        sta _pg_ix
        lda _pg_vx0+1
        adc _pg_offset+1
        sta _pg_ix+1
        rts

_pg_ix_sub:
        sec
        lda _pg_vx0
        sbc _pg_offset
        sta _pg_ix
        lda _pg_vx0+1
        sbc _pg_offset+1
        sta _pg_ix+1
        rts

_pg_ix_v0:
        lda _pg_vx0
        sta _pg_ix
        lda _pg_vx0+1
        sta _pg_ix+1
        rts

_pg_ix_v1:
        lda _pg_vx1
        sta _pg_ix
        lda _pg_vx1+1
        sta _pg_ix+1
        rts


;---------------------------------------------------------------------------------------
; _pg_mul8x8 - Unsigned 8x8 -> 16-bit multiply
; Input:  A = multiplicand, X = multiplier
; Output: _pg_mul_result (16-bit, +1 = high byte)
;---------------------------------------------------------------------------------------
_pg_mul8x8:
        sta _pg_mul_a
        stx _pg_mul_b
        lda #0
        sta _pg_mul_result
        sta _pg_mul_result+1
        ldx #8
-       lsr _pg_mul_b
        bcc +
        clc
        lda _pg_mul_result+1
        adc _pg_mul_a
        sta _pg_mul_result+1
+       ror _pg_mul_result+1
        ror _pg_mul_result
        dex
        bne -
        rts


;---------------------------------------------------------------------------------------
; _pg_div16x8 - 16-bit / 8-bit unsigned division
; Input:  _pg_div_num (16-bit), _pg_divisor (8-bit)
; Output: _pg_div_num (16-bit quotient)
;---------------------------------------------------------------------------------------
_pg_div16x8:
        lda #0
        sta _pg_div_rem
        ldx #16
-       asl _pg_div_num
        rol _pg_div_num+1
        rol _pg_div_rem
        lda _pg_div_rem
        cmp _pg_divisor
        bcc +
        sbc _pg_divisor
        sta _pg_div_rem
        inc _pg_div_num
+       dex
        bne -
        rts


;---------------------------------------------------------------------------------------
; Quarter-sine lookup table (65 entries: indices 0-64)
; _pg_sin_table[i] = round(sin(i * pi / 128) * 255)
; Covers 0 to 90 degrees; full circle handled by quadrant mirroring in _pg_get_sin
;---------------------------------------------------------------------------------------
_pg_sin_table:
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
_pg_fill:       .byte 0
_pg_cur_angle:  .byte 0
_pg_cur_vertex: .byte 0
_pg_sin_angle:  .byte 0
_pg_sin_neg:    .byte 0
_pg_offset:     .word 0
_pg_tmp_vx:     .word 0
_pg_edge_idx:   .byte 0
_pg_scan_y:     .byte 0
_pg_min_y:      .byte 0
_pg_max_y:      .byte 0
_pg_min_sx:     .word 0
_pg_max_sx:     .word 0
_pg_vx0:        .word 0
_pg_vy0:        .byte 0
_pg_vx1:        .word 0
_pg_vy1:        .byte 0
_pg_ix:         .word 0
_pg_t:          .byte 0
_pg_dy:         .byte 0
_pg_dx:         .word 0
_pg_dx_neg:     .byte 0
_pg_frac:       .byte 0
_pg_mul_a:      .byte 0
_pg_mul_b:      .byte 0
_pg_mul_result: .word 0
_pg_div_num:    .word 0
_pg_div_rem:    .byte 0
_pg_divisor:    .byte 0

; Vertex buffers (max 32 vertices)
_pg_vx:
        .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
_pg_vy:
        .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
        .byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0