;=======================================================================================
; demo_lines.asm - Mystify-style screensaver
; Two line objects with independently bouncing endpoints create flowing mesh patterns
;
; Each frame per line object:
;   1. Erase the OLDEST quad (connecting trail_idx to trail_idx+1)
;   2. Move endpoints
;   3. Store new position at trail_idx
;   4. Draw the NEWEST quad (connecting trail_idx to trail_idx-1)
;   5. Advance trail_idx
;
; Uses 16-bit X coordinates for full 320-pixel screen width.
;
; Quad shape (box):
;     A.ep0 ------- B.ep0
;       |             |
;     A.ep1 ------- B.ep1
;=======================================================================================

NUM_LINES = 2
TRAIL_LENGTH = 15

; Screen bounds
X_MIN = 5
X_MAX_LO = <314
X_MAX_HI = >314
Y_MIN = 5
Y_MAX = 194

demo_lines:
        lda #MODE_BITMAP40
        jsr set_screen_mode

        lda #$00
        sta BORDERCOL
        sta BACKCOL
        
        jsr clear_bitmap
        jsr _dl_init_lines

_dl_main_loop:
        jsr $FFE4
        cmp #' '
        beq _dl_exit

        jsr _dl_update_all_lines

        jmp _dl_main_loop

_dl_exit:
        rts

;=======================================================================================
; _dl_init_lines
;=======================================================================================
_dl_init_lines:
        ldx #0
_dl_init_loop:
        stx _dl_current_line

        ; Color (1-15)
        jsr _dl_random
        and #$0E
        ora #$01
        sta _dl_color,x

        ; Endpoint 0: left half of screen
        jsr _dl_random
        and #$7F                ; 0-127
        clc
        adc #20                 ; 20-147
        sta _dl_x0_lo,x
        lda #0
        sta _dl_x0_hi,x

        jsr _dl_random
        and #$7F
        clc
        adc #20
        cmp #Y_MAX
        bcc +
        lda #Y_MAX-10
+       sta _dl_y0,x

        ; Endpoint 1: right half of screen
        jsr _dl_random
        and #$7F                ; 0-127
        clc
        adc #130                ; 130-257
        sta _dl_x1_lo,x
        lda #0
        adc #0                  ; carry from add
        sta _dl_x1_hi,x
        ; Clamp to X_MAX
        lda _dl_x1_hi,x
        cmp #X_MAX_HI
        bcc _dl_init_x1ok
        bne _dl_init_x1clamp
        lda _dl_x1_lo,x
        cmp #X_MAX_LO
        bcc _dl_init_x1ok
_dl_init_x1clamp:
        lda #X_MAX_LO
        sta _dl_x1_lo,x
        lda #X_MAX_HI
        sta _dl_x1_hi,x
_dl_init_x1ok:

        jsr _dl_random
        and #$7F
        clc
        adc #20
        cmp #Y_MAX
        bcc +
        lda #Y_MAX-10
+       sta _dl_y1,x

        ; Velocities (signed, -3 to +3, no zero)
        jsr _dl_rand_vel
        sta _dl_vx0,x
        jsr _dl_rand_vel
        sta _dl_vy0,x
        jsr _dl_rand_vel
        sta _dl_vx1,x
        jsr _dl_rand_vel
        sta _dl_vy1,x

        ; Trail index and fill counter
        lda #0
        sta _dl_trail_idx,x
        sta _dl_fill_count,x    ; 0 = no quads stored yet

        ; Fill entire trail buffer with initial position
        ; so erase never reads garbage
        jsr _dl_fill_trail_with_current

        ldx _dl_current_line
        inx
        cpx #NUM_LINES
        bne _dl_init_loop
        rts

;---------------------------------------------------------------------------
; Fill all trail slots for current line with its current endpoint positions
;---------------------------------------------------------------------------
_dl_fill_trail_with_current:
        lda #0
        sta _dl_pf_slot
_dl_ftc_loop:
        ; _dl_temp must be set before EACH set call
        lda _dl_pf_slot
        sta _dl_temp
        ldx _dl_current_line
        lda _dl_x0_lo,x
        jsr _dl_set_trail_x0_lo

        lda _dl_pf_slot
        sta _dl_temp
        ldx _dl_current_line
        lda _dl_x0_hi,x
        jsr _dl_set_trail_x0_hi

        lda _dl_pf_slot
        sta _dl_temp
        ldx _dl_current_line
        lda _dl_y0,x
        jsr _dl_set_trail_y0

        lda _dl_pf_slot
        sta _dl_temp
        ldx _dl_current_line
        lda _dl_x1_lo,x
        jsr _dl_set_trail_x1_lo

        lda _dl_pf_slot
        sta _dl_temp
        ldx _dl_current_line
        lda _dl_x1_hi,x
        jsr _dl_set_trail_x1_hi

        lda _dl_pf_slot
        sta _dl_temp
        ldx _dl_current_line
        lda _dl_y1,x
        jsr _dl_set_trail_y1

        inc _dl_pf_slot
        lda _dl_pf_slot
        cmp #TRAIL_LENGTH
        bne _dl_ftc_loop
        rts

_dl_pf_slot: .byte 0

;---------------------------------------------------------------------------
; Random velocity: -3 to +3, never 0
;---------------------------------------------------------------------------
_dl_rand_vel:
        jsr _dl_random
        and #$05               ; 0-5
        clc
        adc #1                 ; 1-6
        sta _dl_rv_tmp
        jsr _dl_random
        and #$01               ; 0 or 1
        beq +
        ; Negate
        lda _dl_rv_tmp
        eor #$FF
        clc
        adc #1
        sta _dl_rv_tmp
+       lda _dl_rv_tmp
        ; Clamp to -3..+3
        bmi _dl_rv_neg
        cmp #4
        bcc _dl_rv_done
        lda #3
        bne _dl_rv_done
_dl_rv_neg:
        cmp #$FD               ; -3
        bcs _dl_rv_done
        lda #$FD               ; clamp to -3
_dl_rv_done:
        cmp #0
        bne +
        lda #1
+       rts

_dl_rv_tmp: .byte 0

;=======================================================================================
; _dl_random - PRNG
;=======================================================================================
_dl_random:
        lda _dl_random_seed
        asl
        asl
        asl
        eor _dl_random_seed
        asl
        rol _dl_random_seed
        lda $D012
        eor _dl_random_seed
        eor $DC04
        sta _dl_random_seed
        rts

_dl_random_seed: .byte 123

;=======================================================================================
; _dl_update_all_lines
;=======================================================================================
_dl_update_all_lines:
        ldx #0
_dl_ual_loop:
        stx _dl_current_line

        ; Only erase if we have a full trail (fill_count >= TRAIL_LENGTH)
        lda _dl_fill_count,x
        cmp #TRAIL_LENGTH
        bcc _dl_ual_skip_erase
        jsr _dl_erase_oldest
_dl_ual_skip_erase:

        ; Move endpoints
        jsr _dl_move_endpoints

        ; Store new position
        jsr _dl_store_positions

        ; Only draw newest quad if we have at least 2 entries
        ldx _dl_current_line
        lda _dl_fill_count,x
        cmp #2
        bcc _dl_ual_skip_draw
        jsr _dl_draw_newest
_dl_ual_skip_draw:

        ; Advance trail_idx
        ldx _dl_current_line
        inc _dl_trail_idx,x
        lda _dl_trail_idx,x
        cmp #TRAIL_LENGTH
        bcc +
        lda #0
        sta _dl_trail_idx,x
+

        ; Increment fill counter (caps at TRAIL_LENGTH)
        ldx _dl_current_line
        lda _dl_fill_count,x
        cmp #TRAIL_LENGTH
        bcs +
        inc _dl_fill_count,x
+

        ldx _dl_current_line
        inx
        cpx #NUM_LINES
        bne _dl_ual_loop
        rts

;=======================================================================================
; _dl_draw_quad - Draw/erase a box between two trail positions
;
;     A.ep0 ------- B.ep0
;       |             |
;     A.ep1 ------- B.ep1
;
; Input: _dl_pos_a, _dl_pos_b, _dl_draw_col
;=======================================================================================
_dl_draw_quad:
        ; --- Load vertex A.ep0 ---
        lda _dl_pos_a
        sta _dl_temp
        jsr _dl_get_trail_x0_lo
        lda _dl_tx
        sta _dl_qa_x0
        lda _dl_pos_a
        sta _dl_temp
        jsr _dl_get_trail_x0_hi
        lda _dl_tx
        sta _dl_qa_x0+1
        lda _dl_pos_a
        sta _dl_temp
        jsr _dl_get_trail_y0
        lda _dl_ty
        sta _dl_qa_y0

        ; --- Load vertex A.ep1 ---
        lda _dl_pos_a
        sta _dl_temp
        jsr _dl_get_trail_x1_lo
        lda _dl_tx
        sta _dl_qa_x1
        lda _dl_pos_a
        sta _dl_temp
        jsr _dl_get_trail_x1_hi
        lda _dl_tx
        sta _dl_qa_x1+1
        lda _dl_pos_a
        sta _dl_temp
        jsr _dl_get_trail_y1
        lda _dl_ty
        sta _dl_qa_y1

        ; --- Load vertex B.ep0 ---
        lda _dl_pos_b
        sta _dl_temp
        jsr _dl_get_trail_x0_lo
        lda _dl_tx
        sta _dl_qb_x0
        lda _dl_pos_b
        sta _dl_temp
        jsr _dl_get_trail_x0_hi
        lda _dl_tx
        sta _dl_qb_x0+1
        lda _dl_pos_b
        sta _dl_temp
        jsr _dl_get_trail_y0
        lda _dl_ty
        sta _dl_qb_y0

        ; --- Load vertex B.ep1 ---
        lda _dl_pos_b
        sta _dl_temp
        jsr _dl_get_trail_x1_lo
        lda _dl_tx
        sta _dl_qb_x1
        lda _dl_pos_b
        sta _dl_temp
        jsr _dl_get_trail_x1_hi
        lda _dl_tx
        sta _dl_qb_x1+1
        lda _dl_pos_b
        sta _dl_temp
        jsr _dl_get_trail_y1
        lda _dl_ty
        sta _dl_qb_y1

        ; --- Line 1: A.ep0 -> B.ep0 ---
        lda _dl_qa_x0
        sta line_x0
        lda _dl_qa_x0+1
        sta line_x0+1
        lda _dl_qa_y0
        sta line_y0
        lda _dl_qb_x0
        sta line_x1
        lda _dl_qb_x0+1
        sta line_x1+1
        lda _dl_qb_y0
        sta line_y1
        lda _dl_draw_col
        sta line_col
        jsr draw_line

        ; --- Line 2: B.ep0 -> B.ep1 ---
        lda _dl_qb_x0
        sta line_x0
        lda _dl_qb_x0+1
        sta line_x0+1
        lda _dl_qb_y0
        sta line_y0
        lda _dl_qb_x1
        sta line_x1
        lda _dl_qb_x1+1
        sta line_x1+1
        lda _dl_qb_y1
        sta line_y1
        lda _dl_draw_col
        sta line_col
        jsr draw_line

        ; --- Line 3: B.ep1 -> A.ep1 ---
        lda _dl_qb_x1
        sta line_x0
        lda _dl_qb_x1+1
        sta line_x0+1
        lda _dl_qb_y1
        sta line_y0
        lda _dl_qa_x1
        sta line_x1
        lda _dl_qa_x1+1
        sta line_x1+1
        lda _dl_qa_y1
        sta line_y1
        lda _dl_draw_col
        sta line_col
        jsr draw_line

        ; --- Line 4: A.ep1 -> A.ep0 ---
        lda _dl_qa_x1
        sta line_x0
        lda _dl_qa_x1+1
        sta line_x0+1
        lda _dl_qa_y1
        sta line_y0
        lda _dl_qa_x0
        sta line_x1
        lda _dl_qa_x0+1
        sta line_x1+1
        lda _dl_qa_y0
        sta line_y1
        lda _dl_draw_col
        sta line_col
        jsr draw_line

        rts

;=======================================================================================
; _dl_erase_oldest
; Oldest quad = trail_idx -> trail_idx+1
;=======================================================================================
_dl_erase_oldest:
        ldx _dl_current_line
        lda _dl_trail_idx,x
        sta _dl_pos_a
        clc
        adc #1
        cmp #TRAIL_LENGTH
        bcc +
        lda #0
+       sta _dl_pos_b
        lda #$00
        sta _dl_draw_col
        jsr _dl_draw_quad
        rts

;=======================================================================================
; _dl_draw_newest
; Newest quad = trail_idx -> trail_idx-1
;=======================================================================================
_dl_draw_newest:
        ldx _dl_current_line
        lda _dl_trail_idx,x
        sta _dl_pos_a
        sec
        sbc #1
        bpl +
        lda #TRAIL_LENGTH-1
+       sta _dl_pos_b
        ldx _dl_current_line
        lda _dl_color,x
        sta _dl_draw_col
        jsr _dl_draw_quad
        rts

;=======================================================================================
; _dl_move_endpoints - 16-bit X bounce, 8-bit Y bounce
;=======================================================================================
_dl_move_endpoints:
        ldx _dl_current_line

        ; ---- Endpoint 0 X ----
        lda _dl_vx0,x
        bpl _dl_mx0_pos
        ; Negative: subtract |vel|
        eor #$FF
        clc
        adc #1
        sta _dl_tmp_vel
        sec
        lda _dl_x0_lo,x
        sbc _dl_tmp_vel
        sta _dl_x0_lo,x
        lda _dl_x0_hi,x
        sbc #0
        sta _dl_x0_hi,x
        bmi _dl_mx0_lo          ; underflowed
        bne _dl_mx0_y           ; hi>0, OK
        lda _dl_x0_lo,x
        cmp #X_MIN
        bcs _dl_mx0_y
_dl_mx0_lo:
        lda #X_MIN
        sta _dl_x0_lo,x
        lda #0
        sta _dl_x0_hi,x
        lda _dl_vx0,x
        eor #$FF
        clc
        adc #1
        sta _dl_vx0,x
        jmp _dl_mx0_y
_dl_mx0_pos:
        clc
        adc _dl_x0_lo,x
        sta _dl_x0_lo,x
        lda _dl_x0_hi,x
        adc #0
        sta _dl_x0_hi,x
        cmp #X_MAX_HI
        bcc _dl_mx0_y
        bne _dl_mx0_hi
        lda _dl_x0_lo,x
        cmp #X_MAX_LO+1
        bcc _dl_mx0_y
_dl_mx0_hi:
        lda #X_MAX_LO
        sta _dl_x0_lo,x
        lda #X_MAX_HI
        sta _dl_x0_hi,x
        lda _dl_vx0,x
        eor #$FF
        clc
        adc #1
        sta _dl_vx0,x

_dl_mx0_y:
        ; ---- Endpoint 0 Y ----
        lda _dl_y0,x
        clc
        adc _dl_vy0,x
        sta _dl_y0,x
        bmi _dl_my0_lo
        cmp #Y_MIN
        bcc _dl_my0_lo
        cmp #Y_MAX+1
        bcc _dl_mx1
        lda #Y_MAX
        sta _dl_y0,x
        lda _dl_vy0,x
        eor #$FF
        clc
        adc #1
        sta _dl_vy0,x
        jmp _dl_mx1
_dl_my0_lo:
        lda #Y_MIN
        sta _dl_y0,x
        lda _dl_vy0,x
        eor #$FF
        clc
        adc #1
        sta _dl_vy0,x

_dl_mx1:
        ; ---- Endpoint 1 X ----
        lda _dl_vx1,x
        bpl _dl_mx1_pos
        eor #$FF
        clc
        adc #1
        sta _dl_tmp_vel
        sec
        lda _dl_x1_lo,x
        sbc _dl_tmp_vel
        sta _dl_x1_lo,x
        lda _dl_x1_hi,x
        sbc #0
        sta _dl_x1_hi,x
        bmi _dl_mx1_lo
        bne _dl_mx1_y
        lda _dl_x1_lo,x
        cmp #X_MIN
        bcs _dl_mx1_y
_dl_mx1_lo:
        lda #X_MIN
        sta _dl_x1_lo,x
        lda #0
        sta _dl_x1_hi,x
        lda _dl_vx1,x
        eor #$FF
        clc
        adc #1
        sta _dl_vx1,x
        jmp _dl_mx1_y
_dl_mx1_pos:
        clc
        adc _dl_x1_lo,x
        sta _dl_x1_lo,x
        lda _dl_x1_hi,x
        adc #0
        sta _dl_x1_hi,x
        cmp #X_MAX_HI
        bcc _dl_mx1_y
        bne _dl_mx1_hi
        lda _dl_x1_lo,x
        cmp #X_MAX_LO+1
        bcc _dl_mx1_y
_dl_mx1_hi:
        lda #X_MAX_LO
        sta _dl_x1_lo,x
        lda #X_MAX_HI
        sta _dl_x1_hi,x
        lda _dl_vx1,x
        eor #$FF
        clc
        adc #1
        sta _dl_vx1,x

_dl_mx1_y:
        ; ---- Endpoint 1 Y ----
        lda _dl_y1,x
        clc
        adc _dl_vy1,x
        sta _dl_y1,x
        bmi _dl_my1_lo
        cmp #Y_MIN
        bcc _dl_my1_lo
        cmp #Y_MAX+1
        bcc _dl_me_done
        lda #Y_MAX
        sta _dl_y1,x
        lda _dl_vy1,x
        eor #$FF
        clc
        adc #1
        sta _dl_vy1,x
        jmp _dl_me_done
_dl_my1_lo:
        lda #Y_MIN
        sta _dl_y1,x
        lda _dl_vy1,x
        eor #$FF
        clc
        adc #1
        sta _dl_vy1,x

_dl_me_done:
        rts

_dl_tmp_vel: .byte 0

;=======================================================================================
; _dl_store_positions
;=======================================================================================
_dl_store_positions:
        ldx _dl_current_line
        lda _dl_trail_idx,x
        sta _dl_temp
        lda _dl_x0_lo,x
        jsr _dl_set_trail_x0_lo

        ldx _dl_current_line
        lda _dl_trail_idx,x
        sta _dl_temp
        lda _dl_x0_hi,x
        jsr _dl_set_trail_x0_hi

        ldx _dl_current_line
        lda _dl_trail_idx,x
        sta _dl_temp
        lda _dl_y0,x
        jsr _dl_set_trail_y0

        ldx _dl_current_line
        lda _dl_trail_idx,x
        sta _dl_temp
        lda _dl_x1_lo,x
        jsr _dl_set_trail_x1_lo

        ldx _dl_current_line
        lda _dl_trail_idx,x
        sta _dl_temp
        lda _dl_x1_hi,x
        jsr _dl_set_trail_x1_hi

        ldx _dl_current_line
        lda _dl_trail_idx,x
        sta _dl_temp
        lda _dl_y1,x
        jsr _dl_set_trail_y1
        rts

;=======================================================================================
; Trail index: current_line * 15 + _dl_temp
;=======================================================================================
_dl_calc_trail_index:
        lda _dl_current_line
        asl
        asl
        asl
        asl                     ; *16
        sec
        sbc _dl_current_line    ; *15
        clc
        adc _dl_temp
        tax
        rts

;=======================================================================================
; Trail get/set
;=======================================================================================
_dl_get_trail_x0_lo:
        jsr _dl_calc_trail_index
        lda _dl_trail_x0_lo,x
        sta _dl_tx
        rts
_dl_get_trail_x0_hi:
        jsr _dl_calc_trail_index
        lda _dl_trail_x0_hi,x
        sta _dl_tx
        rts
_dl_get_trail_y0:
        jsr _dl_calc_trail_index
        lda _dl_trail_y0,x
        sta _dl_ty
        rts
_dl_get_trail_x1_lo:
        jsr _dl_calc_trail_index
        lda _dl_trail_x1_lo,x
        sta _dl_tx
        rts
_dl_get_trail_x1_hi:
        jsr _dl_calc_trail_index
        lda _dl_trail_x1_hi,x
        sta _dl_tx
        rts
_dl_get_trail_y1:
        jsr _dl_calc_trail_index
        lda _dl_trail_y1,x
        sta _dl_ty
        rts

_dl_set_trail_x0_lo:
        pha
        jsr _dl_calc_trail_index
        pla
        sta _dl_trail_x0_lo,x
        rts
_dl_set_trail_x0_hi:
        pha
        jsr _dl_calc_trail_index
        pla
        sta _dl_trail_x0_hi,x
        rts
_dl_set_trail_y0:
        pha
        jsr _dl_calc_trail_index
        pla
        sta _dl_trail_y0,x
        rts
_dl_set_trail_x1_lo:
        pha
        jsr _dl_calc_trail_index
        pla
        sta _dl_trail_x1_lo,x
        rts
_dl_set_trail_x1_hi:
        pha
        jsr _dl_calc_trail_index
        pla
        sta _dl_trail_x1_hi,x
        rts
_dl_set_trail_y1:
        pha
        jsr _dl_calc_trail_index
        pla
        sta _dl_trail_y1,x
        rts

;=======================================================================================
; Data
;=======================================================================================
_dl_x0_lo:          .fill NUM_LINES, 0
_dl_x0_hi:          .fill NUM_LINES, 0
_dl_y0:             .fill NUM_LINES, 0
_dl_x1_lo:          .fill NUM_LINES, 0
_dl_x1_hi:          .fill NUM_LINES, 0
_dl_y1:             .fill NUM_LINES, 0
_dl_vx0:            .fill NUM_LINES, 0
_dl_vy0:            .fill NUM_LINES, 0
_dl_vx1:            .fill NUM_LINES, 0
_dl_vy1:            .fill NUM_LINES, 0
_dl_color:          .fill NUM_LINES, 0
_dl_trail_idx:      .fill NUM_LINES, 0
_dl_fill_count:     .fill NUM_LINES, 0

_dl_trail_x0_lo:    .fill NUM_LINES * TRAIL_LENGTH, 0
_dl_trail_x0_hi:    .fill NUM_LINES * TRAIL_LENGTH, 0
_dl_trail_y0:       .fill NUM_LINES * TRAIL_LENGTH, 0
_dl_trail_x1_lo:    .fill NUM_LINES * TRAIL_LENGTH, 0
_dl_trail_x1_hi:    .fill NUM_LINES * TRAIL_LENGTH, 0
_dl_trail_y1:       .fill NUM_LINES * TRAIL_LENGTH, 0

_dl_current_line:   .byte 0
_dl_temp:           .byte 0
_dl_tx:             .byte 0
_dl_ty:             .byte 0

_dl_pos_a:          .byte 0
_dl_pos_b:          .byte 0
_dl_draw_col:       .byte 0
_dl_qa_x0:          .word 0
_dl_qa_y0:          .byte 0
_dl_qa_x1:          .word 0
_dl_qa_y1:          .byte 0
_dl_qb_x0:          .word 0
_dl_qb_y0:          .byte 0
_dl_qb_x1:          .word 0
_dl_qb_y1:          .byte 0