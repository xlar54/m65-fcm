;=======================================================================================
; demo_lines.asm - Mystify-style screensaver (simplified)
;
; Linear trail array, no circular buffer:
;   trail[0] = oldest, trail[TRAIL_LENGTH-1] = newest
;
; Each frame:
;   1. Erase quad between trail[0] and trail[1]
;   2. Shift trail down by 1
;   3. Move endpoints
;   4. Store new position at trail[TRAIL_LENGTH-1]
;   5. Draw quad between trail[TRAIL_LENGTH-2] and trail[TRAIL_LENGTH-1]
;
; Uses [PTR],z for 32-bit indirect + Z indexed addressing (45GS02).
;=======================================================================================

NUM_LINES = 2
TRAIL_LENGTH = 8

; Each trail entry = 6 bytes: x0_lo, x0_hi, y0, x1_lo, x1_hi, y1
ENTRY_SIZE = 6
TRAIL_BYTES = TRAIL_LENGTH * ENTRY_SIZE   ; 48 bytes per line

; Offsets within an entry
OFS_X0_LO = 0
OFS_X0_HI = 1
OFS_Y0    = 2
OFS_X1_LO = 3
OFS_X1_HI = 4
OFS_Y1    = 5

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
        stx _dl_cur

        ; Color
        jsr _dl_random
        and #$0E
        ora #$01
        sta _dl_color,x

        ; Endpoint 0: left area
        jsr _dl_random
        and #$7F
        clc
        adc #20
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

        ; Endpoint 1: right area
        jsr _dl_random
        and #$7F
        clc
        adc #130
        sta _dl_x1_lo,x
        lda #0
        adc #0
        sta _dl_x1_hi,x
        ; Clamp
        lda _dl_x1_hi,x
        cmp #X_MAX_HI
        bcc _dl_ix1ok
        bne _dl_ix1cl
        lda _dl_x1_lo,x
        cmp #X_MAX_LO
        bcc _dl_ix1ok
_dl_ix1cl:
        lda #X_MAX_LO
        sta _dl_x1_lo,x
        lda #X_MAX_HI
        sta _dl_x1_hi,x
_dl_ix1ok:

        jsr _dl_random
        and #$7F
        clc
        adc #20
        cmp #Y_MAX
        bcc +
        lda #Y_MAX-10
+       sta _dl_y1,x

        ; Velocities
        jsr _dl_rand_vel
        sta _dl_vx0,x
        jsr _dl_rand_vel
        sta _dl_vy0,x
        jsr _dl_rand_vel
        sta _dl_vx1,x
        jsr _dl_rand_vel
        sta _dl_vy1,x

        ; Fill count
        lda #1
        sta _dl_fill,x

        ; Fill all trail slots with initial position
        jsr _dl_fill_trail

        ldx _dl_cur
        inx
        cpx #NUM_LINES
        bne _dl_init_loop
        rts

;---------------------------------------------------------------------------
; Fill all trail slots for current line with its current position
;---------------------------------------------------------------------------
_dl_fill_trail:
        jsr _dl_set_ptr_trail   ; PTR -> trail base for this line

        ldz #0
        ldx _dl_cur
_dl_ft_loop:
        lda _dl_x0_lo,x
        sta [PTR],z
        inz
        lda _dl_x0_hi,x
        sta [PTR],z
        inz
        lda _dl_y0,x
        sta [PTR],z
        inz
        lda _dl_x1_lo,x
        sta [PTR],z
        inz
        lda _dl_x1_hi,x
        sta [PTR],z
        inz
        lda _dl_y1,x
        sta [PTR],z
        inz
        cpz #TRAIL_BYTES
        bne _dl_ft_loop
        rts

;---------------------------------------------------------------------------
; Set PTR to trail base for line _dl_cur
; Address = _dl_trail + _dl_cur * TRAIL_BYTES (48)
;---------------------------------------------------------------------------
_dl_set_ptr_trail:
        lda _dl_cur
        ; *48 = *32 + *16
        asl             ; *2
        asl             ; *4
        asl             ; *8
        asl             ; *16
        sta _dl_tb_tmp
        asl             ; *32
        clc
        adc _dl_tb_tmp  ; *48
        clc
        adc #<_dl_trail
        sta PTR
        lda #>_dl_trail
        adc #0
        sta PTR+1
        lda #0
        sta PTR+2
        sta PTR+3
        rts

_dl_tb_tmp: .byte 0

;---------------------------------------------------------------------------
; Random velocity: -3 to +3, never 0
;---------------------------------------------------------------------------
_dl_rand_vel:
        jsr _dl_random
        and #$07               ; 0-7
        cmp #7
        bcc +
        lda #6
+       sec
        sbc #3                 ; -3 to +3
        cmp #0
        bne +
        lda #1
+       rts

;=======================================================================================
; _dl_random
;=======================================================================================
_dl_random:
        lda _dl_seed
        asl
        asl
        asl
        eor _dl_seed
        asl
        rol _dl_seed
        lda $D012
        eor _dl_seed
        eor $DC04
        sta _dl_seed
        rts

_dl_seed: .byte 123

;=======================================================================================
; _dl_update_all_lines
;=======================================================================================
_dl_update_all_lines:
        ldx #0
_dl_ual_loop:
        stx _dl_cur

        ; 1. Erase oldest quad (trail[0] -> trail[1])
        lda _dl_fill,x
        cmp #2
        bcc _dl_ual_skip_erase
        lda #$00
        sta _dl_draw_col
        lda #0                          ; slot A offset = 0
        sta _dl_slot_a
        lda #ENTRY_SIZE                 ; slot B offset = 6
        sta _dl_slot_b
        jsr _dl_draw_quad
_dl_ual_skip_erase:

        ; 2. Shift trail down by 1 entry
        jsr _dl_shift_trail

        ; 3. Move endpoints
        jsr _dl_move_endpoints

        ; 4. Store new at trail[TRAIL_LENGTH-1]
        jsr _dl_store_newest

        ; 5. Draw newest quad
        ldx _dl_cur
        lda _dl_fill,x
        cmp #2
        bcc _dl_ual_skip_draw
        ldx _dl_cur
        lda _dl_color,x
        sta _dl_draw_col
        lda #(TRAIL_LENGTH-2)*ENTRY_SIZE
        sta _dl_slot_a
        lda #(TRAIL_LENGTH-1)*ENTRY_SIZE
        sta _dl_slot_b
        jsr _dl_draw_quad
_dl_ual_skip_draw:

        ; Increment fill count (cap at TRAIL_LENGTH)
        ldx _dl_cur
        lda _dl_fill,x
        cmp #TRAIL_LENGTH
        bcs +
        inc _dl_fill,x
+
        ldx _dl_cur
        inx
        cpx #NUM_LINES
        bne _dl_ual_loop
        rts

;=======================================================================================
; _dl_shift_trail - Shift trail entries down by 1
; Copy trail[1..end] to trail[0..end-1]
;=======================================================================================
_dl_shift_trail:
        jsr _dl_set_ptr_trail   ; PTR -> trail base

        ldz #0
_dl_st_loop:
        ; Read from z + ENTRY_SIZE
        phz                     ; save dst offset
        tza
        clc
        adc #ENTRY_SIZE
        taz                     ; z = src offset
        lda [PTR],z             ; read src
        sta _dl_st_tmp          ; temp store
        plz                     ; restore dst offset
        lda _dl_st_tmp
        sta [PTR],z             ; write to dst
        inz
        cpz #(TRAIL_BYTES - ENTRY_SIZE)
        bne _dl_st_loop
        rts

_dl_st_tmp: .byte 0

;=======================================================================================
; _dl_store_newest - Store current positions at trail[TRAIL_LENGTH-1]
;=======================================================================================
_dl_store_newest:
        jsr _dl_set_ptr_trail

        ldx _dl_cur
        ldz #(TRAIL_LENGTH-1)*ENTRY_SIZE

        lda _dl_x0_lo,x
        sta [PTR],z
        inz
        lda _dl_x0_hi,x
        sta [PTR],z
        inz
        lda _dl_y0,x
        sta [PTR],z
        inz
        lda _dl_x1_lo,x
        sta [PTR],z
        inz
        lda _dl_x1_hi,x
        sta [PTR],z
        inz
        lda _dl_y1,x
        sta [PTR],z
        rts

;=======================================================================================
; _dl_draw_quad - Draw/erase a box between two trail entries
;
; Input: _dl_slot_a = byte offset of entry A in trail
;        _dl_slot_b = byte offset of entry B in trail
;        _dl_draw_col = color
;        PTR must be set to trail base (call _dl_set_ptr_trail first)
;
;     A.ep0 ------- B.ep0
;       |             |
;     A.ep1 ------- B.ep1
;=======================================================================================
_dl_draw_quad:
        jsr _dl_set_ptr_trail

        ; Load vertex A
        ldz _dl_slot_a
        lda [PTR],z             ; A.x0 lo
        sta _dl_qa_x0
        inz
        lda [PTR],z             ; A.x0 hi
        sta _dl_qa_x0+1
        inz
        lda [PTR],z             ; A.y0
        sta _dl_qa_y0
        inz
        lda [PTR],z             ; A.x1 lo
        sta _dl_qa_x1
        inz
        lda [PTR],z             ; A.x1 hi
        sta _dl_qa_x1+1
        inz
        lda [PTR],z             ; A.y1
        sta _dl_qa_y1

        ; Load vertex B
        ldz _dl_slot_b
        lda [PTR],z
        sta _dl_qb_x0
        inz
        lda [PTR],z
        sta _dl_qb_x0+1
        inz
        lda [PTR],z
        sta _dl_qb_y0
        inz
        lda [PTR],z
        sta _dl_qb_x1
        inz
        lda [PTR],z
        sta _dl_qb_x1+1
        inz
        lda [PTR],z
        sta _dl_qb_y1

        ; Line 1: A.ep0 -> B.ep0
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

        ; Line 2: B.ep0 -> B.ep1
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

        ; Line 3: B.ep1 -> A.ep1
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

        ; Line 4: A.ep1 -> A.ep0
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
; _dl_move_endpoints
;=======================================================================================
_dl_move_endpoints:
        ldx _dl_cur

        ; ---- EP0 X ----
        lda _dl_vx0,x
        bpl _dl_mx0p
        eor #$FF
        clc
        adc #1
        sta _dl_vtmp
        sec
        lda _dl_x0_lo,x
        sbc _dl_vtmp
        sta _dl_x0_lo,x
        lda _dl_x0_hi,x
        sbc #0
        sta _dl_x0_hi,x
        bmi _dl_mx0bl
        bne _dl_mx0y
        lda _dl_x0_lo,x
        cmp #X_MIN
        bcs _dl_mx0y
_dl_mx0bl:
        lda #X_MIN
        sta _dl_x0_lo,x
        lda #0
        sta _dl_x0_hi,x
        lda _dl_vx0,x
        eor #$FF
        clc
        adc #1
        sta _dl_vx0,x
        jmp _dl_mx0y
_dl_mx0p:
        clc
        adc _dl_x0_lo,x
        sta _dl_x0_lo,x
        lda _dl_x0_hi,x
        adc #0
        sta _dl_x0_hi,x
        cmp #X_MAX_HI
        bcc _dl_mx0y
        bne _dl_mx0bh
        lda _dl_x0_lo,x
        cmp #X_MAX_LO+1
        bcc _dl_mx0y
_dl_mx0bh:
        lda #X_MAX_LO
        sta _dl_x0_lo,x
        lda #X_MAX_HI
        sta _dl_x0_hi,x
        lda _dl_vx0,x
        eor #$FF
        clc
        adc #1
        sta _dl_vx0,x

_dl_mx0y:
        ; ---- EP0 Y ----
        lda _dl_y0,x
        clc
        adc _dl_vy0,x
        sta _dl_y0,x
        bmi _dl_my0bl
        cmp #Y_MIN
        bcc _dl_my0bl
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
_dl_my0bl:
        lda #Y_MIN
        sta _dl_y0,x
        lda _dl_vy0,x
        eor #$FF
        clc
        adc #1
        sta _dl_vy0,x

_dl_mx1:
        ; ---- EP1 X ----
        lda _dl_vx1,x
        bpl _dl_mx1p
        eor #$FF
        clc
        adc #1
        sta _dl_vtmp
        sec
        lda _dl_x1_lo,x
        sbc _dl_vtmp
        sta _dl_x1_lo,x
        lda _dl_x1_hi,x
        sbc #0
        sta _dl_x1_hi,x
        bmi _dl_mx1bl
        bne _dl_mx1y
        lda _dl_x1_lo,x
        cmp #X_MIN
        bcs _dl_mx1y
_dl_mx1bl:
        lda #X_MIN
        sta _dl_x1_lo,x
        lda #0
        sta _dl_x1_hi,x
        lda _dl_vx1,x
        eor #$FF
        clc
        adc #1
        sta _dl_vx1,x
        jmp _dl_mx1y
_dl_mx1p:
        clc
        adc _dl_x1_lo,x
        sta _dl_x1_lo,x
        lda _dl_x1_hi,x
        adc #0
        sta _dl_x1_hi,x
        cmp #X_MAX_HI
        bcc _dl_mx1y
        bne _dl_mx1bh
        lda _dl_x1_lo,x
        cmp #X_MAX_LO+1
        bcc _dl_mx1y
_dl_mx1bh:
        lda #X_MAX_LO
        sta _dl_x1_lo,x
        lda #X_MAX_HI
        sta _dl_x1_hi,x
        lda _dl_vx1,x
        eor #$FF
        clc
        adc #1
        sta _dl_vx1,x

_dl_mx1y:
        ; ---- EP1 Y ----
        lda _dl_y1,x
        clc
        adc _dl_vy1,x
        sta _dl_y1,x
        bmi _dl_my1bl
        cmp #Y_MIN
        bcc _dl_my1bl
        cmp #Y_MAX+1
        bcc _dl_mdone
        lda #Y_MAX
        sta _dl_y1,x
        lda _dl_vy1,x
        eor #$FF
        clc
        adc #1
        sta _dl_vy1,x
        jmp _dl_mdone
_dl_my1bl:
        lda #Y_MIN
        sta _dl_y1,x
        lda _dl_vy1,x
        eor #$FF
        clc
        adc #1
        sta _dl_vy1,x

_dl_mdone:
        rts

_dl_vtmp: .byte 0

;=======================================================================================
; Data
;=======================================================================================
_dl_x0_lo:      .fill NUM_LINES, 0
_dl_x0_hi:      .fill NUM_LINES, 0
_dl_y0:         .fill NUM_LINES, 0
_dl_x1_lo:      .fill NUM_LINES, 0
_dl_x1_hi:      .fill NUM_LINES, 0
_dl_y1:         .fill NUM_LINES, 0
_dl_vx0:        .fill NUM_LINES, 0
_dl_vy0:        .fill NUM_LINES, 0
_dl_vx1:        .fill NUM_LINES, 0
_dl_vy1:        .fill NUM_LINES, 0
_dl_color:      .fill NUM_LINES, 0
_dl_fill:       .fill NUM_LINES, 0

; Trail: packed entries, ENTRY_SIZE bytes each
_dl_trail:      .fill NUM_LINES * TRAIL_BYTES, 0

; Working vars
_dl_cur:        .byte 0
_dl_draw_col:   .byte 0
_dl_slot_a:     .byte 0
_dl_slot_b:     .byte 0
_dl_qa_x0:      .word 0
_dl_qa_y0:      .byte 0
_dl_qa_x1:      .word 0
_dl_qa_y1:      .byte 0
_dl_qb_x0:      .word 0
_dl_qb_y0:      .byte 0
_dl_qb_x1:      .word 0
_dl_qb_y1:      .byte 0