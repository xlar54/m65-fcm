;=======================================================================================
; demo_abstract.asm - Growing rotating shapes demo
; Shapes grow from center, rotate, and change colors infinitely until SPACE is pressed
;=======================================================================================

demo_abstract:
lda #MODE_BITMAP40
        jsr set_screen_mode

        ; Set background to black
        lda #$00
        sta BORDERCOL
        lda #$00
        sta BACKCOL

        ; Initialize first shape randomly
        jsr _dg_init_random_shape

_dg_main_loop:
        ; Draw current shape
        jsr _dg_draw_shape

        ; Check for spacebar
        jsr $FFE4
        cmp #' '
        beq _dg_exit

        ; Increment size
        lda _dg_radius
        clc
        adc #8                  ; Growth speed (3 pixels per frame)
        sta _dg_radius

        ; Rotate slightly
        lda _dg_angle
        clc
        adc #4                  ; Rotation speed
        sta _dg_angle

        ; Change color gradually
        inc _dg_color
        lda _dg_color
        and #$0F                ; Keep in range 0-15
        ora #$01                ; Avoid black (0)
        sta _dg_color

        ; Check if shape is too big
        lda _dg_radius
        cmp #90                 ; Max radius (fits in 180 height)
        bcc _dg_main_loop       ; Not too big yet, continue

        ; Shape reached max size - start new random shape
        jsr _dg_init_random_shape
        jmp _dg_main_loop

_dg_exit:
        rts

;=======================================================================================
; _dg_init_random_shape - Initialize a new random shape
;=======================================================================================
_dg_init_random_shape:
        ; Get random seed from system clock
        lda $D012               ; Raster line (pseudo-random)
        eor $DC04               ; XOR with CIA timer low byte
        sta _dg_random

        ; Random number of sides (3-8)
        and #$07                ; 0-7
        clc
        adc #3                  ; 3-10
        cmp #9
        bcc +
        lda #8                  ; Cap at 8 sides
+       sta _dg_sides

        ; Random starting color (1-15, avoid 0=black)
        lda _dg_random
        lsr
        lsr
        and #$0F
        ora #$01                ; Ensure not zero
        sta _dg_color

        ; Random starting angle (0-255)
        lda $DC05               ; CIA timer high byte
        sta _dg_angle

        ; Start small
        lda #5
        sta _dg_radius

        rts

;=======================================================================================
; _dg_draw_shape - Draw the current shape
;=======================================================================================
_dg_draw_shape:
        ; Set center of screen
        lda screen_mode
        cmp #80
        beq _dg_draw_640

        ; 40-col mode: center = 160, 100
        lda #<160
        sta poly_cx
        lda #>160
        sta poly_cx+1
        lda #100
        sta poly_cy
        jmp _dg_draw_params

_dg_draw_640:
        ; 80-col mode: center = 320, 100
        lda #<320
        sta poly_cx
        lda #>320
        sta poly_cx+1
        lda #100
        sta poly_cy

_dg_draw_params:
        ; Set polygon parameters
        lda _dg_radius
        sta poly_r
        lda _dg_sides
        sta poly_sides
        lda _dg_color
        sta poly_col
        lda #0
        sta poly_grad
        lda _dg_angle
        sta poly_angle

        ; Draw filled polygon
        clc                     ; outline
        jsr draw_polygon

        rts

;=======================================================================================
; Working variables
;=======================================================================================
_dg_radius:     .byte 5         ; Current radius
_dg_sides:      .byte 5         ; Number of sides
_dg_color:      .byte 2         ; Current color
_dg_angle:      .byte 0         ; Current rotation angle
_dg_random:     .byte 0         ; Random seed