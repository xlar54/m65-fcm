;=======================================================================================
; demo_floodfill.asm - Simple overlapping circles flood fill demo
; Called from main.asm as part of the FCM library demo suite
;=======================================================================================

demo_floodfill:
        lda #MODE_BITMAP40
        jsr set_screen_mode

        ;===================================================================
        ; Title text
        ;===================================================================
        lda #<_ff_msg
        sta str_ptr
        lda #>_ff_msg
        sta str_ptr+1
        lda #0
        sta str_row
        lda #8
        sta str_col
        lda #$01
        sta str_color
        jsr draw_petscii_string

        ; Set background to black
        lda #$00
        sta BORDERCOL
        lda #$00
        sta BACKCOL

        ; Draw the three overlapping circles (outlines only)
        jsr _dff_draw_circles

        ; Wait for spacebar
        jsr WAIT_SPACEBAR

        ; Fill the circles with colors
        jsr _dff_fill_circles

        ; Wait for any key to continue
        jsr WAIT_SPACEBAR

        rts

_ff_msg:
        .text "Flood fill example",0

;=======================================================================================
; _dff_draw_circles - Draw three overlapping circle outlines
;=======================================================================================
_dff_draw_circles:
        ; Left circle outline (will be red)
        lda #<120
        sta circ_cx
        lda #>120
        sta circ_cx+1
        lda #110
        sta circ_cy
        lda #50
        sta circ_r
        lda #$02                ; Red outline
        sta circ_col
        lda #0
        sta circ_grad
        clc                     ; Outline only
        jsr draw_circle

        ; Right circle outline (will be green)
        lda #<200
        sta circ_cx
        lda #>200
        sta circ_cx+1
        lda #110
        sta circ_cy
        lda #50
        sta circ_r
        lda #$05                ; Green outline
        sta circ_col
        clc
        jsr draw_circle

        ; Top circle outline (will be blue)
        lda #<160
        sta circ_cx
        lda #>160
        sta circ_cx+1
        lda #65
        sta circ_cy
        lda #50
        sta circ_r
        lda #$06                ; Blue outline
        sta circ_col
        clc
        jsr draw_circle

        rts

;=======================================================================================
; _dff_fill_circles - Fill each circle and overlapping regions
;=======================================================================================
_dff_fill_circles:
        ; Fill left circle with red (avoiding overlaps for now)
        lda #<95                ; Seed point in left-only area
        sta flood_x
        lda #>95
        sta flood_x+1
        lda #110
        sta flood_y
        lda #$02                ; Red
        sta flood_color
        lda #0                  ; Boundary fill mode
        sta flood_mode
        jsr flood_fill

        ; Fill right circle with green (avoiding overlaps)
        lda #<225               ; Seed point in right-only area
        sta flood_x
        lda #>225
        sta flood_x+1
        lda #110
        sta flood_y
        lda #$05                ; Green
        sta flood_color
        jsr flood_fill

        ; Fill top circle with blue (avoiding overlaps)
        lda #<160               ; Seed point in top-only area
        sta flood_x
        lda #>160
        sta flood_x+1
        lda #40
        sta flood_y
        lda #$06                ; Blue
        sta flood_color
        jsr flood_fill

        ; Fill red+blue overlap (left-top) with magenta
        lda #<130
        sta flood_x
        lda #>130
        sta flood_x+1
        lda #75
        sta flood_y
        lda #$04                ; Magenta (red+blue)
        sta flood_color
        jsr flood_fill

        ; Fill green+blue overlap (right-top) with cyan
        lda #<190
        sta flood_x
        lda #>190
        sta flood_x+1
        lda #75
        sta flood_y
        lda #$03                ; Cyan (green+blue)
        sta flood_color
        jsr flood_fill

        ; Fill red+green overlap (bottom) with yellow
        lda #<160
        sta flood_x
        lda #>160
        sta flood_x+1
        lda #125
        sta flood_y
        lda #$07                ; Yellow (red+green)
        sta flood_color
        jsr flood_fill

        ; Fill center (all three overlap) with white
        lda #<160
        sta flood_x
        lda #>160
        sta flood_x+1
        lda #90
        sta flood_y
        lda #$01                ; White (all colors)
        sta flood_color
        jsr flood_fill

        rts