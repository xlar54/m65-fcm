;=======================================================================================
; demo40.asm - Shape showcase demo for 320x200 FCM bitmap mode
;
; Displays two rows of shapes:
;   Top row:    Outline drawings (square, rectangle, pentagon, triangle, circle, ellipse)
;   Bottom row: Same shapes filled with angular gradients, alternating directions:
;               SQR=left-right (angle 64), RECT=top-bottom (angle 0),
;               PENT=right-left (angle 192), TRI=bottom-top (angle 128),
;               CIRC=diagonal TL-BR (angle 32), ELIP=diagonal TR-BL (angle 224)
;=======================================================================================

SET_PALETTE_CLR      .macro index, r,g,b
        lda #\index
        ldx #\r
        ldy #\g
        ldz #\b
        jsr set_palette_color
.endmacro


;=======================================================================================
; demo40 - Main entry point
;=======================================================================================
demo40:
        lda #MODE_BITMAP40
        jsr set_screen_mode

        ;===================================================================
        ; Title text
        ;===================================================================
        lda #<_d40_msg1
        sta str_ptr
        lda #>_d40_msg1
        sta str_ptr+1
        lda #0
        sta str_row
        lda #8
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d40_msg2
        sta str_ptr
        lda #>_d40_msg2
        sta str_ptr+1
        lda #1
        sta str_row
        lda #11
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        ;===================================================================
        ; Outline shape colors
        ;===================================================================
        #SET_PALETTE_CLR $10, $0F, $02, $02
        ;lda #$10
        ;ldx #$0F
        ;ldy #$02
        ;ldz #$02
        ;jsr set_palette_color   ; red (square)

        #SET_PALETTE_CLR $11, $02, $0D, $02
        ;lda #$11
        ;ldx #$02
        ;ldy #$0D
        ;ldz #$02
        ;jsr set_palette_color   ; green (rectangle)

        #SET_PALETTE_CLR $12, $02, $04, $0F
        ;lda #$12
        ;ldx #$02
        ;ldy #$04
        ;ldz #$0F
        ;jsr set_palette_color   ; blue (pentagon)

        #SET_PALETTE_CLR $13, $0E, $0D, $01
        ;lda #$13
        ;ldx #$0E
        ;ldy #$0D
        ;ldz #$01
        ;jsr set_palette_color   ; yellow (triangle)

        #SET_PALETTE_CLR $14, $01, $0C, $0E
        ;lda #$14
        ;ldx #$01
        ;ldy #$0C
        ;ldz #$0E
        ;jsr set_palette_color   ; cyan (circle)

        lda #$15
        ldx #$0D
        ldy #$02
        ldz #$0D
        jsr set_palette_color   ; magenta (ellipse)

        lda #$16
        ldx #$06
        ldy #$06
        ldz #$06
        jsr set_palette_color   ; grey (divider)

        ;===================================================================
        ; Ensure gf_mode is off for outline shapes
        ;===================================================================
        lda #0
        sta gf_mode

        ;===================================================================
        ; TOP ROW: Outline shapes, y ~ 30..85
        ;===================================================================

        ;--- 1. Square (outline) at (10, 35) size 40x40 ---
        lda #10
        sta rect_x
        lda #0
        sta rect_x+1
        lda #35
        sta rect_y
        lda #40
        sta rect_w
        lda #0
        sta rect_w+1
        lda #40
        sta rect_h
        lda #$10
        sta rect_col
        lda #0
        sta rect_grad
        clc
        jsr draw_rect

        ;--- 2. Rectangle (outline) at (62, 40) size 48x30 ---
        lda #62
        sta rect_x
        lda #0
        sta rect_x+1
        lda #40
        sta rect_y
        lda #48
        sta rect_w
        lda #0
        sta rect_w+1
        lda #30
        sta rect_h
        lda #$11
        sta rect_col
        lda #0
        sta rect_grad
        clc
        jsr draw_rect

        ;--- 3. Pentagon (outline) at (140, 55) r=22 ---
        lda #<140
        sta poly_cx
        lda #>140
        sta poly_cx+1
        lda #55
        sta poly_cy
        lda #22
        sta poly_r
        lda #5
        sta poly_sides
        lda #$12
        sta poly_col
        lda #0
        sta poly_grad
        lda #218             ; Flat left edge: 128 + (256/(sides*2)) = 102
        sta poly_angle       ; For a flat bottom edge, rotate 90° more (or 64 in a 0-255 system) = 102+64
        clc
        jsr draw_polygon

        ;--- 4. Triangle (outline) at (192, 57) r=22 ---
        lda #<192
        sta poly_cx
        lda #>192
        sta poly_cx+1
        lda #57
        sta poly_cy
        lda #22
        sta poly_r
        lda #3
        sta poly_sides
        lda #$13
        sta poly_col
        lda #0
        sta poly_grad
        lda #149          ; Flat left edge: 128 + (256/(sides*2)) = 42
        sta poly_angle  ; For a flat bottom edge, rotate 90° more (or 64 in this 0-255 system) = 42+64
        clc
        jsr draw_polygon

        ;--- 5. Circle (outline) at (242, 55) r=22 ---
        lda #<242
        sta circ_cx
        lda #>242
        sta circ_cx+1
        lda #55
        sta circ_cy
        lda #22
        sta circ_r
        lda #$14
        sta circ_col
        lda #0
        sta circ_grad
        clc
        jsr draw_circle

        ;--- 6. Ellipse (outline) at (295, 55) rx=22 ry=18 ---
        lda #<295
        sta elip_cx
        lda #>295
        sta elip_cx+1
        lda #55
        sta elip_cy
        lda #22
        sta elip_rx
        lda #18
        sta elip_ry
        lda #$15
        sta elip_col
        lda #0
        sta elip_grad
        clc
        jsr draw_ellipse

        ;===================================================================
        ; Divider line
        ;===================================================================
        lda #0
        sta line_x0
        sta line_x0+1
        lda #94
        sta line_y0
        lda #<319
        sta line_x1
        lda #>319
        sta line_x1+1
        lda #94
        sta line_y1
        lda #$16
        sta line_col
        jsr draw_line

        ;===================================================================
        ; Section labels
        ;===================================================================
        lda #<_d40_lbl_outline
        sta str_ptr
        lda #>_d40_lbl_outline
        sta str_ptr+1
        lda #2
        sta str_row
        lda #0
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d40_lbl_gradient
        sta str_ptr
        lda #>_d40_lbl_gradient
        sta str_ptr+1
        lda #12
        sta str_row
        lda #0
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        ;===================================================================
        ; BOTTOM ROW: Gradient-filled shapes with angular gradients
        ;===================================================================

        ;--- 1. Square: LEFT-TO-RIGHT (angle 64), red to yellow ---
        lda #$0F
        sta grad_r1
        lda #$00
        sta grad_g1
        sta grad_b1
        lda #$0F
        sta grad_r2
        sta grad_g2
        lda #$00
        sta grad_b2
        lda #$80
        sta grad_pal

        lda #<30
        sta gf_cx
        lda #>30
        sta gf_cx+1
        lda #125
        sta gf_cy
        lda #20
        sta gf_radius
        lda #64
        sta gf_angle
        jsr gf_setup

        lda #10
        sta rect_x
        lda #0
        sta rect_x+1
        lda #105
        sta rect_y
        lda #40
        sta rect_w
        lda #0
        sta rect_w+1
        lda #40
        sta rect_h
        lda #$80
        sta rect_col
        lda #0
        sta rect_grad
        sec
        jsr draw_rect

        ;--- 2. Rectangle: TOP-TO-BOTTOM (angle 0), green to blue ---
        lda #$00
        sta grad_r1
        lda #$0F
        sta grad_g1
        lda #$00
        sta grad_b1
        lda #$00
        sta grad_r2
        sta grad_g2
        lda #$0F
        sta grad_b2
        lda #$90
        sta grad_pal

        lda #<86
        sta gf_cx
        lda #>86
        sta gf_cx+1
        lda #120
        sta gf_cy
        lda #15
        sta gf_radius
        lda #0
        sta gf_angle
        jsr gf_setup

        lda #62
        sta rect_x
        lda #0
        sta rect_x+1
        lda #105
        sta rect_y
        lda #48
        sta rect_w
        lda #0
        sta rect_w+1
        lda #30
        sta rect_h
        lda #$90
        sta rect_col
        lda #0
        sta rect_grad
        sec
        jsr draw_rect

        ;--- 3. Pentagon: RIGHT-TO-LEFT (angle 192), cyan to dark red ---
        lda #$00
        sta grad_r1
        lda #$0F
        sta grad_g1
        sta grad_b1
        lda #$0C
        sta grad_r2
        lda #$00
        sta grad_g2
        sta grad_b2
        lda #$A0
        sta grad_pal

        lda #<140
        sta gf_cx
        lda #>140
        sta gf_cx+1
        lda #125
        sta gf_cy
        lda #22
        sta gf_radius
        lda #192
        sta gf_angle
        jsr gf_setup

        lda #<140
        sta poly_cx
        lda #>140
        sta poly_cx+1
        lda #125
        sta poly_cy
        lda #22
        sta poly_r
        lda #5
        sta poly_sides
        lda #$A0
        sta poly_col
        lda #0
        sta poly_grad
        lda #217
        sta poly_angle
        sec
        jsr draw_polygon

        ;--- 4. Triangle: BOTTOM-TO-TOP (angle 128), orange to purple ---
        lda #$0F
        sta grad_r1
        lda #$08
        sta grad_g1
        lda #$00
        sta grad_b1
        lda #$08
        sta grad_r2
        lda #$00
        sta grad_g2
        lda #$0F
        sta grad_b2
        lda #$B0
        sta grad_pal

        lda #<192
        sta gf_cx
        lda #>192
        sta gf_cx+1
        lda #127
        sta gf_cy
        lda #22
        sta gf_radius
        lda #128
        sta gf_angle
        jsr gf_setup

        lda #<192
        sta poly_cx
        lda #>192
        sta poly_cx+1
        lda #127
        sta poly_cy
        lda #22
        sta poly_r
        lda #3
        sta poly_sides
        lda #$B0
        sta poly_col
        lda #0
        sta poly_grad
        lda #149
        sta poly_angle
        sec
        jsr draw_polygon

        ;--- 5. Circle: DIAGONAL TL-BR (angle 32), white to deep blue ---
        lda #$0F
        sta grad_r1
        sta grad_g1
        sta grad_b1
        lda #$00
        sta grad_r2
        sta grad_g2
        lda #$0F
        sta grad_b2
        lda #$C0
        sta grad_pal

        lda #<242
        sta gf_cx
        lda #>242
        sta gf_cx+1
        lda #125
        sta gf_cy
        lda #22
        sta gf_radius
        lda #32
        sta gf_angle
        jsr gf_setup

        lda #<242
        sta circ_cx
        lda #>242
        sta circ_cx+1
        lda #125
        sta circ_cy
        lda #22
        sta circ_r
        lda #$C0
        sta circ_col
        lda #0
        sta circ_grad
        sec
        jsr draw_circle

        ;--- 6. Ellipse: DIAGONAL TR-BL (angle 224), yellow to red ---
        lda #$0F
        sta grad_r1
        sta grad_g1
        lda #$00
        sta grad_b1
        lda #$0F
        sta grad_r2
        lda #$00
        sta grad_g2
        sta grad_b2
        lda #$D0
        sta grad_pal

        lda #<295
        sta gf_cx
        lda #>295
        sta gf_cx+1
        lda #125
        sta gf_cy
        lda #22
        sta gf_radius
        lda #224
        sta gf_angle
        jsr gf_setup

        lda #<295
        sta elip_cx
        lda #>295
        sta elip_cx+1
        lda #125
        sta elip_cy
        lda #22
        sta elip_rx
        lda #18
        sta elip_ry
        lda #$D0
        sta elip_col
        lda #0
        sta elip_grad
        sec
        jsr draw_ellipse

        ;===================================================================
        ; Disable angular gradient
        ;===================================================================
        lda #0
        sta gf_mode

        ;===================================================================
        ; Shape labels (row 23)
        ;===================================================================
        lda #<_d40_lbl_sq
        sta str_ptr
        lda #>_d40_lbl_sq
        sta str_ptr+1
        lda #23
        sta str_row
        lda #1
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d40_lbl_rc
        sta str_ptr
        lda #>_d40_lbl_rc
        sta str_ptr+1
        lda #23
        sta str_row
        lda #8
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d40_lbl_pg
        sta str_ptr
        lda #>_d40_lbl_pg
        sta str_ptr+1
        lda #23
        sta str_row
        lda #15
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d40_lbl_tr
        sta str_ptr
        lda #>_d40_lbl_tr
        sta str_ptr+1
        lda #23
        sta str_row
        lda #21
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d40_lbl_ci
        sta str_ptr
        lda #>_d40_lbl_ci
        sta str_ptr+1
        lda #23
        sta str_row
        lda #27
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d40_lbl_el
        sta str_ptr
        lda #>_d40_lbl_el
        sta str_ptr+1
        lda #23
        sta str_row
        lda #33
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        ;--- Direction labels (row 24) ---
        lda #<_d40_dir1
        sta str_ptr
        lda #>_d40_dir1
        sta str_ptr+1
        lda #24
        sta str_row
        lda #1
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d40_dir2
        sta str_ptr
        lda #>_d40_dir2
        sta str_ptr+1
        lda #24
        sta str_row
        lda #9
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d40_dir3
        sta str_ptr
        lda #>_d40_dir3
        sta str_ptr+1
        lda #24
        sta str_row
        lda #15
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d40_dir4
        sta str_ptr
        lda #>_d40_dir4
        sta str_ptr+1
        lda #24
        sta str_row
        lda #21
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d40_dir5
        sta str_ptr
        lda #>_d40_dir5
        sta str_ptr+1
        lda #24
        sta str_row
        lda #27
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d40_dir6
        sta str_ptr
        lda #>_d40_dir6
        sta str_ptr+1
        lda #24
        sta str_row
        lda #33
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        ;===================================================================
        ; Wait for spacebar
        ;===================================================================
        ;jsr WAIT_SPACEBAR

;--- 4. Spinning Triangle at (192, 57) r=22 ---
        lda #149
        sta _spin_angle         ; Start at angle 0

_spin_loop:
        ; Erase previous triangle (draw in background color)
        lda #<192
        sta poly_cx
        lda #>192
        sta poly_cx+1
        lda #57
        sta poly_cy
        lda #22
        sta poly_r
        lda #3
        sta poly_sides
        lda #$01                ; Background color (erase)
        sta poly_col
        lda #0
        sta poly_grad
        lda _spin_angle
        sta poly_angle
        clc
        jsr draw_polygon

        ; Increment angle for next frame
        lda _spin_angle
        clc
        adc #2                  ; Rotation speed (2 = slow, 4 = medium, 8 = fast)
        sta _spin_angle

        ; Draw triangle at new angle (in yellow)
        lda #<192
        sta poly_cx
        lda #>192
        sta poly_cx+1
        lda #57
        sta poly_cy
        lda #22
        sta poly_r
        lda #3
        sta poly_sides
        lda #$13                ; Yellow
        sta poly_col
        lda #0
        sta poly_grad
        lda _spin_angle
        sta poly_angle
        clc
        jsr draw_polygon

        ; Check for spacebar
        jsr $FFE4               ; GETIN
        cmp #' '
        beq _spin_done          ; Exit if spacebar pressed
        cmp #0
        beq _spin_loop          ; No key pressed, continue spinning
        jmp _spin_loop          ; Other key pressed, ignore and continue

_spin_done:
        ; Optionally redraw final triangle in original position
        ; (or leave it wherever it stopped)

        rts
_spin_angle: .byte 0

;---------------------------------------------------------------------------------------
; Label strings
;---------------------------------------------------------------------------------------
_d40_lbl_sq:       .text "SQR",$00
_d40_lbl_rc:       .text "RECT",$00
_d40_lbl_pg:       .text "PENT",$00
_d40_lbl_tr:       .text "TRI",$00
_d40_lbl_ci:       .text "CIRC",$00
_d40_lbl_el:       .text "ELIP",$00
_d40_lbl_outline:  .text "OUTLINE:",$00
_d40_lbl_gradient: .text "GRADIENT:",$00
_d40_dir1:         .text "L-R",$00
_d40_dir2:         .text "T-B",$00
_d40_dir3:         .text "R-L",$00
_d40_dir4:         .text "B-T",$00
_d40_dir5:         .text "DIAG",$00
_d40_dir6:         .text "DIAG",$00

;---------------------------------------------------------------------------------------
; Messages
;---------------------------------------------------------------------------------------
_d40_msg1:
        .text "320x200 with 256 colors",$00
_d40_msg2:
        .text "SPACE to continue",$00