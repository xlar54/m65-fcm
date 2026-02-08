;=======================================================================================
; demo80.asm - Shape showcase demo for 640x200 FCM bitmap mode
;
; Same layout as demo40 but for 80-column (640x200) mode.
; Displays two rows of shapes:
;   Top row:    Outline drawings (square, rectangle, pentagon, triangle, circle, ellipse)
;   Bottom row: Same shapes filled with angular gradients, alternating directions:
;               SQR=left-right (angle 64), RECT=top-bottom (angle 0),
;               PENT=right-left (angle 192), TRI=bottom-top (angle 128),
;               CIRC=diagonal TL-BR (angle 32), ELIP=diagonal TR-BL (angle 224)
;
; Note: circles, ellipses, and polygons auto-double X offsets in 80-col mode,
; so radii stay the same as 40-col. Only center X and rect positions are doubled.
;=======================================================================================

;=======================================================================================
; demo80 - Main entry point
;=======================================================================================
demo_bitmap_80:

        ; Set background and border
        lda #$01                ; screen color
        sta BACKCOL
        lda #$05                ; border
        sta BORDERCOL

        lda #MODE_BITMAP80
        jsr set_screen_mode

        ;===================================================================
        ; Title text
        ;===================================================================
        lda #<_d80_msg1
        sta str_ptr
        lda #>_d80_msg1
        sta str_ptr+1
        lda #0
        sta str_row
        lda #28
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d80_msg2
        sta str_ptr
        lda #>_d80_msg2
        sta str_ptr+1
        lda #1
        sta str_row
        lda #31
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        ;===================================================================
        ; Outline shape colors
        ;===================================================================
        lda #$10
        ldx #$0F
        ldy #$02
        ldz #$02
        jsr set_palette_color   ; red (square)

        lda #$11
        ldx #$02
        ldy #$0D
        ldz #$02
        jsr set_palette_color   ; green (rectangle)

        lda #$12
        ldx #$02
        ldy #$04
        ldz #$0F
        jsr set_palette_color   ; blue (pentagon)

        lda #$13
        ldx #$0E
        ldy #$0D
        ldz #$01
        jsr set_palette_color   ; yellow (triangle)

        lda #$14
        ldx #$01
        ldy #$0C
        ldz #$0E
        jsr set_palette_color   ; cyan (circle)

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

        ;--- 1. Square (outline) at (20, 35) size 80x40 ---
        lda #<20
        sta rect_x
        lda #>20
        sta rect_x+1
        lda #35
        sta rect_y
        lda #80
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

        ;--- 2. Rectangle (outline) at (124, 40) size 96x30 ---
        lda #<124
        sta rect_x
        lda #>124
        sta rect_x+1
        lda #40
        sta rect_y
        lda #96
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

        ;--- 3. Pentagon (outline) at (280, 55) r=22 ---
        lda #<280
        sta poly_cx
        lda #>280
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
        lda #218
        sta poly_angle
        clc
        jsr draw_polygon

        ;--- 4. Triangle (outline) at (384, 57) r=22 ---
        lda #<384
        sta poly_cx
        lda #>384
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
        lda #149
        sta poly_angle
        clc
        jsr draw_polygon

        ;--- 5. Circle (outline) at (484, 55) r=22 ---
        lda #<484
        sta circ_cx
        lda #>484
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

        ;--- 6. Ellipse (outline) at (590, 55) rx=22 ry=18 ---
        lda #<590
        sta elip_cx
        lda #>590
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
        lda #<639
        sta line_x1
        lda #>639
        sta line_x1+1
        lda #94
        sta line_y1
        lda #$16
        sta line_col
        jsr draw_line

        ;===================================================================
        ; Section labels
        ;===================================================================
        lda #<_d80_lbl_outline
        sta str_ptr
        lda #>_d80_lbl_outline
        sta str_ptr+1
        lda #2
        sta str_row
        lda #0
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d80_lbl_gradient
        sta str_ptr
        lda #>_d80_lbl_gradient
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

        lda #<60
        sta gf_cx
        lda #>60
        sta gf_cx+1
        lda #125
        sta gf_cy
        lda #20
        sta gf_radius
        lda #64
        sta gf_angle
        jsr gf_setup

        lda #<20
        sta rect_x
        lda #>20
        sta rect_x+1
        lda #105
        sta rect_y
        lda #80
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

        lda #<172
        sta gf_cx
        lda #>172
        sta gf_cx+1
        lda #120
        sta gf_cy
        lda #15
        sta gf_radius
        lda #0
        sta gf_angle
        jsr gf_setup

        lda #<124
        sta rect_x
        lda #>124
        sta rect_x+1
        lda #105
        sta rect_y
        lda #96
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

        lda #<280
        sta gf_cx
        lda #>280
        sta gf_cx+1
        lda #125
        sta gf_cy
        lda #22
        sta gf_radius
        lda #192
        sta gf_angle
        jsr gf_setup

        lda #<280
        sta poly_cx
        lda #>280
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
        lda #218
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

        lda #<384
        sta gf_cx
        lda #>384
        sta gf_cx+1
        lda #127
        sta gf_cy
        lda #22
        sta gf_radius
        lda #128
        sta gf_angle
        jsr gf_setup

        lda #<384
        sta poly_cx
        lda #>384
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

        lda #<484
        sta gf_cx
        lda #>484
        sta gf_cx+1
        lda #125
        sta gf_cy
        lda #22
        sta gf_radius
        lda #32
        sta gf_angle
        jsr gf_setup

        lda #<484
        sta circ_cx
        lda #>484
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

        lda #<590
        sta gf_cx
        lda #>590
        sta gf_cx+1
        lda #125
        sta gf_cy
        lda #22
        sta gf_radius
        lda #224
        sta gf_angle
        jsr gf_setup

        lda #<590
        sta elip_cx
        lda #>590
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
        lda #<_d80_lbl_sq
        sta str_ptr
        lda #>_d80_lbl_sq
        sta str_ptr+1
        lda #23
        sta str_row
        lda #3
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d80_lbl_rc
        sta str_ptr
        lda #>_d80_lbl_rc
        sta str_ptr+1
        lda #23
        sta str_row
        lda #16
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d80_lbl_pg
        sta str_ptr
        lda #>_d80_lbl_pg
        sta str_ptr+1
        lda #23
        sta str_row
        lda #33
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d80_lbl_tr
        sta str_ptr
        lda #>_d80_lbl_tr
        sta str_ptr+1
        lda #23
        sta str_row
        lda #46
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d80_lbl_ci
        sta str_ptr
        lda #>_d80_lbl_ci
        sta str_ptr+1
        lda #23
        sta str_row
        lda #58
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d80_lbl_el
        sta str_ptr
        lda #>_d80_lbl_el
        sta str_ptr+1
        lda #23
        sta str_row
        lda #72
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        ;--- Direction labels (row 24) ---
        lda #<_d80_dir1
        sta str_ptr
        lda #>_d80_dir1
        sta str_ptr+1
        lda #24
        sta str_row
        lda #3
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d80_dir2
        sta str_ptr
        lda #>_d80_dir2
        sta str_ptr+1
        lda #24
        sta str_row
        lda #17
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d80_dir3
        sta str_ptr
        lda #>_d80_dir3
        sta str_ptr+1
        lda #24
        sta str_row
        lda #33
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d80_dir4
        sta str_ptr
        lda #>_d80_dir4
        sta str_ptr+1
        lda #24
        sta str_row
        lda #46
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d80_dir5
        sta str_ptr
        lda #>_d80_dir5
        sta str_ptr+1
        lda #24
        sta str_row
        lda #58
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        lda #<_d80_dir6
        sta str_ptr
        lda #>_d80_dir6
        sta str_ptr+1
        lda #24
        sta str_row
        lda #72
        sta str_col
        lda #$00
        sta str_color
        jsr draw_petscii_string

        ;===================================================================
        ; Wait for spacebar
        ;===================================================================
        jsr WAIT_SPACEBAR
        rts

;---------------------------------------------------------------------------------------
; Label strings
;---------------------------------------------------------------------------------------
_d80_lbl_sq:       .text "SQR",$00
_d80_lbl_rc:       .text "RECT",$00
_d80_lbl_pg:       .text "PENT",$00
_d80_lbl_tr:       .text "TRI",$00
_d80_lbl_ci:       .text "CIRC",$00
_d80_lbl_el:       .text "ELIP",$00
_d80_lbl_outline:  .text "OUTLINE:",$00
_d80_lbl_gradient: .text "GRADIENT:",$00
_d80_dir1:         .text "L-R",$00
_d80_dir2:         .text "T-B",$00
_d80_dir3:         .text "R-L",$00
_d80_dir4:         .text "B-T",$00
_d80_dir5:         .text "DIAG",$00
_d80_dir6:         .text "DIAG",$00

;---------------------------------------------------------------------------------------
; Messages
;---------------------------------------------------------------------------------------
_d80_msg1:
        .text "640x200 with 256 colors",$00
_d80_msg2:
        .text "SPACE to continue",$00