;=======================================================================================
; FCM Characters and Drawing
;=======================================================================================

        .cpu "45gs02"

;=======================================================================================
; Constants
;=======================================================================================
BORDERCOL               = $D020
BACKCOL                 = $D021
VIC4_KEY                = $D02F
VIC3_CTRL               = $D031
VIC4_CTRL               = $D054
VIC4_LINESTPLSB         = $D058
VIC4_LINESTPMSB         = $D059
VIC4_SCRNPTRLSB         = $D060
VIC4_SCRNPTRMSB         = $D061
VIC4_SCRBPTRBNK         = $D062
VIC4_COLPTRLSB          = $D064
VIC4_COLPTRMSB          = $D065
VIC4_TEXTXPOS           = $D04C
VIC4_TEXTYPOS           = $D04E
VIC4_CHRCOUNT           = $D05E
VIC4_DISPROWS           = $D07B

SCREEN_RAM              = $10000
CHAR_DATA               = $40000
CHAR_CODE_BASE          = $1000         ; $40000/64
PTR = $FC                               ; 4-byte pointer

MODE_BASIC      = 0
MODE_TEXT40     = 1
MODE_TEXT80     = 2
MODE_BITMAP40   = 3
MODE_BITMAP80   = 4

;=======================================================================================

        * = $2001

        ; BASIC stub
        .word (+), 2025
        .byte $fe, $02, $30     ; BANK 0
        .byte ':'
        .byte $9e               ; SYS
        .text "8210"            ; Start address
        .byte 0
+       .word 0

        * = $2012

        jmp main

screen_mode:
        .byte 80        ; 40 or 80 col


;=======================================================================================
; main entry
;=======================================================================================
main:

        ; Set background and border
        lda #$01                ; screen color
        sta BACKCOL
        lda #$05                ; border
        sta BORDERCOL

        jsr demo_text40
        jsr demo_text80
        jsr demo_bitmap40
        jsr demo_bitmap80

        lda #MODE_BASIC         ; Return to BASIC mode
        jsr set_screen_mode
        rts

msg:
        .text "FCM Demo - std PETSCII gfx",$00

msg2:
        .text "FCM Demo - 320x200 with 256 colors!",$00

msg3:
        .text "FCM Demo - 640x200 with 256 colors!",$00

msg_space
        .text "SPACE BAR to next demo screen",$00

_allchars:
        .byte $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F
        .byte $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $1A, $1B, $1C, $1D, $1E, $1F
        .byte $20, $21, $22, $23, $24, $25, $26, $27, $28, $29, $2A, $2B, $2C, $2D, $2E, $2F
        .byte $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, $3A, $3B, $3C, $3D, $3E, $3F
        .byte $40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4A, $4B, $4C, $4D, $4E, $4F
        .byte $50, $51, $52, $53, $54, $55, $56, $57, $58, $59, $5A, $5B, $5C, $5D, $5E, $5F
        .byte $60, $61, $62, $63, $64, $65, $66, $67, $68, $69, $6A, $6B, $6C, $6D, $6E, $6F
        .byte $70, $71, $72, $73, $74, $75, $76, $77, $78, $79, $7A, $7B, $7C, $7D, $7E, $7F
        .byte $80, $81, $82, $83, $84, $85, $86, $87, $88, $89, $8A, $8B, $8C, $8D, $8E, $8F
        .byte $90, $91, $92, $93, $94, $95, $96, $97, $98, $99, $9A, $9B, $9C, $9D, $9E, $9F
        .byte $A0, $A1, $A2, $A3, $A4, $A5, $A6, $A7, $A8, $A9, $AA, $AB, $AC, $AD, $AE, $AF
        .byte $B0, $B1, $B2, $B3, $B4, $B5, $B6, $B7, $B8, $B9, $BA, $BB, $BC, $BD, $BE, $BF
        .byte $C0, $C1, $C2, $C3, $C4, $C5, $C6, $C7, $C8, $C9, $CA, $CB, $CC, $CD, $CE, $CF
        .byte $D0, $D1, $D2, $D3, $D4, $D5, $D6, $D7, $D8, $D9, $DA, $DB, $DC, $DD, $DE, $DF
        .byte $E0, $E1, $E2, $E3, $E4, $E5, $E6, $E7, $E8, $E9, $EA, $EB, $EC, $ED, $EE, $EF
        .byte $F0, $F1, $F2, $F3, $F4, $F5, $F6, $F7, $F8, $F9, $FA, $FB, $FC, $FD, $FE, $FF
        .byte $00

.include "fcm.asm"
.include "text.asm"
.include "bitmap.asm"

demo_text40:
        lda #MODE_TEXT40
        jsr set_screen_mode

        lda #$00                ; space
        jsr clear_screen_ram

        jsr init_palette

        ; draw some custom chars
        lda #1
        ldx #10
        ldy #10
        jsr draw_char

        lda #2
        ldx #11
        ldy #10
        jsr draw_char

        ; draw folder

        ; Set folder palette colors
        lda #$A1                ; dark yellow (back/tab)
        ldx #$0C
        ldy #$09
        ldz #$00
        jsr set_palette_color

        lda #$A2                ; light yellow (front panel)
        ldx #$0F
        ldy #$0C
        ldz #$02
        jsr set_palette_color

        lda #$A3                ; white (document)
        ldx #$0F
        ldy #$0F
        ldz #$0F
        jsr set_palette_color

        lda #$A4                ; grey (document fold)
        ldx #$09
        ldy #$09
        ldz #$09
        jsr set_palette_color

        ; Draw folder at row 5, col 5 (3×3 chars)
        lda #6                  ; folder_tl
        ldx #5
        ldy #5
        jsr draw_char

        lda #7                  ; folder_tc
        ldx #5
        ldy #6
        jsr draw_char

        lda #8                  ; folder_tr
        ldx #5
        ldy #7
        jsr draw_char

        lda #9                  ; folder_ml
        ldx #6
        ldy #5
        jsr draw_char

        lda #10                 ; folder_mc
        ldx #6
        ldy #6
        jsr draw_char

        lda #11                 ; folder_mr
        ldx #6
        ldy #7
        jsr draw_char

        lda #12                 ; folder_bl
        ldx #7
        ldy #5
        jsr draw_char

        lda #13                 ; folder_bc
        ldx #7
        ldy #6
        jsr draw_char

        lda #14                 ; folder_br
        ldx #7
        ldy #7
        jsr draw_char

        ; print a string using petscii ROM characters

        lda #<msg
        sta str_ptr
        lda #>msg
        sta str_ptr+1
        lda #0                  ; row 0
        sta str_row
        lda #0                 ; column 0
        sta str_col
        lda #$00                ; black
        sta str_color
        jsr draw_petscii_string

        lda #<msg_space
        sta str_ptr
        lda #>msg_space
        sta str_ptr+1
        lda #1                  ; row 0
        sta str_row
        lda #0                 ; column 0
        sta str_col
        lda #$00                ; black
        sta str_color
        jsr draw_petscii_string

        _loop:
        jsr $FFE4
        cmp #' '
        bne _loop

        rts

demo_text80:

        lda #MODE_TEXT80
        jsr set_screen_mode
        jsr init_palette

        lda #$00                ; space
        jsr clear_screen_ram

        ; draw some custom chars
        lda #1
        ldx #10
        ldy #10
        jsr draw_char

        lda #2
        ldx #11
        ldy #10
        jsr draw_char

        ; print a string using petscii ROM characters

        lda #<msg
        sta str_ptr
        lda #>msg
        sta str_ptr+1
        lda #0                  ; row 0
        sta str_row
        lda #0                 ; column 0
        sta str_col
        lda #$00                ; black
        sta str_color
        jsr draw_petscii_string

        lda #<msg_space
        sta str_ptr
        lda #>msg_space
        sta str_ptr+1
        lda #1                  ; row 1
        sta str_row
        lda #0                 ; column 0
        sta str_col
        lda #$00                ; black
        sta str_color
        jsr draw_petscii_string

        _loop:
        jsr $FFE4
        cmp #' '
        bne _loop

        rts

demo_bitmap40:

        lda #MODE_BITMAP40
        jsr set_screen_mode

        lda #<msg2
        sta str_ptr
        lda #>msg2
        sta str_ptr+1
        lda #0                  ; row 0
        sta str_row
        lda #0                 ; column 0
        sta str_col
        lda #$00                ; black
        sta str_color
        jsr draw_petscii_string

        lda #<msg_space
        sta str_ptr
        lda #>msg_space
        sta str_ptr+1
        lda #1                  ; row 1
        sta str_row
        lda #0                 ; column 0
        sta str_col
        lda #$00                ; black
        sta str_color
        jsr draw_petscii_string

        ; Plot a red pixel at (100, 50)
        lda #100
        sta plot_x
        lda #0
        sta plot_x+1
        lda #50
        sta plot_y
        lda #$02                ; Red (palette entry 2)
        sta plot_col
        jsr plot_pixel

        ; Draw a red horizontal line -  0,104 to 319,104
        lda #0
        sta line_x0
        lda #0
        sta line_x0+1
        lda #104
        sta line_y0

        lda #<319
        sta line_x1
        lda #>319
        sta line_x1+1
        lda #104
        sta line_y1

        lda #$02                ; Red
        sta line_col
        jsr draw_line

        ; Draw an outline rectangle (CLC)
        lda #50
        sta rect_x
        lda #0
        sta rect_x+1
        lda #40
        sta rect_y
        lda #80
        sta rect_w
        lda #0
        sta rect_w+1
        lda #60
        sta rect_h
        lda #$08                ; White
        sta rect_col
        clc                     ; Outline only
        jsr draw_rect

        ; Draw a filled rectangle (SEC)
        lda #100
        sta rect_x
        lda #0
        sta rect_x+1
        lda #80
        sta rect_y
        lda #50
        sta rect_w
        lda #0
        sta rect_w+1
        lda #40
        sta rect_h
        lda #$AA                ; black
        sta rect_col
        sec                     ; Filled
        jsr draw_rect

        ; draw diagnoal line
        lda #0
        sta line_x0
        sta line_x0+1
        sta line_y0

        lda #<319
        sta line_x1
        lda #>319
        sta line_x1+1
        lda #199
        sta line_y1

        lda #$08
        sta line_col
        jsr draw_line

        ; Outline circle at (160, 100) radius 50
        lda #<160
        sta circ_cx
        lda #>160
        sta circ_cx+1
        lda #100
        sta circ_cy
        lda #50
        sta circ_r
        lda #$12                ; Red
        sta circ_col
        clc                     ; Outline
        jsr draw_circle

        ; Filled circle at (200, 80) radius 30
        lda #<200
        sta circ_cx
        lda #>200
        sta circ_cx+1
        lda #80
        sta circ_cy
        lda #30
        sta circ_r
        lda #$08                ; Orange
        sta circ_col
        sec                     ; Filled
        jsr draw_circle

        ; Draw vertical lines showing predefined palette colors
        lda #$01                ; start at 1 (skip $00 = transparent)
        sta _cur_color
        lda #50
        sta _cur_x
_draw_colorful_lines:
        lda _cur_x
        sta line_x0
        lda #0
        sta line_x0+1
        lda #120
        sta line_y0

        lda _cur_x
        sta line_x1
        lda #0
        sta line_x1+1
        lda #190
        sta line_y1

        lda _cur_color
        sta line_col
        jsr draw_line

        inc _cur_color
        lda _cur_color
        cmp #32                 ; stop after last predefined color
        beq _grad
        inc _cur_x
        jmp _draw_colorful_lines

_grad:
        lda #0
        sta grad_dir
        lda #<200
        sta grad_x
        lda #>200
        sta grad_x+1
        lda #50
        sta grad_y
        lda #50
        sta grad_w
        lda #0
        sta grad_w+1
        lda #50
        sta grad_h
        lda #$0F                ; start: white (15,15,15)
        sta grad_r1
        sta grad_g1
        sta grad_b1
        lda #$0F                ; end: red (15,0,0)
        sta grad_r2
        lda #$00
        sta grad_g2
        sta grad_b2
        lda #$80
        sta grad_pal
        jsr draw_gradient_rect

        ; Vertical gradient: blue to green, top-to-bottom
        lda #1                  ; vertical
        sta grad_dir
        lda #<100
        sta grad_x
        lda #>100
        sta grad_x+1
        lda #20
        sta grad_y
        lda #80
        sta grad_w
        lda #0
        sta grad_w+1
        lda #50
        sta grad_h
        lda #$00
        sta grad_r1
        sta grad_r2
        lda #$00
        sta grad_g1
        lda #$0F
        sta grad_g2
        lda #$0F
        sta grad_b1
        lda #$00
        sta grad_b2
        lda #$C0
        sta grad_pal
        jsr draw_gradient_rect
        

_grad_done:

_loop:
        jsr $FFE4
        cmp #' '
        bne _loop

        rts

_cur_color:
        .byte $00
_cur_x:
        .byte 50


demo_bitmap80:

        lda #MODE_BITMAP80
        jsr set_screen_mode
        
        ; print a string using petscii ROM characters

        lda #<msg3
        sta str_ptr
        lda #>msg3
        sta str_ptr+1
        lda #0                  ; row 0
        sta str_row
        lda #0                 ; column 0
        sta str_col
        lda #$00                ; black
        sta str_color
        jsr draw_petscii_string

        lda #<msg_space
        sta str_ptr
        lda #>msg_space
        sta str_ptr+1
        lda #1                  ; row 1
        sta str_row
        lda #0                 ; column 0
        sta str_col
        lda #$00                ; black
        sta str_color
        jsr draw_petscii_string

        ; Plot a red pixel at (100, 50)
        lda #100
        sta plot_x
        lda #0
        sta plot_x+1
        lda #50
        sta plot_y
        lda #$02                ; Red (palette entry 2)
        sta plot_col
        jsr plot_pixel

        ; Draw a red line from (0, 0) to (319, 199)
        lda #0
        sta line_x0
        lda #0
        sta line_x0+1
        lda #104
        sta line_y0

        lda #<639
        sta line_x1
        lda #>639
        sta line_x1+1
        lda #104
        sta line_y1

        lda #$02                ; Red
        sta line_col
        jsr draw_line

        ; Draw an outline rectangle (CLC)
        lda #50
        sta rect_x
        lda #0
        sta rect_x+1
        lda #40
        sta rect_y
        lda #80
        sta rect_w
        lda #0
        sta rect_w+1
        lda #60
        sta rect_h
        lda #$08                ; White
        sta rect_col
        clc                     ; Outline only
        jsr draw_rect

        ; Draw a filled rectangle (SEC)
        lda #100
        sta rect_x
        lda #0
        sta rect_x+1
        lda #80
        sta rect_y
        lda #50
        sta rect_w
        lda #0
        sta rect_w+1
        lda #40
        sta rect_h
        lda #$AA                ; black
        sta rect_col
        sec                     ; Filled
        jsr draw_rect

        ; draw diagnoal line
        lda #0
        sta line_x0
        sta line_x0+1
        sta line_y0

        lda #<639
        sta line_x1
        lda #>639
        sta line_x1+1
        lda #199
        sta line_y1

        lda #$02
        sta line_col
        jsr draw_line

; Outline circle at (360, 100) radius 50
        lda #<360
        sta circ_cx
        lda #>360
        sta circ_cx+1
        lda #100
        sta circ_cy
        lda #50
        sta circ_r
        lda #$12                ; Red
        sta circ_col
        clc                     ; Outline
        jsr draw_circle

        ; Filled circle at (500, 80) radius 30
        lda #<500
        sta circ_cx
        lda #>500
        sta circ_cx+1
        lda #80
        sta circ_cy
        lda #30
        sta circ_r
        lda #$08                ; Orange
        sta circ_col
        sec                     ; Filled
        jsr draw_circle


        ; Draw vertical lines showing predefined palette colors
        lda #$01                ; start at 1 (skip $00 = transparent)
        sta _cur_color
        lda #50
        sta _cur_x
_draw_colorful_lines:
        lda _cur_x
        sta line_x0
        lda #0
        sta line_x0+1
        lda #120
        sta line_y0

        lda _cur_x
        sta line_x1
        lda #0
        sta line_x1+1
        lda #190
        sta line_y1

        lda _cur_color
        sta line_col
        jsr draw_line

        inc _cur_color
        lda _cur_color
        cmp #32                 ; stop after last predefined color
        beq _grad
        inc _cur_x
        jmp _draw_colorful_lines

_grad:
        lda #0
        sta grad_dir
        lda #<200
        sta grad_x
        lda #>200
        sta grad_x+1
        lda #50
        sta grad_y
        lda #50
        sta grad_w
        lda #0
        sta grad_w+1
        lda #50
        sta grad_h
        lda #$0F                ; start: white (15,15,15)
        sta grad_r1
        sta grad_g1
        sta grad_b1
        lda #$0F                ; end: blue (0,0,15)
        sta grad_b2
        lda #$00
        sta grad_g2
        sta grad_r2
        lda #$80
        sta grad_pal
        jsr draw_gradient_rect

        ; Vertical gradient: blue to green, top-to-bottom
        lda #1                  ; vertical
        sta grad_dir
        lda #<100
        sta grad_x
        lda #>100
        sta grad_x+1
        lda #20
        sta grad_y
        lda #80
        sta grad_w
        lda #0
        sta grad_w+1
        lda #50
        sta grad_h
        lda #$00
        sta grad_r1
        sta grad_r2
        lda #$00
        sta grad_g1
        lda #$0F
        sta grad_g2
        lda #$0F
        sta grad_b1
        lda #$00
        sta grad_b2
        lda #$C0
        sta grad_pal
        jsr draw_gradient_rect

_loop_ending:
        jsr $FFE4
        cmp #' '
        bne _loop_ending

        ; Full-screen white to red gradient
        lda #0
        sta grad_x
        sta grad_x+1
        sta grad_y
        sta grad_dir            ; horizontal
        lda #<640
        sta grad_w
        lda #>640
        sta grad_w+1            ; must set both bytes!
        lda #200
        sta grad_h
        lda #$0F
        sta grad_r1
        sta grad_g1
        sta grad_b1             ; white (15,15,15)
        lda #$0F
        sta grad_r2
        lda #$00
        sta grad_g2
        sta grad_b2             ; red (15,0,0)
        lda #$80
        sta grad_pal
        jsr draw_gradient_rect

_grad_done:

_loop:
        jsr $FFE4
        cmp #' '
        bne _loop

        rts

_cur_color:
        .byte $00
_cur_x:
        .byte 50

