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

SCREEN_RAM              = $10000
CHAR_DATA               = $40000
CHAR_CODE_BASE          = $1000         ; $40000/64
PTR = $FC                               ; 4-byte pointer

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
        lda #40
        sta screen_mode
        jsr fcm_init

        ; Set background and border
        lda #$01                ; screen color
        sta BACKCOL
        lda #$07                ; border
        sta BORDERCOL

        lda #$04                ; space
        jsr clear_screen_ram
        jsr clear_color_ram
        jsr set_palette

        ; draw some custom chars
        lda #0
        ldx #0
        ldy #0
        jsr draw_char

        lda #<_msg
        sta str_ptr
        lda #>_msg
        sta str_ptr+1
        lda #5                  ; row 5
        sta str_row
        lda #10                 ; column 10
        sta str_col
        lda #$00                ; black
        sta str_color
        jsr draw_petscii_string

_loop:
        ; change colors of character in loop
        lda #1
        ldx _color_val

        inc _color_val
        bne _not_zero       ; if it didn't wrap to 0, continue
        inc _color_val      ; skip $00, go to $01
_not_zero:
        ldy _color_val
        jsr set_color

        jsr $FFE4
        cmp #' '
        bne _loop

        jsr demo_bitmap40
        jsr exit_fcm
        jsr demo_bitmap80
        jsr exit_fcm
        rts

_color_val:
        .byte $01

_msg:
        .text "fcm demo"
        .byte 0                 ; null terminator

.include "fcm.asm"
.include "text.asm"
.include "bitmap.asm"

demo_bitmap40:

        lda #40
        sta screen_mode

        jsr fcm_init
        jsr clear_color_ram
        jsr init_bitmap         ; Set up screen codes
        
        ; Set background and border
        lda #$01                ; screen color
        sta BACKCOL
        lda #$07                ; border
        sta BORDERCOL

        lda #$00                ; Clear to background
        jsr clear_bitmap
        
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
        
        ; Plot a white pixel at (200, 100)
        lda #200
        sta plot_x
        lda #0
        sta plot_x+1
        lda #100
        sta plot_y
        lda #$02                ; red
        sta plot_col
        jsr plot_pixel


        ; Draw a red line from (0, 0) to (319, 199)
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

_loop:
        jsr $FFE4
        cmp #' '
        bne _loop

        rts


demo_bitmap80:

        lda #80
        sta screen_mode

        jsr fcm_init
        jsr clear_color_ram
        jsr init_bitmap         ; Set up screen codes
        
        ; Set background and border
        lda #$01                ; screen color
        sta BACKCOL
        lda #$07                ; border
        sta BORDERCOL

        lda #$00                ; Clear to background
        jsr clear_bitmap
        
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
        
        ; Plot a white pixel at (200, 100)
        lda #200
        sta plot_x
        lda #0
        sta plot_x+1
        lda #100
        sta plot_y
        lda #$02                ; red
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

_loop:
        jsr $FFE4
        cmp #' '
        bne _loop

        rts
