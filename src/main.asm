;=======================================================================================
; FCM/NCM Characters and Drawing - Main Entry Point
;=======================================================================================

        .cpu "45gs02"

;=======================================================================================
; Constants
;=======================================================================================
BORDERCOL               = $D020
BACKCOL                 = $D021
VIC4_KEY                = $D02F
VIC3_CTRL               = $D031
VIC_TBDRPOS             = $D048 ;  (top border)
VIC_BBDRPOS             = $D049 ;  (bottom border)
VIC4_CTRL               = $D054
VIC4_LINESTPLSB         = $D058
VIC4_LINESTPMSB         = $D059
VIC_SDBDRWD             = $D05C ; (side border width)
VIC4_SCRNPTRLSB         = $D060
VIC4_SCRNPTRMSB         = $D061
VIC4_SCRBPTRBNK         = $D062
VIC4_COLPTRLSB          = $D064
VIC4_COLPTRMSB          = $D065
VIC4_TEXTXPOS           = $D04C
VIC4_TEXTYPOS           = $D04E
VIC4_CHRCOUNT           = $D05E
VIC4_DISPROWS           = $D07B

; Hardware math registers
MULTINA     = $D770     ; Multiplier/Divider input A (32-bit)
MULTINB     = $D774     ; Multiplier/Divider input B (32-bit)
MULTOUT     = $D778     ; Multiply output (64-bit)
DIVOUT      = $D768     ; Divide output (64-bit: frac in 0-3, whole in 4-7)
DIVBUSY     = $D70F     ; Bit 7 = divider busy flag


SCREEN_RAM              = $10000
CHAR_DATA               = $40000
CHAR_CODE_BASE          = $1000         ; $40000/64
PTR                     = $FC           ; 4-byte pointer

; Screen modes
MODE_BASIC      = 0
MODE_TEXT40     = 1
MODE_TEXT80     = 2
MODE_BITMAP40   = 3
MODE_BITMAP80   = 4
MODE_NCM40      = 5     ; NCM 40-col (40 screen positions, 20 NCM chars visible)
MODE_NCM80      = 6     ; NCM 80-col (80 screen positions, 40 NCM chars visible)

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

;=======================================================================================
; main entry
;=======================================================================================
main:
        ; Text mode demos
        jsr demo_text40
        jsr restore_default_screen

        jsr demo_text80
        jsr restore_default_screen

        ; Bitmap mode demos (pixel plotting)
        jsr demo_bitmap_40
        jsr restore_default_screen

        jsr demo_bitmap_80 
        jsr restore_default_screen

        ; NCM demos (colorful character tiles)
        jsr demo_ncm_40
        jsr restore_default_screen

        jsr demo_ncm_40_scroll
        jsr restore_default_screen

        ;jsr demo_ncm_80
        ;jsr restore_default_screen

        ; Additional demos
        jsr demo_floodfill
        jsr restore_default_screen

        jsr demo_abstract
        jsr restore_default_screen

        jsr demo_lines
        jsr restore_default_screen

        jsr demo_cube
        jsr restore_default_screen

        jsr demo_colorwheel
        jsr restore_default_screen

        jsr demo_solar
        jsr restore_default_screen

        ; Return to BASIC
        lda #MODE_BASIC
        jsr set_screen_mode
        rts

;---------------------------------------------------------------------------------------
; Messages
;---------------------------------------------------------------------------------------
msg:
        .text "FCM Demo - std PETSCII gfx",$00

msg2:
        .text "FCM Demo - 320x200 with 256 colors!",$00

msg3:
        .text "FCM Demo - 640x200 with 256 colors!",$00

msg4:
        .text "NCM Demo - 320 wide, 16 colors/char!",$00

msg5:
        .text "NCM Demo - 640 wide, 16 colors/char!",$00

msg_space:
        .text "SPACE BAR to next demo screen",$00

;---------------------------------------------------------------------------------------
; Wait for spacebar
;---------------------------------------------------------------------------------------
WAIT_SPACEBAR:
        jsr $FFE4
        cmp #' '
        bne WAIT_SPACEBAR
        rts

;=======================================================================================
; Include library files
;=======================================================================================

; Core libraries (required)
.include "fcm.asm" 
.include "ncm.asm"
.include "ncm_scroll.asm"
.include "text.asm"
.include "bitmap.asm"
.include "gradient.asm"
.include "gradfill.asm"

; Shape libraries (optional)
.include "rectangle.asm"
.include "circle.asm"
.include "ellipse.asm"
.include "polygon.asm"
.include "floodfill.asm"

; Demo files
.include "demos/demo_text_40.asm"
.include "demos/demo_text_80.asm"
.include "demos/demo_bitmap_320x200.asm"
.include "demos/demo_bitmap_640x200.asm"
.include "demos/demo_ncm_40.asm"
.include "demos/demo_ncm_40_scroll.asm"
.include "demos/demo_ncm_80.asm"
.include "demos/demo_floodfill.asm"
.include "demos/demo_lines.asm"
.include "demos/demo_abstract.asm"
.include "demos/demo_cube.asm"
.include "demos/demo_colorwheel.asm"
.include "demos/demo_solar.asm"