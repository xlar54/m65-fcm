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
        jsr demo40
        jsr demo80
        jsr demo_colorwheel
        jsr demo_solar

        lda #MODE_BASIC         ; Return to BASIC mode
        jsr set_screen_mode
        rts

msg:
        .text "FCM Demo - std PETSCII gfx",$00

msg2:
        .text "FCM Demo - 320x200 with 256 colors!",$00

msg3:
        .text "FCM Demo - 640x200 with 256 colors!",$00

msg_space:
        .text "SPACE BAR to next demo screen",$00

WAIT_SPACEBAR:
        jsr $FFE4
        cmp #' '
        bne WAIT_SPACEBAR
        rts

; libraries (required)
.include "fcm.asm" 
.include "text.asm"
.include "bitmap.asm"
.include "gradient.asm"
.include "gradfill.asm"

; libraries (optional)
.include "rectangle.asm"
.include "circle.asm"
.include "ellipse.asm"
.include "polygon.asm"

; demos
.include "demo_text_40.asm"
.include "demo_text_80.asm"
.include "demo_bitmap_320x200.asm"
.include "demo_bitmap_640x200.asm"
.include "demo_colorwheel.asm"
.include "demo_solar.asm"





