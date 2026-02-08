;=======================================================================================
; Demo: NCM 40-column (320 pixel width)
;
; Demonstrates NCM characters - each character can have 16 colors!
; NCM chars are 16 pixels wide × 8 tall.
;
; This demo:
; 1. Creates several colorful NCM character patterns
; 2. Displays them on screen in rows
; 3. Shows how NCM enables rich, colorful tiles
;=======================================================================================

SET_NCM_COLOR .macro index, red, green, blue
        lda #\index           ; palette index 0
        ldx #\red           ; red
        ldy #\green         ; green
        ldz #\blue           ; blue
        jsr set_palette_color
.endmacro

demo_ncm_40:
        ; Set NCM 40-column mode
        lda #MODE_NCM40
        jsr set_screen_mode

        ; Print a title
        lda #<_title
        sta str_ptr
        lda #>_title
        sta str_ptr+1
        lda #0                  ; row 0
        sta str_row
        lda #0                 ; column 0
        sta str_col
        lda #$01                ; black
        sta str_color
        jsr draw_petscii_string

        ; Set background color
        lda #0
        sta $D021

        #SET_NCM_COLOR 0,0,15,0
        #SET_NCM_COLOR 1,0,0,0
        #SET_NCM_COLOR 2,15,11,13
        #SET_NCM_COLOR 3,9,6,0
        #SET_NCM_COLOR 4,0,0,6
        #SET_NCM_COLOR 5,6,10,15
        #SET_NCM_COLOR 6,0,15,15
        #SET_NCM_COLOR 7,10,0,10
        #SET_NCM_COLOR 8,15,0,0
        #SET_NCM_COLOR 9,15,11,9
        #SET_NCM_COLOR 10,15,8,0
        #SET_NCM_COLOR 11,15,15,0
        #SET_NCM_COLOR 12,15,15,15
       


        ; Create NCM character 1: Colorful "M"
        lda #0
        ldx #<_ncm_char_mario0
        ldy #>_ncm_char_mario0
        jsr create_ncm_char

        ; Create NCM character 2: Checkerboard
        lda #1
        ldx #<_ncm_char_mario1
        ldy #>_ncm_char_mario1
        jsr create_ncm_char

        ; Create NCM character 2: Checkerboard
        lda #2
        ldx #<_ncm_char_mario2
        ldy #>_ncm_char_mario2
        jsr create_ncm_char

        ; Create NCM character 2: Checkerboard
        lda #3
        ldx #<_ncm_char_mario3
        ldy #>_ncm_char_mario3
        jsr create_ncm_char

        ; Create NCM character 2: Checkerboard
        lda #4
        ldx #<_ncm_char_mario4
        ldy #>_ncm_char_mario4
        jsr create_ncm_char

        ; Create NCM character 2: Checkerboard
        lda #5
        ldx #<_ncm_char_mario5
        ldy #>_ncm_char_mario5
        jsr create_ncm_char

        ; Create NCM character 1: Colorful "M"
        lda #6
        ldx #<_ncm_char_mario6
        ldy #>_ncm_char_mario6
        jsr create_ncm_char

        ; Create NCM character 1: Colorful "M"
        lda #7
        ldx #<_ncm_char_mario7
        ldy #>_ncm_char_mario7
        jsr create_ncm_char

        ; === Display characters on screen ===
        lda #0                  ; Character 1 (left leg)
        ldx #10                 ; Col 10
        ldy #7                 ; Row 10
        jsr set_ncm_char

        lda #1                  ; Character 2 (right leg)
        ldx #11                 ; Col 10
        ldy #7                 ; Row 10
        jsr set_ncm_char
        
        lda #2                  ; Character 1 (left leg)
        ldx #10                 ; Col 10
        ldy #8                 ; Row 10
        jsr set_ncm_char

        lda #3                  ; Character 2 (right leg)
        ldx #11                 ; Col 10
        ldy #8                 ; Row 10
        jsr set_ncm_char

        lda #4                  ; Character 1 (left leg)
        ldx #10                 ; Col 10
        ldy #9                 ; Row 10
        jsr set_ncm_char

        lda #5                  ; Character 2 (right leg)
        ldx #11                 ; Col 10
        ldy #9                 ; Row 10
        jsr set_ncm_char

        lda #6                  ; Character 1 (left leg)
        ldx #10                 ; Col 10
        ldy #10                 ; Row 10
        jsr set_ncm_char

        lda #7                  ; Character 2 (right leg)
        ldx #11                 ; Col 10
        ldy #10                 ; Row 10
        jsr set_ncm_char


        ; Wait for user
        jsr WAIT_SPACEBAR
        rts

_title:
        .text "NCM 40 Column Custom Characters",$00

;---------------------------------------------------------------------------------------
; NCM Character Data (64 bytes each: 8 rows × 8 bytes, 2 pixels per byte)
;---------------------------------------------------------------------------------------

_ncm_char_mario0:
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $07
        .byte $00, $01, $01, $0c, $01, $01, $07, $08
        .byte $00, $01, $0c, $01, $0c, $01, $08, $0a
        .byte $00, $0c, $01, $0c, $0c, $01, $01, $01

_ncm_char_mario1:
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $07, $07, $07, $00, $00, $00, $00, $00
        .byte $0a, $0b, $08, $07, $07, $00, $00, $00
        .byte $0c, $0c, $0a, $08, $08, $07, $00, $00
        .byte $01, $01, $01, $01, $08, $08, $07, $00

_ncm_char_mario2:
        .byte $00, $01, $01, $0c, $01, $01, $01, $01
        .byte $01, $0c, $0c, $01, $01, $01, $01, $01
        .byte $01, $0c, $01, $0c, $0c, $01, $0c, $0c
        .byte $01, $01, $01, $01, $0c, $01, $0c, $01
        .byte $01, $0c, $0c, $0c, $0c, $01, $0c, $01
        .byte $00, $07, $07, $07, $07, $09, $09, $09
        .byte $00, $07, $08, $08, $07, $01, $01, $01
        .byte $00, $07, $08, $08, $07, $01, $01, $01

_ncm_char_mario3:
        .byte $01, $01, $01, $01, $01, $08, $07, $00
        .byte $01, $01, $01, $01, $01, $01, $07, $00
        .byte $09, $09, $0c, $0c, $01, $01, $07, $00
        .byte $09, $09, $01, $0c, $09, $09, $01, $09
        .byte $09, $09, $01, $0c, $09, $09, $01, $09
        .byte $09, $09, $09, $09, $09, $01, $01, $09
        .byte $09, $09, $09, $01, $01, $09, $01, $09
        .byte $01, $01, $01, $01, $01, $09, $03, $00

_ncm_char_mario4:
        .byte $00, $07, $08, $08, $07, $09, $01, $01
        .byte $00, $07, $08, $08, $08, $07, $09, $09
        .byte $00, $00, $07, $08, $08, $07, $03, $03
        .byte $00, $00, $07, $08, $08, $08, $05, $05
        .byte $00, $00, $07, $07, $08, $08, $05, $06
        .byte $00, $00, $00, $07, $04, $05, $06, $06
        .byte $00, $00, $00, $04, $06, $0c, $0c, $06
        .byte $00, $00, $00, $04, $06, $0c, $0c, $06

_ncm_char_mario5:
        .byte $01, $01, $01, $01, $09, $03, $00, $00
        .byte $09, $09, $09, $03, $03, $00, $00, $00
        .byte $03, $03, $03, $08, $07, $00, $00, $00
        .byte $08, $08, $05, $05, $08, $07, $00, $00
        .byte $07, $07, $06, $05, $07, $01, $01, $00
        .byte $06, $06, $06, $06, $05, $0c, $0c, $01
        .byte $06, $06, $06, $0c, $0c, $04, $0c, $01
        .byte $06, $06, $06, $0c, $0c, $04, $0c, $01

_ncm_char_mario6:
        .byte $00, $00, $00, $04, $06, $06, $06, $06 
        .byte $00, $00, $04, $05, $06, $06, $06, $05 
        .byte $00, $00, $04, $05, $06, $06, $05, $04 
        .byte $00, $00, $04, $05, $05, $05, $04, $00 
        .byte $00, $00, $04, $04, $04, $04, $00, $00 
        .byte $00, $01, $03, $03, $03, $01, $00, $00 
        .byte $01, $02, $03, $03, $03, $01, $00, $00 
        .byte $01, $01, $01, $01, $01, $01, $00, $00 
        
_ncm_char_mario7:
        .byte $06, $06, $06, $06, $06, $05, $04, $00
        .byte $05, $05, $05, $06, $06, $05, $04, $00
        .byte $04, $04, $04, $05, $06, $05, $04, $00
        .byte $00, $00, $04, $05, $05, $05, $04, $00
        .byte $00, $00, $04, $04, $04, $04, $04, $00
        .byte $00, $00, $01, $03, $03, $03, $01, $00
        .byte $00, $00, $01, $03, $03, $03, $02, $01
        .byte $00, $00, $01, $01, $01, $01, $01, $01

; Character 0: Rainbow horizontal gradient
_ncm_char_rainbow:
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE  ; Row 0
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE  ; Row 1
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE  ; Row 2
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE  ; Row 3
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE  ; Row 4
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE  ; Row 5
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE  ; Row 6
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE  ; Row 7

; Character 1: Colorful "M" for MEGA65
_ncm_char_M:
        ; M shape with gradient colors
        .byte $E0, $0E, $00, $00, $00, $00, $E0, $0E  ; Row 0: M   M
        .byte $EE, $EE, $00, $00, $00, $00, $EE, $EE  ; Row 1: MM MM
        .byte $AE, $EA, $00, $00, $00, $00, $AE, $EA  ; Row 2: M M M
        .byte $A0, $EA, $AE, $00, $00, $EA, $AE, $0A  ; Row 3: M  M  M
        .byte $70, $07, $77, $77, $77, $77, $70, $07  ; Row 4: M    M
        .byte $70, $07, $00, $00, $00, $00, $70, $07  ; Row 5: M    M
        .byte $50, $05, $00, $00, $00, $00, $50, $05  ; Row 6: M    M
        .byte $50, $05, $00, $00, $00, $00, $50, $05  ; Row 7: M    M

; Character 2: Checkerboard pattern
_ncm_char_checker:
        .byte $24, $42, $24, $42, $24, $42, $24, $42  ; Row 0
        .byte $42, $24, $42, $24, $42, $24, $42, $24  ; Row 1
        .byte $24, $42, $24, $42, $24, $42, $24, $42  ; Row 2
        .byte $42, $24, $42, $24, $42, $24, $42, $24  ; Row 3
        .byte $24, $42, $24, $42, $24, $42, $24, $42  ; Row 4
        .byte $42, $24, $42, $24, $42, $24, $42, $24  ; Row 5
        .byte $24, $42, $24, $42, $24, $42, $24, $42  ; Row 6
        .byte $42, $24, $42, $24, $42, $24, $42, $24  ; Row 7

; Character 3: Vertical color bars
_ncm_char_bars:
        .byte $11, $22, $33, $44, $55, $66, $77, $88  ; Row 0
        .byte $11, $22, $33, $44, $55, $66, $77, $88  ; Row 1
        .byte $11, $22, $33, $44, $55, $66, $77, $88  ; Row 2
        .byte $11, $22, $33, $44, $55, $66, $77, $88  ; Row 3
        .byte $11, $22, $33, $44, $55, $66, $77, $88  ; Row 4
        .byte $11, $22, $33, $44, $55, $66, $77, $88  ; Row 5
        .byte $11, $22, $33, $44, $55, $66, $77, $88  ; Row 6
        .byte $11, $22, $33, $44, $55, $66, $77, $88  ; Row 7

; Character 4: Gradient box (darker at edges)
_ncm_char_gradbox:
        .byte $00, $00, $00, $00, $00, $00, $00, $00  ; Row 0: border
        .byte $00, $11, $22, $33, $33, $22, $11, $00  ; Row 1
        .byte $00, $22, $44, $66, $66, $44, $22, $00  ; Row 2
        .byte $00, $33, $66, $FF, $FF, $66, $33, $00  ; Row 3
        .byte $00, $33, $66, $FF, $FF, $66, $33, $00  ; Row 4
        .byte $00, $22, $44, $66, $66, $44, $22, $00  ; Row 5
        .byte $00, $11, $22, $33, $33, $22, $11, $00  ; Row 6
        .byte $00, $00, $00, $00, $00, $00, $00, $00  ; Row 7: border

;---------------------------------------------------------------------------------------
; Palette (16 colors)
;---------------------------------------------------------------------------------------
_dn40_palette_r:
        .byte $00, $FF, $FF, $00, $FF, $00, $00, $FF  ; 0-7
        .byte $FF, $88, $FF, $44, $88, $88, $88, $CC  ; 8-15
_dn40_palette_g:
        .byte $00, $FF, $00, $FF, $00, $FF, $00, $FF  ; 0-7
        .byte $88, $44, $88, $44, $88, $FF, $88, $CC  ; 8-15
_dn40_palette_b:
        .byte $00, $FF, $00, $00, $FF, $00, $FF, $00  ; 0-7
        .byte $00, $00, $88, $44, $88, $88, $FF, $CC  ; 8-15