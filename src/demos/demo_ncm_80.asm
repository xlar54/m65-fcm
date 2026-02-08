;=======================================================================================
; Demo: NCM 80-column (640 pixel width)
;
; Same as 40-column but with H640 mode enabled.
; 40 NCM characters × 16 pixels = 640 pixels wide
;=======================================================================================

demo_ncm_80:
        ; Set NCM 80-column mode
        lda #MODE_NCM80
        jsr set_screen_mode

        ; Setup palette
        ldx #0
_dn80_pal_loop:
        lda _dn80_palette_r,x
        sta $D100,x
        lda _dn80_palette_g,x
        sta $D200,x
        lda _dn80_palette_b,x
        sta $D300,x
        inx
        cpx #16
        bne _dn80_pal_loop

        ; Set background
        lda #0
        sta $D021

        ; Create same NCM characters as 40-col demo
        lda #0
        ldx #<_ncm80_char_rainbow
        ldy #>_ncm80_char_rainbow
        jsr create_ncm_char

        lda #1
        ldx #<_ncm80_char_M
        ldy #>_ncm80_char_M
        jsr create_ncm_char

        lda #2
        ldx #<_ncm80_char_checker
        ldy #>_ncm80_char_checker
        jsr create_ncm_char

        lda #3
        ldx #<_ncm80_char_bars
        ldy #>_ncm80_char_bars
        jsr create_ncm_char

        lda #4
        ldx #<_ncm80_char_diamond
        ldy #>_ncm80_char_diamond
        jsr create_ncm_char

        ; === Display patterns ===

        ; Row 2: Rainbow across full 640 pixel width
        ldx #0
_place80_row2:
        lda #0                  ; Rainbow
        ldy #2
        jsr set_ncm_char
        inx
        cpx #40                 ; 40 NCM chars = 640 pixels
        bne _place80_row2

        ; Row 5: M characters
        ldx #0
_place80_row5:
        lda #1                  ; M
        ldy #5
        jsr set_ncm_char
        inx
        cpx #40
        bne _place80_row5

        ; Row 8: Alternating checker and bars
        ldx #0
_place80_row8:
        txa
        and #1
        beq _row8_checker
        lda #3                  ; Bars
        jmp _row8_place
_row8_checker:
        lda #2                  ; Checker
_row8_place:
        ldy #8
        jsr set_ncm_char
        inx
        cpx #40
        bne _place80_row8

        ; Row 11: Diamonds
        ldx #0
_place80_row11:
        lda #4                  ; Diamond
        ldy #11
        jsr set_ncm_char
        inx
        cpx #40
        bne _place80_row11

        ; Row 14: Mixed pattern
        ldx #0
_place80_row14:
        txa
        and #3                  ; Cycle 0-3
        ldy #14
        jsr set_ncm_char
        inx
        cpx #40
        bne _place80_row14

        ; Rows 17-19: Scene with alternating M and rainbow
        ldx #0
_place80_scene:
        txa
        and #1
        ldy #17
        beq _scene_rainbow
        lda #1                  ; M
        jmp _scene_place
_scene_rainbow:
        lda #0                  ; Rainbow
_scene_place:
        jsr set_ncm_char
        
        lda #2                  ; Checker for row 18
        ldy #18
        jsr set_ncm_char
        
        lda #3                  ; Bars for row 19
        ldy #19
        jsr set_ncm_char
        
        inx
        cpx #40
        bne _place80_scene

        jsr WAIT_SPACEBAR
        rts

;---------------------------------------------------------------------------------------
; NCM Character Data for 80-col demo
;---------------------------------------------------------------------------------------

_ncm80_char_rainbow:
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE
        .byte $10, $32, $54, $76, $98, $BA, $DC, $FE

_ncm80_char_M:
        .byte $E0, $0E, $00, $00, $00, $00, $E0, $0E
        .byte $EE, $EE, $00, $00, $00, $00, $EE, $EE
        .byte $AE, $EA, $00, $00, $00, $00, $AE, $EA
        .byte $A0, $EA, $AE, $00, $00, $EA, $AE, $0A
        .byte $70, $07, $77, $77, $77, $77, $70, $07
        .byte $70, $07, $00, $00, $00, $00, $70, $07
        .byte $50, $05, $00, $00, $00, $00, $50, $05
        .byte $50, $05, $00, $00, $00, $00, $50, $05

_ncm80_char_checker:
        .byte $59, $95, $59, $95, $59, $95, $59, $95
        .byte $95, $59, $95, $59, $95, $59, $95, $59
        .byte $59, $95, $59, $95, $59, $95, $59, $95
        .byte $95, $59, $95, $59, $95, $59, $95, $59
        .byte $59, $95, $59, $95, $59, $95, $59, $95
        .byte $95, $59, $95, $59, $95, $59, $95, $59
        .byte $59, $95, $59, $95, $59, $95, $59, $95
        .byte $95, $59, $95, $59, $95, $59, $95, $59

_ncm80_char_bars:
        .byte $22, $44, $66, $88, $AA, $CC, $EE, $FF
        .byte $22, $44, $66, $88, $AA, $CC, $EE, $FF
        .byte $22, $44, $66, $88, $AA, $CC, $EE, $FF
        .byte $22, $44, $66, $88, $AA, $CC, $EE, $FF
        .byte $22, $44, $66, $88, $AA, $CC, $EE, $FF
        .byte $22, $44, $66, $88, $AA, $CC, $EE, $FF
        .byte $22, $44, $66, $88, $AA, $CC, $EE, $FF
        .byte $22, $44, $66, $88, $AA, $CC, $EE, $FF

_ncm80_char_diamond:
        .byte $00, $00, $00, $77, $77, $00, $00, $00
        .byte $00, $00, $77, $AA, $AA, $77, $00, $00
        .byte $00, $77, $AA, $EE, $EE, $AA, $77, $00
        .byte $77, $AA, $EE, $FF, $FF, $EE, $AA, $77
        .byte $77, $AA, $EE, $FF, $FF, $EE, $AA, $77
        .byte $00, $77, $AA, $EE, $EE, $AA, $77, $00
        .byte $00, $00, $77, $AA, $AA, $77, $00, $00
        .byte $00, $00, $00, $77, $77, $00, $00, $00

;---------------------------------------------------------------------------------------
; Palette
;---------------------------------------------------------------------------------------
_dn80_palette_r:
        .byte $00, $FF, $FF, $00, $FF, $00, $00, $FF
        .byte $FF, $88, $FF, $44, $88, $88, $88, $CC
_dn80_palette_g:
        .byte $00, $FF, $00, $FF, $00, $FF, $00, $FF
        .byte $88, $44, $88, $44, $88, $FF, $88, $CC
_dn80_palette_b:
        .byte $00, $FF, $00, $00, $FF, $00, $FF, $00
        .byte $00, $00, $88, $44, $88, $88, $FF, $CC