;=======================================================================================
; fcm_init - Set up VIC-IV for FCM mode
;=======================================================================================
fcm_init:
        ; Only save original values on first call
        lda fcm_initialized
        bne _fcm_skip_save

        ; save original values
        lda VIC3_CTRL
        sta orig_d031
        lda VIC4_CTRL
        sta orig_d054
        lda VIC4_LINESTPLSB
        sta orig_linestep
        lda VIC4_TEXTXPOS
        sta orig_textxpos
        lda VIC4_TEXTYPOS
        sta orig_textypos

        lda #1
        sta fcm_initialized

_fcm_skip_save:
        ; Enable MEGA65 VIC-IV registers
        lda #$47
        sta VIC4_KEY
        lda #$53
        sta VIC4_KEY

        ; Enabling SEAM mode (Super Extended Attribute Mode)
        ; Set CHR16 (bit 0) + FCLRLO (bit 1) + FCLRHI (bit 2)
        ;       CHR16  = 16-bit screen codes (2 bytes per screen cell)
        ;       FCLRLO = FCM for screen codes < 256
        ;       FCLRHI = FCM for screen codes > 255
        
        lda #%00000101
        sta VIC4_CTRL

        lda screen_mode
        cmp #40
        bne _fcm_set80

_fcm_set40:
        ; 40-column mode: clear H640 (bit 7)
        lda VIC3_CTRL
        and #%01111111  ; clear H640 - 40 col
        ora #%00100000  ; set ATTR 
        sta VIC3_CTRL

        ; LINESTEP (40 chars × 2 bytes per screen code)
        lda #80         ; 80 for 40 col mode
        sta VIC4_LINESTPLSB
        lda #0
        sta VIC4_LINESTPMSB

        jmp _fcm_vic4_mode_set

_fcm_set80:
        ; 80-column mode: set bit 7
        lda VIC3_CTRL
        ora #%10100000
        sta VIC3_CTRL

        ; LINESTEP (80 chars × 2 bytes per screen code)
        lda #160         ; 160 for 80 col mode
        sta VIC4_LINESTPLSB
        lda #0
        sta VIC4_LINESTPMSB

_fcm_vic4_mode_set:
        ; -------- all above must be done before setting screen address

        ; Disable hot registers
        lda #$80
        trb $D05D

        ; point screen to new screen pointer (SCRNPTR)
        lda #<SCREEN_RAM
        sta VIC4_SCRNPTRLSB     ; Low byte
        lda #>SCREEN_RAM
        sta VIC4_SCRNPTRMSB     ; Mid byte  
        lda #`SCREEN_RAM
        sta VIC4_SCRBPTRBNK     ; Bank
        stz $D063               ; multi function register

        ; COLPTR = Offset from FF8.0000 for screen color ram
        lda #$00
        sta VIC4_COLPTRLSB
        sta VIC4_COLPTRMSB

        ; CHARPTR = $2D000 (ROM charset for PETSCII)
        lda #$00
        sta $D068               ; Low byte
        lda #$D0
        sta $D069               ; Mid byte  
        lda #$02
        sta $D06A               ; Bank

        jsr load_chars

        lda screen_mode
        cmp #40
        bne _fcm_done
        ; bump the X text position to the left by 3
        dec VIC4_TEXTXPOS
        dec VIC4_TEXTXPOS
        dec VIC4_TEXTXPOS

        ; increase the Y text position by 1
        inc VIC4_TEXTYPOS

_fcm_done:
        rts

orig_d031       .byte 0
orig_d054       .byte 0
orig_linestep   .byte 0
orig_textxpos   .byte $00
orig_textypos   .byte $00
fcm_initialized .byte 0 

;=======================================================================================
; exit_fcm - Restore normal screen mode and return to BASIC
;=======================================================================================
exit_fcm:
        ; Disable FCM/SEAM - clear CHR16, FCLRLO, FCLRHI
        lda orig_d054
        sta VIC4_CTRL

        ; Restore 80-column text mode (MEGA65 default)
        ; Set H640 (bit 7), clear ATTR (bit 5)
        lda orig_d031
        sta VIC3_CTRL

        ; Restore LINESTEP to 80 (80-col normal text mode = 1 byte per char)
        lda orig_linestep
        sta VIC4_LINESTPLSB

        ; Restore screen pointer to default $0800
        lda #$00
        sta VIC4_SCRNPTRLSB     ; $D060
        lda #$08
        sta VIC4_SCRNPTRMSB     ; $D061
        lda #$00
        sta VIC4_SCRBPTRBNK     ; $D062
        stz $D063

        ; Restore CHARPTR to default ROM charset ($2D000)
        lda #$00
        sta $D068
        lda #$D0
        sta $D069
        lda #$02
        sta $D06A

        ; Restore default colors
        lda #$06                ; blue
        sta BORDERCOL
        lda #$06                ; blue  
        sta BACKCOL

        ; Restore text position to 0
        lda orig_textxpos
        sta VIC4_TEXTXPOS
        lda orig_textypos
        sta VIC4_TEXTYPOS

        ; Reset init flag so next fcm_init saves fresh values
        lda #0
        sta fcm_initialized

        ; Reset screen editor and clear screen
        lda #$93
        jsr $FFD2
        jsr $FF81               ; CINT - init screen
        rts
