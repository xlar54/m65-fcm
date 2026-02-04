;=======================================================================================
; set_screen_mode - Initialize screen mode
; Input: A = mode (0-4, other values treated as 0)
; Destroys: A, X, Y, Z, PTR
;=======================================================================================
set_screen_mode:
        ; Validate mode
        cmp #5
        bcc _ssm_valid
        lda #0                  ; Invalid mode -> BASIC
_ssm_valid:
        sta _ssm_mode
        
        ; Mode 0 = exit to BASIC
        cmp #0
        bne _ssm_fcm_init
        jmp _ssm_exit_basic

_ssm_fcm_init:
        ; Enable MEGA65 VIC-IV registers
        lda #$47
        sta VIC4_KEY
        lda #$53
        sta VIC4_KEY

        ; Disable hot registers
        lda #$80
        trb $D05D

        ; Enable SEAM mode
        lda #%00000101
        sta VIC4_CTRL

        ; turn off screen while clearing RAM
        jsr _ssm_screen_off

        ; Branch based on mode
        lda _ssm_mode
        cmp #MODE_TEXT40
        beq _ssm_text40
        cmp #MODE_TEXT80
        beq _ssm_text80
        cmp #MODE_BITMAP40
        beq _ssm_bitmap40
        cmp #MODE_BITMAP80
        beq _ssm_bitmap80
        jmp _ssm_exit_basic     ; Fallback

;---------------------------------------------------------------------------------------
; Text 40-column mode
;---------------------------------------------------------------------------------------
_ssm_text40:
        lda #40
        sta screen_mode

        lda VIC3_CTRL
        and #%01011111          ; Clear H640 AND ATTR
        sta VIC3_CTRL

        lda #80
        sta VIC4_LINESTPLSB
        lda #0
        sta VIC4_LINESTPMSB

        lda #40
        sta VIC4_CHRCOUNT               ; CHRCOUNT

        lda #25
        sta VIC4_DISPROWS               ; CHRCOUNT_V - number of rows 

        lda #$50
        sta VIC4_TEXTXPOS
        lda #0
        sta VIC4_TEXTXPOS+1
        lda #$69
        sta VIC4_TEXTYPOS

        jmp _ssm_finish_text

;---------------------------------------------------------------------------------------
; Text 80-column mode
;---------------------------------------------------------------------------------------
_ssm_text80:
        lda #80
        sta screen_mode

        lda VIC3_CTRL
        and #%11011111          ; Clear ATTR
        ora #%10000000          ; Set H640
        sta VIC3_CTRL

        lda #160
        sta VIC4_LINESTPLSB
        lda #0
        sta VIC4_LINESTPMSB

        lda #80
        sta VIC4_CHRCOUNT               ; CHRCOUNT

        lda #25
        sta VIC4_DISPROWS               ; CHRCOUNT_V - number of rows 

        lda #$50
        sta VIC4_TEXTXPOS
        lda #0
        sta VIC4_TEXTXPOS+1
        lda #$69
        sta VIC4_TEXTYPOS

        jmp _ssm_finish_text

;---------------------------------------------------------------------------------------
; Bitmap 40-column mode (320×200)
;---------------------------------------------------------------------------------------
_ssm_bitmap40:
        lda #40
        sta screen_mode

        lda VIC3_CTRL
        and #%01111111          ; Clear H640
        ora #%00100000          ; Set ATTR
        sta VIC3_CTRL

        lda #80
        sta VIC4_LINESTPLSB
        lda #0
        sta VIC4_LINESTPMSB

        lda #40
        sta VIC4_CHRCOUNT               ; CHRCOUNT

        lda #25
        sta VIC4_DISPROWS               ; CHRCOUNT_V - number of rows 

        lda #$50
        sta VIC4_TEXTXPOS
        lda #0
        sta VIC4_TEXTXPOS+1
        lda #$69
        sta VIC4_TEXTYPOS

        jmp _ssm_finish_bitmap

;---------------------------------------------------------------------------------------
; Bitmap 80-column mode (640×200)
;---------------------------------------------------------------------------------------
_ssm_bitmap80:

        lda #80
        sta screen_mode

        lda VIC3_CTRL
        and #%01011111          ; Clear H640 and ATTR first
        ora #%10100000          ; Then set H640 + ATTR
        sta VIC3_CTRL

        lda #160
        sta VIC4_LINESTPLSB
        lda #0
        sta VIC4_LINESTPMSB

        lda #80
        sta VIC4_CHRCOUNT               ; CHRCOUNT

        lda #25
        sta VIC4_DISPROWS               ; CHRCOUNT_V - number of rows 

        lda #$50
        sta VIC4_TEXTXPOS
        lda #0
        sta VIC4_TEXTXPOS+1
        lda #$69
        sta VIC4_TEXTYPOS

        jmp _ssm_finish_bitmap

;---------------------------------------------------------------------------------------
; Common setup for text modes
;---------------------------------------------------------------------------------------
_ssm_finish_text:
        jsr _ssm_setup_pointers

        ; Clear color RAM with NCM=0 for traditional chars
        lda #$05                ; Green foreground
        jsr clear_color_ram_text
        
        ; Load custom characters for text mode
        jsr load_chars
        
        jmp _ssm_screen_on

;---------------------------------------------------------------------------------------
; Common setup for bitmap modes
;---------------------------------------------------------------------------------------
_ssm_finish_bitmap:
        jsr _ssm_setup_pointers

        ; Then set the correct $00/$01 pattern
        jsr clear_color_ram

        jsr init_bitmap
        
        lda #$00
        jsr clear_bitmap
        
        jmp _ssm_screen_on

;---------------------------------------------------------------------------------------
; Setup screen/color/char pointers (common to all FCM modes)
;---------------------------------------------------------------------------------------
_ssm_setup_pointers:
        ; Screen RAM pointer
        lda #<SCREEN_RAM
        sta VIC4_SCRNPTRLSB
        lda #>SCREEN_RAM
        sta VIC4_SCRNPTRMSB
        lda #`SCREEN_RAM
        sta VIC4_SCRBPTRBNK
        stz $D063

        ; Color RAM pointer
        lda #$00
        sta VIC4_COLPTRLSB
        sta VIC4_COLPTRMSB

        ; CHARPTR = $2D800 (ROM charset for PETSCII)
        lda #$00
        sta $D068
        lda #$d8
        sta $D069
        lda #$02
        sta $D06A
        
        rts

;---------------------------------------------------------------------------------------
; Enable screen display
;---------------------------------------------------------------------------------------
_ssm_screen_on:
        lda $D011
        ora #%00010000          ; Set DEN bit
        sta $D011
        rts

;---------------------------------------------------------------------------------------
; Disable screen display
;---------------------------------------------------------------------------------------
_ssm_screen_off:
        lda $D011
        and #%11101111          ; Clear DEN bit
        sta $D011
        rts

;---------------------------------------------------------------------------------------
; Exit to BASIC mode
;---------------------------------------------------------------------------------------
_ssm_exit_basic:
        ; Keep VIC-IV enabled
        lda #$47
        sta VIC4_KEY
        lda #$53
        sta VIC4_KEY

        ; Disable FCM/SEAM
        lda #$00
        sta VIC4_CTRL

        ; Restore VIC3_CTRL - clear ATTR, set H640
        lda VIC3_CTRL
        ora #%10000000          ; Set H640
        and #%11011111          ; Clear ATTR
        sta VIC3_CTRL

        ; Restore LINESTEP
        lda #80
        sta VIC4_LINESTPLSB
        lda #0
        sta VIC4_LINESTPMSB

        ; Restore CHRCOUNT
        lda #80
        sta VIC4_CHRCOUNT

        ; Restore screen pointer to $0800
        lda #$00
        sta VIC4_SCRNPTRLSB
        lda #$08
        sta VIC4_SCRNPTRMSB
        lda #$00
        sta VIC4_SCRBPTRBNK
        stz $D063

        ; Restore text position
        lda #$50
        sta VIC4_TEXTXPOS
        lda #0
        sta VIC4_TEXTXPOS+1
        lda #$68
        sta VIC4_TEXTYPOS

        ; Restore default colors
        lda #$06
        sta BORDERCOL
        sta BACKCOL

; Restore color RAM to default foreground
;        lda #$00
;        sta PTR
;        lda #$00
;        sta PTR+1
;        lda #$F8
;        sta PTR+2
;        lda #$0F
;        sta PTR+3
;        
;        ldx #22                 ; 5632 bytes = 22 × 256
;_eb_clr:
;        ldz #0
;-       lda #$05                ; default MEGA65 text color
;        sta [PTR],z
;        inz
;        bne -
;        inc PTR+1
;        dex
;        bne _eb_clr

; DMA: Reset color RAM to green ($05) for BASIC
        ; Reset both MBs explicitly for clean KERNAL handoff
        lda #$00
        sta $D707
        .byte $80, $00          ; source MB = $00
        .byte $81, $FF          ; dest MB = $FF (color RAM)
        .byte $00               ; end options
        .byte $03               ; fill
        .word 5632              ; count
        .byte $05, $00          ; fill value = $05 (green)
        .byte $00               ; src bank
        .word $0000             ; dest = $FF80000
        .byte $08               ; dest bank ($F8 in MB $FF)
        .byte $00               ; cmd high
        .word $0000             ; modulo

        jsr $FF81
        rts

_ssm_mode: .byte 0

;===========================================================================================
; sets the palette of colors
; $00 is always transparent
; $FF is special and is foreground color
;
;       $D100-$D1FF : red
;       $D200-$D2FF : green
;       $D300-$D3FF : blue
;
; load a color from $00-$FF into A
; then STA to the RGB color registers, 
; EXCEPT +$00 (always transparent) or +$FF (always color RAM foreground)
;===========================================================================================
init_palette:
        ; establish the color BLACK
        ; $AA is picked abritrarily to stand out in the character definition below
        lda #$00
        sta $D100+$AA           ; $AA in char data for RED is set to $00
        sta $D200+$AA           ; $AA in char data for RED is set to $00
        sta $D300+$AA           ; $AA in char data for RED is set to $00

        rts

;=======================================================================================
; set_palette_color - Set a single palette entry to an RGB color
; Input: A = palette index (0-255)
;        X = red (0-15)
;        Y = green (0-15)
;        Z = blue (0-15)
; Note: Index $00 is always transparent, $FF is always color RAM foreground
; Destroys: A
;=======================================================================================
set_palette_color:
        sta _spc_idx
        stx _spc_r
        sty _spc_g
        tza
        sta _spc_b
        ldx _spc_idx
        lda _spc_r
        sta $D100,x
        lda _spc_g
        sta $D200,x
        lda _spc_b
        sta $D300,x
        rts

_spc_idx: .byte 0
_spc_r:   .byte 0
_spc_g:   .byte 0
_spc_b:   .byte 0