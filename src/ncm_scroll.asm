;=======================================================================================
; NCM Smooth Scrolling Library
;
; Provides smooth pixel-by-pixel scrolling in all 4 directions for NCM mode.
; Uses hardware fine scroll (0-15 pixels) combined with coarse scroll (shift screen RAM)
;
; Before calling ncm_scroll_init, set:
;   ncm_scroll_row_start = first row to scroll (0-24)
;   ncm_scroll_row_end   = row after last (1-25), e.g. 25 means through row 24
;   ncm_scroll_col_start = first column (0-39) - NOT YET IMPLEMENTED
;   ncm_scroll_col_end   = column after last (1-40) - NOT YET IMPLEMENTED
;   ncm_scroll_speed     = pixels per scroll call (default 1)
;
; NCM characters are 16 pixels wide x 8 pixels tall
;=======================================================================================

;---------------------------------------------------------------------------------------
; Scroll region configuration - SET THESE BEFORE CALLING ncm_scroll_init
;---------------------------------------------------------------------------------------
ncm_scroll_row_start:   .byte 0      ; First row to scroll (0-24)
ncm_scroll_row_end:     .byte 25     ; Row after last (exclusive), 25 = all rows
ncm_scroll_col_start:   .byte 0      ; First column (reserved for future)
ncm_scroll_col_end:     .byte 40     ; Column after last (reserved for future)
ncm_scroll_speed:       .byte 1      ; Pixels per scroll call

;---------------------------------------------------------------------------------------
; Scroll state variables
;---------------------------------------------------------------------------------------
scroll_x:       .byte 0         ; Fine X scroll position (0-15)
scroll_y:       .byte 0         ; Fine Y scroll position (0-7)
coarse_x:       .word 0         ; Coarse X position (for map tracking)
coarse_y:       .word 0         ; Coarse Y position (for map tracking)

; original border size
orig_sdbdrwd: .byte 0

; Base text position (default values)
base_textxpos:  .word $0050     ; Default TEXTXPOS
base_textypos:  .byte $69       ; Default TEXTYPOS

;=======================================================================================
; ncm_scroll_init - Initialize scrolling system
; Set ncm_scroll_row_start/end before calling
;=======================================================================================
ncm_scroll_init:
        lda #0
        sta scroll_x
        sta scroll_y
        sta coarse_x
        sta coarse_x+1
        sta coarse_y
        sta coarse_y+1
        
        ; Store default positions
        lda #$50
        sta base_textxpos
        lda #0
        sta base_textxpos+1
        lda #$69
        sta base_textypos

        ; Save original border width
        lda VIC_SDBDRWD
        sta orig_sdbdrwd

        ; Increase side border width
        ; This makes both left AND right borders wider
        clc
        adc #16                         ; Add 16 pixels to border width
        sta VIC_SDBDRWD

        rts

;=======================================================================================
; ncm_scroll_end - Restore original border size
;=======================================================================================
ncm_scroll_end:
        lda orig_sdbdrwd
        sta VIC_SDBDRWD
        rts

;=======================================================================================
; ncm_scroll_left - Scroll screen left by speed pixels
; Returns: A = 1 if coarse scroll happened, 0 if only fine scroll
;=======================================================================================
ncm_scroll_left:
        clc
        lda scroll_x
        adc ncm_scroll_speed        ; Add speed
        sta scroll_x
        
        cmp #16                 ; NCM chars are 16 pixels wide
        bcc _sl_fine_only
        
        ; Reset fine scroll and do coarse scroll
        lda #0
        sta scroll_x
        jsr ncm_scroll_coarse_left

        ; Increment coarse position
        inc coarse_x
        bne +
        inc coarse_x+1
+
        lda #1                  ; Signal coarse scroll happened
        jmp _sl_update
        
_sl_fine_only:
        lda #0
        
_sl_update:
        pha
        jsr update_scroll_hw
        pla
        rts

;=======================================================================================
; ncm_scroll_right - Scroll screen right by speed pixels
; Returns: A = 1 if coarse scroll happened, 0 if only fine scroll
;=======================================================================================
ncm_scroll_right:
        sec
        lda scroll_x
        sbc ncm_scroll_speed        ; Subtract speed
        sta scroll_x

        bpl _sr_fine_only
        
        ; Wrapped - reset to 15 and do coarse scroll
        lda #15
        sta scroll_x
        jsr ncm_scroll_coarse_right
        
        ; Decrement coarse position
        lda coarse_x
        bne +
        dec coarse_x+1
+       dec coarse_x
        
        lda #1
        jmp _sr_update
        
_sr_fine_only:
        lda #0
        
_sr_update:
        pha
        jsr update_scroll_hw
        pla
        rts

;=======================================================================================
; ncm_scroll_up - Scroll screen up by speed pixels
; Returns: A = 1 if coarse scroll happened, 0 if only fine scroll
;=======================================================================================
ncm_scroll_up:
        clc
        lda scroll_y
        adc ncm_scroll_speed        ; Add speed
        sta scroll_y
        
        cmp #8                  ; Characters are 8 pixels tall
        bcc _su_fine_only
        
        ; Reset fine scroll and do coarse scroll
        lda #0
        sta scroll_y
        jsr ncm_scroll_coarse_up
        
        ; Increment coarse Y position
        inc coarse_y
        bne +
        inc coarse_y+1
+
        lda #1
        jmp _su_update
        
_su_fine_only:
        lda #0
        
_su_update:
        pha
        jsr update_scroll_hw
        pla
        rts

;=======================================================================================
; ncm_scroll_down - Scroll screen down by speed pixels
; Returns: A = 1 if coarse scroll happened, 0 if only fine scroll
;=======================================================================================
ncm_scroll_down:
        sec
        lda scroll_y
        sbc ncm_scroll_speed        ; Subtract speed
        sta scroll_y
        
        bpl _sd_fine_only
        
        ; Wrapped - reset to 7 and do coarse scroll
        lda #7
        sta scroll_y
        jsr ncm_scroll_coarse_down
        
        ; Decrement coarse Y position
        lda coarse_y
        bne +
        dec coarse_y+1
+       dec coarse_y
        
        lda #1
        jmp _sd_update
        
_sd_fine_only:
        lda #0
        
_sd_update:
        pha
        jsr update_scroll_hw
        pla
        rts

;=======================================================================================
; update_scroll_hw - Update hardware scroll registers
; Uses TEXTXPOS/TEXTYPOS for fine scrolling
;=======================================================================================
update_scroll_hw:
        ; Update X scroll (TEXTXPOS)
        ; Subtract scroll_x from base position to scroll left
        sec
        lda base_textxpos
        sbc scroll_x
        sta VIC4_TEXTXPOS
        lda base_textxpos+1
        sbc #0
        sta VIC4_TEXTXPOS+1
        
        ; Update Y scroll (TEXTYPOS)
        ; Subtract scroll_y from base position to scroll up
        sec
        lda base_textypos
        sbc scroll_y
        sta VIC4_TEXTYPOS
        
        rts

;=======================================================================================
; ncm_scroll_coarse_left - Shift screen columns left within scroll region
;=======================================================================================
ncm_scroll_coarse_left:
        lda ncm_scroll_row_start
        sta _csl_row
        
_csl_row_loop:
        ; Calculate source and dest for this row
        ; Source = SCREEN_RAM + (row * 80) + 2
        ; Dest = SCREEN_RAM + (row * 80)
        lda _csl_row
        sta MULTINA
        lda #0
        sta MULTINA+1
        sta MULTINA+2
        sta MULTINA+3
        
        lda #80                 ; 40 positions * 2 bytes
        sta MULTINB
        lda #0
        sta MULTINB+1
        sta MULTINB+2
        sta MULTINB+3
        
        ; Dest = SCREEN_RAM + result
        clc
        lda MULTOUT
        adc #<SCREEN_RAM
        sta _csl_dest
        lda MULTOUT+1
        adc #>SCREEN_RAM
        sta _csl_dest+1
        
        ; Source = Dest + 2
        clc
        lda _csl_dest
        adc #2
        sta _csl_src
        lda _csl_dest+1
        adc #0
        sta _csl_src+1
        
        ; DMA copy 78 bytes (39 positions * 2 bytes)
        lda #0
        sta $D707               ; DMA trigger register
        .byte $80, $00          ; Source MB = 0
        .byte $81, $00          ; Dest MB = 0
        .byte $00               ; End of options
        .byte $00               ; Copy command
        .word 78                ; Count
_csl_src:
        .word $0000             ; Source address (modified)
        .byte `SCREEN_RAM       ; Source bank
_csl_dest:
        .word $0000             ; Dest address (modified)
        .byte `SCREEN_RAM       ; Dest bank
        .byte $00               ; Command MSB
        .word $0000             ; Modulo (not used)
        
        inc _csl_row
        lda _csl_row
        cmp ncm_scroll_row_end
        bne _csl_row_loop
        
        ; Also shift color RAM the same way
        jsr ncm_scroll_coarse_left_color
        
        rts

_csl_row: .byte 0

;---------------------------------------------------------------------------------------
; ncm_scroll_coarse_left_color - Shift color RAM columns left
;---------------------------------------------------------------------------------------
ncm_scroll_coarse_left_color:
        lda ncm_scroll_row_start
        sta _cslc_row
        
_cslc_row_loop:
        lda _cslc_row
        sta MULTINA
        lda #0
        sta MULTINA+1
        sta MULTINA+2
        sta MULTINA+3
        
        lda #80
        sta MULTINB
        lda #0
        sta MULTINB+1
        sta MULTINB+2
        sta MULTINB+3
        
        ; Dest = $FF80000 + result
        clc
        lda MULTOUT
        sta _cslc_dest
        adc #2
        sta _cslc_src
        lda MULTOUT+1
        sta _cslc_dest+1
        adc #0
        sta _cslc_src+1
        
        ; DMA copy color RAM
        lda #0
        sta $D707
        .byte $80, $FF          ; Source MB = $FF
        .byte $81, $FF          ; Dest MB = $FF
        .byte $00
        .byte $00               ; Copy
        .word 78
_cslc_src:
        .word $0000
        .byte $08               ; Bank 8 for color RAM
_cslc_dest:
        .word $0000
        .byte $08
        .byte $00
        .word $0000
        
        inc _cslc_row
        lda _cslc_row
        cmp ncm_scroll_row_end
        bne _cslc_row_loop
        rts

_cslc_row: .byte 0

;=======================================================================================
; ncm_scroll_coarse_right - Shift screen columns right within scroll region
;=======================================================================================
ncm_scroll_coarse_right:
        lda ncm_scroll_row_start
        sta _csr_row
        
_csr_row_loop:
        lda _csr_row
        sta MULTINA
        lda #0
        sta MULTINA+1
        sta MULTINA+2
        sta MULTINA+3
        
        lda #80
        sta MULTINB
        lda #0
        sta MULTINB+1
        sta MULTINB+2
        sta MULTINB+3
        
        ; Setup pointer for this row
        clc
        lda MULTOUT
        adc #<SCREEN_RAM
        sta PTR
        lda MULTOUT+1
        adc #>SCREEN_RAM
        sta PTR+1
        lda #`SCREEN_RAM
        sta PTR+2
        lda #0
        sta PTR+3
        
        ldx #38                 ; 39 positions to copy (index 38 down to 0)
_csr_copy_loop:
        ; Calculate offset: x * 2
        txa
        asl
        taz
        
        ; Read 2 bytes from position X
        lda [PTR],z
        sta _csr_temp
        inz
        lda [PTR],z
        sta _csr_temp+1
        
        ; Write to position X+1
        inz
        lda _csr_temp
        sta [PTR],z
        inz
        lda _csr_temp+1
        sta [PTR],z
        
        dex
        bpl _csr_copy_loop
        
        inc _csr_row
        lda _csr_row
        cmp ncm_scroll_row_end
        bne _csr_row_loop
        
        jsr ncm_scroll_coarse_right_color
        rts

_csr_row:      .byte 0
_csr_temp:     .word 0

;---------------------------------------------------------------------------------------
; ncm_scroll_coarse_right_color - Shift color RAM columns right
;---------------------------------------------------------------------------------------
ncm_scroll_coarse_right_color:
        lda ncm_scroll_row_start
        sta _csrc_row
        
_csrc_row_loop:
        lda _csrc_row
        sta MULTINA
        lda #0
        sta MULTINA+1
        sta MULTINA+2
        sta MULTINA+3
        
        lda #80
        sta MULTINB
        lda #0
        sta MULTINB+1
        sta MULTINB+2
        sta MULTINB+3
        
        lda MULTOUT
        sta PTR
        lda MULTOUT+1
        sta PTR+1
        lda #$FF
        sta PTR+2
        lda #$08
        sta PTR+3
        
        ldx #38
_csrc_copy_loop:
        txa
        asl
        taz
        
        lda [PTR],z
        sta _csrc_temp
        inz
        lda [PTR],z
        sta _csrc_temp+1
        
        inz
        lda _csrc_temp
        sta [PTR],z
        inz
        lda _csrc_temp+1
        sta [PTR],z
        
        dex
        bpl _csrc_copy_loop
        
        inc _csrc_row
        lda _csrc_row
        cmp ncm_scroll_row_end
        bne _csrc_row_loop
        rts

_csrc_row:   .byte 0
_csrc_temp:  .word 0

;=======================================================================================
; ncm_scroll_coarse_up - Shift screen rows up within scroll region
;=======================================================================================
ncm_scroll_coarse_up:
        lda ncm_scroll_row_start
        sta _csu_row
        
_csu_row_loop:
        ; Check if next row is past end
        lda _csu_row
        clc
        adc #1
        cmp ncm_scroll_row_end
        bcs _csu_done
        
        ; Source = row + 1
        sta MULTINA
        lda #0
        sta MULTINA+1
        sta MULTINA+2
        sta MULTINA+3
        
        lda #80
        sta MULTINB
        lda #0
        sta MULTINB+1
        sta MULTINB+2
        sta MULTINB+3
        
        ; Source = SCREEN_RAM + (row+1) * 80
        clc
        lda MULTOUT
        adc #<SCREEN_RAM
        sta _csu_src
        lda MULTOUT+1
        adc #>SCREEN_RAM
        sta _csu_src+1
        
        ; Dest = SCREEN_RAM + row * 80
        lda _csu_row
        sta MULTINA
        
        clc
        lda MULTOUT
        adc #<SCREEN_RAM
        sta _csu_dest
        lda MULTOUT+1
        adc #>SCREEN_RAM
        sta _csu_dest+1
        
        ; DMA copy 80 bytes
        lda #0
        sta $D707
        .byte $80, $00
        .byte $81, $00
        .byte $00
        .byte $00               ; Copy
        .word 80
_csu_src:
        .word $0000
        .byte `SCREEN_RAM
_csu_dest:
        .word $0000
        .byte `SCREEN_RAM
        .byte $00
        .word $0000
        
        inc _csu_row
        jmp _csu_row_loop
        
_csu_done:
        jsr ncm_scroll_coarse_up_color
        rts

_csu_row: .byte 0

;---------------------------------------------------------------------------------------
; ncm_scroll_coarse_up_color - Shift color RAM rows up
;---------------------------------------------------------------------------------------
ncm_scroll_coarse_up_color:
        lda ncm_scroll_row_start
        sta _csuc_row
        
_csuc_row_loop:
        lda _csuc_row
        clc
        adc #1
        cmp ncm_scroll_row_end
        bcs _csuc_done
        
        ; Source = row + 1
        sta MULTINA
        lda #0
        sta MULTINA+1
        sta MULTINA+2
        sta MULTINA+3
        
        lda #80
        sta MULTINB
        lda #0
        sta MULTINB+1
        sta MULTINB+2
        sta MULTINB+3
        
        lda MULTOUT
        sta _csuc_src
        lda MULTOUT+1
        sta _csuc_src+1
        
        ; Dest = row
        lda _csuc_row
        sta MULTINA
        
        lda MULTOUT
        sta _csuc_dest
        lda MULTOUT+1
        sta _csuc_dest+1
        
        lda #0
        sta $D707
        .byte $80, $FF
        .byte $81, $FF
        .byte $00
        .byte $00
        .word 80
_csuc_src:
        .word $0000
        .byte $08
_csuc_dest:
        .word $0000
        .byte $08
        .byte $00
        .word $0000
        
        inc _csuc_row
        jmp _csuc_row_loop
        
_csuc_done:
        rts

_csuc_row: .byte 0

;=======================================================================================
; ncm_scroll_coarse_down - Shift screen rows down within scroll region
;=======================================================================================
ncm_scroll_coarse_down:
        ; Start from bottom row and work up
        lda ncm_scroll_row_end
        sec
        sbc #2
        sta _csd_row
        
_csd_row_loop:
        lda _csd_row
        cmp ncm_scroll_row_start
        bcc _csd_done
        
        ; Source = row
        lda _csd_row
        sta MULTINA
        lda #0
        sta MULTINA+1
        sta MULTINA+2
        sta MULTINA+3
        
        lda #80
        sta MULTINB
        lda #0
        sta MULTINB+1
        sta MULTINB+2
        sta MULTINB+3
        
        clc
        lda MULTOUT
        adc #<SCREEN_RAM
        sta _csd_src
        lda MULTOUT+1
        adc #>SCREEN_RAM
        sta _csd_src+1
        
        ; Dest = row + 1
        lda _csd_row
        clc
        adc #1
        sta MULTINA
        
        clc
        lda MULTOUT
        adc #<SCREEN_RAM
        sta _csd_dest
        lda MULTOUT+1
        adc #>SCREEN_RAM
        sta _csd_dest+1
        
        ; DMA copy 80 bytes
        lda #0
        sta $D707
        .byte $80, $00
        .byte $81, $00
        .byte $00
        .byte $00
        .word 80
_csd_src:
        .word $0000
        .byte `SCREEN_RAM
_csd_dest:
        .word $0000
        .byte `SCREEN_RAM
        .byte $00
        .word $0000
        
        dec _csd_row
        jmp _csd_row_loop
        
_csd_done:
        jsr ncm_scroll_coarse_down_color
        rts

_csd_row: .byte 0

;---------------------------------------------------------------------------------------
; ncm_scroll_coarse_down_color - Shift color RAM rows down
;---------------------------------------------------------------------------------------
ncm_scroll_coarse_down_color:
        lda ncm_scroll_row_end
        sec
        sbc #2
        sta _csdc_row
        
_csdc_row_loop:
        lda _csdc_row
        cmp ncm_scroll_row_start
        bcc _csdc_done
        
        ; Source = row
        lda _csdc_row
        sta MULTINA
        lda #0
        sta MULTINA+1
        sta MULTINA+2
        sta MULTINA+3
        
        lda #80
        sta MULTINB
        lda #0
        sta MULTINB+1
        sta MULTINB+2
        sta MULTINB+3
        
        lda MULTOUT
        sta _csdc_src
        lda MULTOUT+1
        sta _csdc_src+1
        
        ; Dest = row + 1
        lda _csdc_row
        clc
        adc #1
        sta MULTINA
        
        lda MULTOUT
        sta _csdc_dest
        lda MULTOUT+1
        sta _csdc_dest+1
        
        lda #0
        sta $D707
        .byte $80, $FF
        .byte $81, $FF
        .byte $00
        .byte $00
        .word 80
_csdc_src:
        .word $0000
        .byte $08
_csdc_dest:
        .word $0000
        .byte $08
        .byte $00
        .word $0000
        
        dec _csdc_row
        jmp _csdc_row_loop
        
_csdc_done:
        rts

_csdc_row: .byte 0

;=======================================================================================
; ncm_scroll_fill_column_right - Fill rightmost column with a tile (after scroll left)
; Input: A = tile/character index to fill with
;=======================================================================================
ncm_scroll_fill_column_right:
        sta _fcr_tile
        
        lda #<(SCREEN_RAM + 78) ; Position 39 (rightmost) = offset 78
        sta PTR
        lda #>(SCREEN_RAM + 78)
        sta PTR+1
        lda #`SCREEN_RAM
        sta PTR+2
        lda #0
        sta PTR+3
        
        ldy #0                  ; Row counter
_fcr_loop:
        ; Write tile code
        ldz #0
        clc
        lda #<CHAR_CODE_BASE
        adc _fcr_tile
        sta [PTR],z
        inz
        lda #>CHAR_CODE_BASE
        adc #0
        sta [PTR],z
        
        ; Advance to next row (add 80 bytes)
        clc
        lda PTR
        adc #80
        sta PTR
        lda PTR+1
        adc #0
        sta PTR+1
        
        iny
        cpy #25
        bne _fcr_loop
        rts

_fcr_tile: .byte 0

;=======================================================================================
; ncm_scroll_fill_column_left - Fill leftmost column with a tile (after scroll right)
; Input: A = tile/character index to fill with
;=======================================================================================
ncm_scroll_fill_column_left:
        sta _fcl_tile
        
        lda #<SCREEN_RAM
        sta PTR
        lda #>SCREEN_RAM
        sta PTR+1
        lda #`SCREEN_RAM
        sta PTR+2
        lda #0
        sta PTR+3
        
        ldy #0
_fcl_loop:
        ldz #0
        clc
        lda #<CHAR_CODE_BASE
        adc _fcl_tile
        sta [PTR],z
        inz
        lda #>CHAR_CODE_BASE
        adc #0
        sta [PTR],z
        
        clc
        lda PTR
        adc #80
        sta PTR
        lda PTR+1
        adc #0
        sta PTR+1
        
        iny
        cpy #25
        bne _fcl_loop
        rts

_fcl_tile: .byte 0

;=======================================================================================
; ncm_scroll_fill_row_bottom - Fill bottom row with a tile (after scroll up)
; Input: A = tile/character index to fill with
;=======================================================================================
ncm_scroll_fill_row_bottom:
        sta _frb_tile
        
        ; Row 24 = offset 24 * 80 = 1920
        lda #<(SCREEN_RAM + 1920)
        sta PTR
        lda #>(SCREEN_RAM + 1920)
        sta PTR+1
        lda #`SCREEN_RAM
        sta PTR+2
        lda #0
        sta PTR+3
        
        ldx #0                  ; Column counter
_frb_loop:
        ldz #0
        clc
        lda #<CHAR_CODE_BASE
        adc _frb_tile
        sta [PTR],z
        inz
        lda #>CHAR_CODE_BASE
        adc #0
        sta [PTR],z
        
        clc
        lda PTR
        adc #2
        sta PTR
        bcc +
        inc PTR+1
+
        inx
        cpx #40                 ; 40 positions for NCM40
        bne _frb_loop
        rts

_frb_tile: .byte 0

;=======================================================================================
; ncm_scroll_fill_row_top - Fill top row with a tile (after scroll down)
; Input: A = tile/character index to fill with
;=======================================================================================
ncm_scroll_fill_row_top:
        sta _frt_tile
        
        lda #<SCREEN_RAM
        sta PTR
        lda #>SCREEN_RAM
        sta PTR+1
        lda #`SCREEN_RAM
        sta PTR+2
        lda #0
        sta PTR+3
        
        ldx #0
_frt_loop:
        ldz #0
        clc
        lda #<CHAR_CODE_BASE
        adc _frt_tile
        sta [PTR],z
        inz
        lda #>CHAR_CODE_BASE
        adc #0
        sta [PTR],z
        
        clc
        lda PTR
        adc #2
        sta PTR
        bcc +
        inc PTR+1
+
        inx
        cpx #40
        bne _frt_loop
        rts

_frt_tile: .byte 0