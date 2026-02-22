;=======================================================================================
; Demo: NCM 40-column (320 pixel width)
;
; Demonstrates NCM characters - each character can have 16 colors!
; NCM chars are 16 pixels wide × 8 tall.
;
; Controls:
;   Left cursor  - Scroll left (tiles move left, new tiles appear on right)
;   Right cursor - Scroll right (tiles move right, new tiles appear on left)
;   Spacebar     - Exit
;=======================================================================================



demo_ncm_40_scroll:
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
        lda #1                  ; column 1
        sta str_col
        lda #$0F                ; white
        sta str_color
        jsr draw_petscii_string

        lda #<_title2
        sta str_ptr
        lda #>_title2
        sta str_ptr+1
        lda #1                  ; row 1
        sta str_row
        lda #1                  ; column 1
        sta str_col
        lda #$0F                ; white
        sta str_color
        jsr draw_petscii_string

        ; Set background color
        lda #0
        sta $D021
        
        #SET_NCM_COLOR $01, 0, 0, 0       ; Palette index 1 = dark blue
        #SET_NCM_COLOR $02, 11, 2, 2      ; Palette index 2 = brick red
        #SET_NCM_COLOR $03, 15, 8, 12     ; Palette index 3 = pink mortar
       

        ; Create NCM character 1:
        lda #0
        ldx #<_tile1
        ldy #>_tile1
        jsr create_ncm_char

        ; Create NCM character 2:
        lda #1
        ldx #<_tile2
        ldy #>_tile2
        jsr create_ncm_char

        ; set up initial row and col counters
        lda #21
        sta _row_counter
        lda #0
        sta _col_counter

        ; draw initial ground tiles
        jsr _draw_loop

        ; Set scroll region to rows 21-24 only (protects title at row 0)
        lda #21
        sta ncm_scroll_row_start
        lda #25
        sta ncm_scroll_row_end
        lda #0
        sta ncm_scroll_col_start
        lda #40
        sta ncm_scroll_col_end

        lda #$06
        sta ncm_scroll_speed

        ; Initialize scrolling system
        jsr ncm_scroll_init

_scroll_loop:
        ; Check keyboard
        lda $D610               ; Read keyboard buffer
        beq _scroll_loop        ; No key - keep waiting
        
        sta _key_pressed        ; Save the key
        lda #0
        sta $D610               ; Clear keyboard buffer
        
        lda _key_pressed
        
        ; Check for spacebar ($20) - exit
        cmp #$20
        beq _exit_demo
        
        ; Check for left cursor ($9D) - scroll left
        cmp #$9D
        beq _do_scroll_right
        
        ; Check for right cursor ($1D) - scroll right
        cmp #$1D
        beq _do_scroll_left
        
        jmp _scroll_loop

_do_scroll_left:
        jsr ncm_scroll_left
        jsr _fill_columns_right
        jmp _scroll_loop

_do_scroll_right:
        jsr ncm_scroll_right
        jsr _fill_columns_left
        jmp _scroll_loop

_exit_demo:
        jsr ncm_scroll_end
        rts

_key_pressed: .byte 0

_fill_columns_right:
        lda #21
        sta _fcr_row
_fcr_loop:
        ; Match the same pattern as _draw_loop
        lda _fcr_row
        sec
        sbc #21                 ; 21->0, 22->1, 23->2, 24->3
        and #1                  ; 0->0, 1->1, 2->0, 3->1
        ldx #39                 ; Column 39 (rightmost)
        ldy _fcr_row
        jsr set_ncm_char
        
        inc _fcr_row
        lda _fcr_row
        cmp #25
        bne _fcr_loop
        rts

_fcr_row: .byte 0

_fill_columns_left:
        lda #21
        sta _fcl_row
_fcl_loop:
        ; Match the same pattern as _draw_loop
        lda _fcl_row
        sec
        sbc #21                 ; 21->0, 22->1, 23->2, 24->3
        and #1                  ; 0->0, 1->1, 2->0, 3->1
        ldx #0                  ; Column 0 (leftmost)
        ldy _fcl_row
        jsr set_ncm_char
        
        inc _fcl_row
        lda _fcl_row
        cmp #25
        bne _fcl_loop
        rts

_fcl_row: .byte 0

_draw_loop:
        ; Row 21 = tile 0, Row 22 = tile 1, Row 23 = tile 0, Row 24 = tile 1
        lda _row_counter
        sec
        sbc #21                 ; 21->0, 22->1, 23->2, 24->3
        and #1                  ; 0->0, 1->1, 2->0, 3->1
        ldx _col_counter
        ldy _row_counter
        jsr set_ncm_char
        
        inc _col_counter
        lda _col_counter
        cmp #40
        beq _done_col
        jmp _draw_loop
_done_col:
        lda #0
        sta _col_counter
        inc _row_counter
        lda _row_counter
        cmp #25
        beq _draw_done
        jmp _draw_loop
_draw_done:
        rts

_row_counter:
        .byte 21
_col_counter:
        .byte 0

_title:
        .text "NCM 40 Column Custom Characters",$00
_title2:
        .text "cursor left / right to scroll",$00

;---------------------------------------------------------------------------------------
; NCM Character Data (64 bytes each: 8 rows × 8 bytes, 1 byte per pixel)
;---------------------------------------------------------------------------------------

_tile1:
        .byte $02, $03, $03, $03, $01, $02, $03, $02
        .byte $02, $02, $02, $02, $01, $02, $02, $01
        .byte $02, $02, $02, $02, $01, $02, $02, $01
        .byte $02, $02, $02, $02, $01, $02, $02, $01
        .byte $02, $02, $02, $02, $01, $01, $02, $01
        .byte $02, $02, $02, $02, $01, $01, $01, $01
        .byte $02, $02, $02, $02, $01, $03, $03, $01
        .byte $02, $02, $02, $02, $01, $02, $02, $01
_tile2:
        .byte $02, $02, $02, $02, $01, $02, $02, $01
        .byte $02, $02, $02, $02, $01, $02, $02, $01
        .byte $01, $02, $02, $02, $01, $02, $02, $01
        .byte $03, $01, $02, $02, $01, $02, $02, $01
        .byte $02, $03, $01, $01, $02, $02, $02, $01
        .byte $02, $02, $03, $01, $02, $02, $02, $01
        .byte $02, $02, $02, $01, $02, $02, $02, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $01


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