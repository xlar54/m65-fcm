; ========================================================================================
; demo_text80
;
; This code demonstrates using this library to create custom characters that use 
; 640x200 at 256 colors (FCM)
;=========================================================================================

demo_text80:

        lda #MODE_TEXT80
        jsr set_screen_mode
        jsr init_palette

        lda #$00                ; space
        jsr clear_screen_ram

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

        ; draw some custom chars
        lda #1
        ldx #10
        ldy #10
        jsr draw_char

        lda #2
        ldx #11
        ldy #10
        jsr draw_char

        ; draw folder using custom chars

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

        jsr WAIT_SPACEBAR

        rts