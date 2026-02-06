;=======================================================================================
; demo_cube.asm - Rotating 3D wireframe cube for MEGA65
;
; ALL math in software - no hardware multiplier/divider used.
; Uses shift-and-add for 8x8 signed multiply.
; Uses reciprocal lookup table for perspective (no division).
; Syncs to vblank to avoid flicker.
;=======================================================================================

CUBE_SZ = 40
CTR_X   = 160
CTR_Y   = 100
Z_OFF   = 180

N_VERT  = 8
N_EDGE  = 12

demo_cube:
        lda #MODE_BITMAP40
        jsr set_screen_mode
        lda #$00
        sta BORDERCOL
        sta BACKCOL
        jsr clear_bitmap

        lda #0
        sta _dc_aX
        sta _dc_aY

        ; Initial transform
        jsr _dc_xform
        ; Copy current -> old
        ldx #(N_VERT*2)-1
-       lda _dc_scx,x
        sta _dc_ocx,x
        dex
        bpl -
        ldx #N_VERT-1
-       lda _dc_scy,x
        sta _dc_ocy,x
        dex
        bpl -

_dc_mainloop:
        jsr $FFE4
        cmp #' '
        beq _dc_quit

        ; Wait for vblank (raster line >= 200)
-       lda $D012
        cmp #210
        bcc -

        ; Erase old edges
        jsr _dc_erase

        ; Advance angles
        clc
        lda _dc_aY
        adc #2
        sta _dc_aY
        clc
        lda _dc_aX
        adc #1
        sta _dc_aX

        ; Transform + project
        jsr _dc_xform

        ; Draw new edges
        jsr _dc_draw

        ; Copy current -> old
        ldx #(N_VERT*2)-1
-       lda _dc_scx,x
        sta _dc_ocx,x
        dex
        bpl -
        ldx #N_VERT-1
-       lda _dc_scy,x
        sta _dc_ocy,x
        dex
        bpl -

        jmp _dc_mainloop

_dc_quit:
        rts

_dc_aX: .byte 0
_dc_aY: .byte 0

;=======================================================================================
; Cube data
;=======================================================================================
_dc_vx: .char -CUBE_SZ, CUBE_SZ, CUBE_SZ,-CUBE_SZ,-CUBE_SZ, CUBE_SZ, CUBE_SZ,-CUBE_SZ
_dc_vy: .char -CUBE_SZ,-CUBE_SZ, CUBE_SZ, CUBE_SZ,-CUBE_SZ,-CUBE_SZ, CUBE_SZ, CUBE_SZ
_dc_vz: .char -CUBE_SZ,-CUBE_SZ,-CUBE_SZ,-CUBE_SZ, CUBE_SZ, CUBE_SZ, CUBE_SZ, CUBE_SZ

_dc_e0: .byte 0,1,2,3, 4,5,6,7, 0,1,2,3
_dc_e1: .byte 1,2,3,0, 5,6,7,4, 4,5,6,7

; Screen coords
_dc_scx: .fill N_VERT*2, 0     ; 16-bit screen X
_dc_scy: .fill N_VERT, 0       ; 8-bit screen Y
_dc_ocx: .fill N_VERT*2, 0     ; old screen X
_dc_ocy: .fill N_VERT, 0       ; old screen Y

;=======================================================================================
; _dc_erase / _dc_draw - Draw all edges in black / green
;=======================================================================================
_dc_erase:
        lda #$00
        sta _dc_lcol
        lda #1
        sta _dc_use_old
        jmp _dc_draw_edges

_dc_draw:
        lda #$0D
        sta _dc_lcol
        lda #0
        sta _dc_use_old

_dc_draw_edges:
        ldx #0
_dc_de_lp:
        stx _dc_eidx

        ; Vertex indices
        lda _dc_e0,x
        sta _dc_v0
        lda _dc_e1,x
        sta _dc_v1

        lda _dc_use_old
        bne _dc_de_old

        ; Current coords
        lda _dc_v0
        asl
        tay
        lda _dc_scx,y
        sta line_x0
        lda _dc_scx+1,y
        sta line_x0+1
        ldx _dc_v0
        lda _dc_scy,x
        sta line_y0
        lda _dc_v1
        asl
        tay
        lda _dc_scx,y
        sta line_x1
        lda _dc_scx+1,y
        sta line_x1+1
        ldx _dc_v1
        lda _dc_scy,x
        sta line_y1
        jmp _dc_de_go

_dc_de_old:
        lda _dc_v0
        asl
        tay
        lda _dc_ocx,y
        sta line_x0
        lda _dc_ocx+1,y
        sta line_x0+1
        ldx _dc_v0
        lda _dc_ocy,x
        sta line_y0
        lda _dc_v1
        asl
        tay
        lda _dc_ocx,y
        sta line_x1
        lda _dc_ocx+1,y
        sta line_x1+1
        ldx _dc_v1
        lda _dc_ocy,x
        sta line_y1

_dc_de_go:
        lda _dc_lcol
        sta line_col
        jsr draw_line
        ldx _dc_eidx
        inx
        cpx #N_EDGE
        bne _dc_de_lp
        rts

_dc_lcol:    .byte 0
_dc_use_old: .byte 0
_dc_eidx:    .byte 0
_dc_v0:      .byte 0
_dc_v1:      .byte 0

;=======================================================================================
; _dc_xform - Rotate and project all vertices
;=======================================================================================
_dc_xform:
        ldx _dc_aY
        lda _dc_sin,x
        sta _dc_sinv_y
        txa
        clc
        adc #64
        tax
        lda _dc_sin,x
        sta _dc_cosv_y

        ldx _dc_aX
        lda _dc_sin,x
        sta _dc_sinv_x
        txa
        clc
        adc #64
        tax
        lda _dc_sin,x
        sta _dc_cosv_x

        ldx #0
_dc_xf_lp:
        stx _dc_vidx

        ; === Y rotation ===
        ; x' = vx*cosY - vz*sinY
        lda _dc_vx,x
        sta _dc_ma
        lda _dc_cosv_y
        sta _dc_mb
        jsr _dc_smul8           ; result in _dc_mr (16-bit)
        lda _dc_mr
        sta _dc_tx
        lda _dc_mr+1
        sta _dc_tx+1

        ldx _dc_vidx
        lda _dc_vz,x
        sta _dc_ma
        lda _dc_sinv_y
        sta _dc_mb
        jsr _dc_smul8
        sec
        lda _dc_tx
        sbc _dc_mr
        sta _dc_tx
        lda _dc_tx+1
        sbc _dc_mr+1
        sta _dc_tx+1

        ; z' = vx*sinY + vz*cosY
        ldx _dc_vidx
        lda _dc_vx,x
        sta _dc_ma
        lda _dc_sinv_y
        sta _dc_mb
        jsr _dc_smul8
        lda _dc_mr
        sta _dc_tz
        lda _dc_mr+1
        sta _dc_tz+1

        ldx _dc_vidx
        lda _dc_vz,x
        sta _dc_ma
        lda _dc_cosv_y
        sta _dc_mb
        jsr _dc_smul8
        clc
        lda _dc_tz
        adc _dc_mr
        sta _dc_tz
        lda _dc_tz+1
        adc _dc_mr+1
        sta _dc_tz+1

        ; === X rotation ===
        ; y' = vy*cosX - z'*sinX
        ldx _dc_vidx
        lda _dc_vy,x
        sta _dc_ma
        lda _dc_cosv_x
        sta _dc_mb
        jsr _dc_smul8
        lda _dc_mr
        sta _dc_ty
        lda _dc_mr+1
        sta _dc_ty+1

        lda _dc_tz              ; z' low byte as signed 8-bit
        sta _dc_ma
        lda _dc_sinv_x
        sta _dc_mb
        jsr _dc_smul8
        sec
        lda _dc_ty
        sbc _dc_mr
        sta _dc_ty
        lda _dc_ty+1
        sbc _dc_mr+1
        sta _dc_ty+1

        ; z'' = vy*sinX + z'*cosX
        ldx _dc_vidx
        lda _dc_vy,x
        sta _dc_ma
        lda _dc_sinv_x
        sta _dc_mb
        jsr _dc_smul8
        lda _dc_mr
        sta _dc_tz2
        lda _dc_mr+1
        sta _dc_tz2+1

        lda _dc_tz
        sta _dc_ma
        lda _dc_cosv_x
        sta _dc_mb
        jsr _dc_smul8
        clc
        lda _dc_tz2
        adc _dc_mr
        sta _dc_tz2
        lda _dc_tz2+1
        adc _dc_mr+1
        sta _dc_tz2+1

        ; === Perspective ===
        ; zt = z'' + Z_OFF (8-bit, range ~140..220)
        clc
        lda _dc_tz2
        adc #Z_OFF
        sta _dc_zt

        ; Screen X = CTR_X + (tx * recip[zt]) >> 8
        lda _dc_tx              ; signed 8-bit rotated x
        ldx _dc_zt
        jsr _dc_project
        ; _dc_mr has signed 16-bit offset
        clc
        lda _dc_mr
        adc #<CTR_X
        sta _dc_px
        lda _dc_mr+1
        adc #>CTR_X
        sta _dc_px+1
        ; Clamp X to 0..319
        lda _dc_px+1
        bmi _dc_clx0
        cmp #1
        bcc _dc_xfxok
        beq _dc_xfchk
        jmp _dc_clx1
_dc_xfchk:
        lda _dc_px
        cmp #<320
        bcc _dc_xfxok
_dc_clx1:
        lda #<319
        sta _dc_px
        lda #>319
        sta _dc_px+1
        jmp _dc_xfxok
_dc_clx0:
        lda #0
        sta _dc_px
        sta _dc_px+1
_dc_xfxok:
        ldx _dc_vidx
        txa
        asl
        tax
        lda _dc_px
        sta _dc_scx,x
        lda _dc_px+1
        sta _dc_scx+1,x

        ; Screen Y = CTR_Y + (ty * recip[zt]) >> 8
        lda _dc_ty
        ldx _dc_zt
        jsr _dc_project
        ; _dc_mr:_dc_mr+1 is signed 16-bit offset
        ; Compute CTR_Y + offset as 16-bit
        clc
        lda _dc_mr
        adc #CTR_Y
        sta _dc_py
        lda _dc_mr+1
        adc #0
        sta _dc_py+1
        ; Now _dc_py is 16-bit signed screen Y
        ; If high byte != 0, it's out of 0-255 range
        lda _dc_py+1
        bmi _dc_ycl0            ; negative = clamp to 0
        bne _dc_ycl199          ; > 255 = clamp to 199
        lda _dc_py
        cmp #200
        bcc _dc_yfok
_dc_ycl199:
        lda #199
        jmp _dc_yfok
_dc_ycl0:
        lda #0
_dc_yfok:
        ldx _dc_vidx
        sta _dc_scy,x

        ldx _dc_vidx
        inx
        cpx #N_VERT
        bne _dc_xf_lp
        rts

_dc_vidx:    .byte 0
_dc_sinv_y:  .byte 0
_dc_cosv_y:  .byte 0
_dc_sinv_x:  .byte 0
_dc_cosv_x:  .byte 0
_dc_tx:      .word 0
_dc_ty:      .word 0
_dc_tz:      .word 0
_dc_tz2:     .word 0
_dc_zt:      .byte 0
_dc_px:      .word 0
_dc_py:      .word 0

;=======================================================================================
; _dc_smul8 - Signed 8×8 multiply with >>7, HARDWARE ACCELERATED
;
; Input:  _dc_ma, _dc_mb (signed bytes)
; Output: _dc_mr (signed 16-bit) = (ma * mb) >> 7
;
; Uses MEGA65 hardware multiplier at $D770-$D77F.
; Sign handling done in software, unsigned multiply in hardware.
;=======================================================================================
_dc_ma: .byte 0
_dc_mb: .byte 0
_dc_mr: .word 0

_dc_smul8:
        ; Determine result sign and take absolute values
        lda #0
        sta _sm_sign

        lda _dc_ma
        bpl _sm_apos
        eor #$FF
        clc
        adc #1
        sta _dc_ma
        inc _sm_sign
_sm_apos:
        lda _dc_mb
        bpl _sm_bpos
        eor #$FF
        clc
        adc #1
        sta _dc_mb
        lda _sm_sign
        eor #1
        sta _sm_sign
_sm_bpos:
        ; Set up MULTINA = |ma| (32-bit, upper bytes = 0)
        lda _dc_ma
        sta $D770
        lda #0
        sta $D771
        sta $D772
        sta $D773

        ; Set up MULTINB = |mb| (32-bit, upper bytes = 0)
        ; Writing $D777 (last byte) triggers final recalculation
        lda _dc_mb
        sta $D774
        lda #0
        sta $D775
        sta $D776
        sta $D777

        ; Multiplier is 1 cycle - result available now
        ; Product in MULTOUT: $D778=byte0(lo), $D779=byte1(hi)
        ; Max product: 127*127 = 16129 = $3F01
        ; Shift right 7: equivalent to (product << 1) >> 8
        ;   = take (hi << 1) | (lo >> 7) as result_lo
        ;   = hi >> 7 as result_hi (always 0 for our range)
        lda $D778               ; product lo byte
        asl                     ; bit 7 -> carry (A discarded)
        lda $D779               ; product hi byte
        rol                     ; (hi << 1) | carry = result_lo
        sta _dc_mr
        lda #0
        rol                     ; carry from above = result_hi
        sta _dc_mr+1

        ; Apply sign
        lda _sm_sign
        beq _sm_done
        sec
        lda #0
        sbc _dc_mr
        sta _dc_mr
        lda #0
        sbc _dc_mr+1
        sta _dc_mr+1
_sm_done:
        rts

_sm_sign:   .byte 0

;=======================================================================================
; _dc_project - Multiply signed coord by reciprocal, >>8, HARDWARE ACCELERATED
;
; Input:  A = signed 8-bit coordinate
;         X = z_total (index into recip table)
; Output: _dc_mr (signed 16-bit offset)
;
; Does: (|coord| × recip[zt]) >> 8 with sign, using hardware multiplier
; recip is 16-bit, so this is 8×16 = 24-bit product, take bytes 1-2
;=======================================================================================
_dc_project:
        sta _pr_coord
        stx _pr_zi

        lda #0
        sta _pr_neg
        lda _pr_coord
        bpl _pr_pos
        eor #$FF
        clc
        adc #1
        sta _pr_coord
        inc _pr_neg
_pr_pos:
        ; MULTINA = |coord| (8-bit in 32-bit register)
        lda _pr_coord
        sta $D770
        lda #0
        sta $D771
        sta $D772
        sta $D773

        ; MULTINB = recip[zi] (16-bit in 32-bit register)
        ldx _pr_zi
        lda _dc_recip_lo,x
        sta $D774
        lda _dc_recip_hi,x
        sta $D775
        lda #0
        sta $D776
        sta $D777

        ; Product is 24-bit max (8-bit × 16-bit)
        ; MULTOUT bytes: $D778=b0, $D779=b1, $D77A=b2, ...
        ; We want product >> 8 = bytes 1 and 2
        lda $D779               ; byte 1 = result lo
        sta _dc_mr
        lda $D77A               ; byte 2 = result hi
        sta _dc_mr+1

        ; Apply sign
        lda _pr_neg
        beq _pr_done
        sec
        lda #0
        sbc _dc_mr
        sta _dc_mr
        lda #0
        sbc _dc_mr+1
        sta _dc_mr+1
_pr_done:
        rts

_pr_coord:  .byte 0
_pr_zi:     .byte 0
_pr_neg:    .byte 0

;=======================================================================================
; Sin table
;=======================================================================================
_dc_sin:
        .byte $00, $03, $06, $09, $0C, $10, $13, $16
        .byte $19, $1C, $1F, $22, $25, $28, $2B, $2E
        .byte $31, $33, $36, $39, $3C, $3F, $41, $44
        .byte $47, $49, $4C, $4E, $51, $53, $55, $58
        .byte $5A, $5C, $5E, $60, $62, $64, $66, $68
        .byte $6A, $6B, $6D, $6F, $70, $71, $73, $74
        .byte $75, $76, $78, $79, $7A, $7A, $7B, $7C
        .byte $7D, $7D, $7E, $7E, $7E, $7F, $7F, $7F
        .byte $7F, $7F, $7F, $7F, $7E, $7E, $7E, $7D
        .byte $7D, $7C, $7B, $7A, $7A, $79, $78, $76
        .byte $75, $74, $73, $71, $70, $6F, $6D, $6B
        .byte $6A, $68, $66, $64, $62, $60, $5E, $5C
        .byte $5A, $58, $55, $53, $51, $4E, $4C, $49
        .byte $47, $44, $41, $3F, $3C, $39, $36, $33
        .byte $31, $2E, $2B, $28, $25, $22, $1F, $1C
        .byte $19, $16, $13, $10, $0C, $09, $06, $03
        .byte $00, $FD, $FA, $F7, $F4, $F0, $ED, $EA
        .byte $E7, $E4, $E1, $DE, $DB, $D8, $D5, $D2
        .byte $CF, $CD, $CA, $C7, $C4, $C1, $BF, $BC
        .byte $B9, $B7, $B4, $B2, $AF, $AD, $AB, $A8
        .byte $A6, $A4, $A2, $A0, $9E, $9C, $9A, $98
        .byte $96, $95, $93, $91, $90, $8F, $8D, $8C
        .byte $8B, $8A, $88, $87, $86, $86, $85, $84
        .byte $83, $83, $82, $82, $82, $81, $81, $81
        .byte $81, $81, $81, $81, $82, $82, $82, $83
        .byte $83, $84, $85, $86, $86, $87, $88, $8A
        .byte $8B, $8C, $8D, $8F, $90, $91, $93, $95
        .byte $96, $98, $9A, $9C, $9E, $A0, $A2, $A4
        .byte $A6, $A8, $AB, $AD, $AF, $B2, $B4, $B7
        .byte $B9, $BC, $BF, $C1, $C4, $C7, $CA, $CD
        .byte $CF, $D2, $D5, $D8, $DB, $DE, $E1, $E4
        .byte $E7, $EA, $ED, $F0, $F4, $F7, $FA, $FD

;=======================================================================================
; Reciprocal table: recip[i] = round(150*256/i), split lo/hi
;=======================================================================================
_dc_recip_lo:
        .byte $00, $00, $00, $00, $80, $00, $00, $6E
        .byte $C0, $AB, $00, $A3, $80, $8A, $B7, $00
        .byte $60, $D3, $55, $E5, $80, $25, $D1, $86
        .byte $40, $00, $C5, $8E, $5B, $2C, $00, $D7
        .byte $B0, $8C, $69, $49, $2B, $0E, $F3, $D9
        .byte $C0, $A9, $92, $7D, $69, $55, $43, $31
        .byte $20, $10, $00, $F1, $E2, $D5, $C7, $BA
        .byte $AE, $A2, $96, $8B, $80, $76, $6B, $62
        .byte $58, $4F, $46, $3D, $35, $2D, $25, $1D
        .byte $15, $0E, $07, $00, $F9, $F3, $EC, $E6
        .byte $E0, $DA, $D4, $CF, $C9, $C4, $BF, $B9
        .byte $B4, $AF, $AB, $A6, $A1, $9D, $99, $94
        .byte $90, $8C, $88, $84, $80, $7C, $78, $75
        .byte $71, $6E, $6A, $67, $64, $60, $5D, $5A
        .byte $57, $54, $51, $4E, $4B, $48, $45, $43
        .byte $40, $3D, $3B, $38, $36, $33, $31, $2E
        .byte $2C, $2A, $27, $25, $23, $21, $1F, $1C
        .byte $1A, $18, $16, $14, $12, $10, $0E, $0D
        .byte $0B, $09, $07, $05, $03, $02, $00, $FE
        .byte $FD, $FB, $F9, $F8, $F6, $F5, $F3, $F2
        .byte $F0, $EF, $ED, $EC, $EA, $E9, $E7, $E6
        .byte $E5, $E3, $E2, $E1, $DF, $DE, $DD, $DB
        .byte $DA, $D9, $D8, $D7, $D5, $D4, $D3, $D2
        .byte $D1, $D0, $CE, $CD, $CC, $CB, $CA, $C9
        .byte $C8, $C7, $C6, $C5, $C4, $C3, $C2, $C1
        .byte $C0, $BF, $BE, $BD, $BC, $BB, $BA, $BA
        .byte $B9, $B8, $B7, $B6, $B5, $B4, $B3, $B3
        .byte $B2, $B1, $B0, $AF, $AF, $AE, $AD, $AC
        .byte $AB, $AB, $AA, $A9, $A8, $A8, $A7, $A6
        .byte $A6, $A5, $A4, $A3, $A3, $A2, $A1, $A1
        .byte $A0, $9F, $9F, $9E, $9D, $9D, $9C, $9B
        .byte $9B, $9A, $9A, $99, $98, $98, $97, $97

_dc_recip_hi:
        .byte $00, $96, $4B, $32, $25, $1E, $19, $15
        .byte $12, $10, $0F, $0D, $0C, $0B, $0A, $0A
        .byte $09, $08, $08, $07, $07, $07, $06, $06
        .byte $06, $06, $05, $05, $05, $05, $05, $04
        .byte $04, $04, $04, $04, $04, $04, $03, $03
        .byte $03, $03, $03, $03, $03, $03, $03, $03
        .byte $03, $03, $03, $02, $02, $02, $02, $02
        .byte $02, $02, $02, $02, $02, $02, $02, $02
        .byte $02, $02, $02, $02, $02, $02, $02, $02
        .byte $02, $02, $02, $02, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $01
        .byte $01, $01, $01, $01, $01, $01, $01, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00
        .byte $00, $00, $00, $00, $00, $00, $00, $00