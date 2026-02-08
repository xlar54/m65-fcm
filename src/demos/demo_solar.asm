;=======================================================================================
; demo_solar - Solar System scene for 80-col FCM bitmap mode
; Showcases: gradient rect, plot_pixel, filled/outline circles, lines, polygons, text
; Requires: polygon.asm included
;=======================================================================================

msg_solar:
        .text "MEGA65 FCM Solar System",$00

demo_solar:
        ; Set background and border
        lda #$00                ; screen color
        sta BACKCOL
        lda #$00                ; border
        sta BORDERCOL

        lda #MODE_BITMAP80
        jsr set_screen_mode

        ;===================================================================
        ; 1. Background: vertical gradient, deep blue to black
        ;===================================================================
        lda #1                  ; vertical
        sta grad_dir
        lda #0
        sta grad_x
        sta grad_x+1
        sta grad_y
        lda #<640
        sta grad_w
        lda #>640
        sta grad_w+1
        lda #200
        sta grad_h
        lda #$00                ; start R
        sta grad_r1
        lda #$00                ; start G
        sta grad_g1
        lda #$04                ; start B: dark blue
        sta grad_b1
        lda #$00
        sta grad_r2
        sta grad_g2
        sta grad_b2             ; end: black
        lda #$40
        sta grad_pal
        jsr draw_gradient_rect

        ;===================================================================
        ; 2. Stars: scattered white pixels
        ;===================================================================
        lda #$60
        ldx #$0F
        ldy #$0F
        ldz #$0F
        jsr set_palette_color   ; $60 = bright white

        lda #$61
        ldx #$0A
        ldy #$0A
        ldz #$0C
        jsr set_palette_color   ; $61 = dim blue-white

        lda #$60
        sta plot_col
        ldx #0
_star_loop:
        stx _sl_idx
        txa
        asl
        tax
        lda _star_x_lo,x
        sta plot_x
        lda _star_x_lo+1,x
        sta plot_x+1
        ldx _sl_idx
        lda _star_y,x
        sta plot_y
        ; Alternate bright/dim
        txa
        and #$03
        bne +
        lda #$61
        sta plot_col
        jmp _star_plot
+       lda #$60
        sta plot_col
_star_plot:
        jsr plot_pixel
        ldx _sl_idx
        inx
        cpx #25
        bne _star_loop

        ;===================================================================
        ; 3. Set up all palette colors for sun, planets, etc.
        ;===================================================================

        ; Sun layers (inner to outer)
        lda #$70                ; corona glow
        ldx #$05
        ldy #$01
        ldz #$00
        jsr set_palette_color

        lda #$71                ; deep red
        ldx #$0A
        ldy #$02
        ldz #$00
        jsr set_palette_color

        lda #$72                ; orange
        ldx #$0F
        ldy #$07
        ldz #$00
        jsr set_palette_color

        lda #$73                ; yellow
        ldx #$0F
        ldy #$0D
        ldz #$02
        jsr set_palette_color

        lda #$74                ; bright core
        ldx #$0F
        ldy #$0F
        ldz #$08
        jsr set_palette_color

        ; Orbit color
        lda #$78                ; dark blue-grey
        ldx #$02
        ldy #$02
        ldz #$04
        jsr set_palette_color

        ; Planet colors
        lda #$80                ; Mercury: grey
        ldx #$08
        ldy #$08
        ldz #$08
        jsr set_palette_color

        lda #$81                ; Venus: pale tan
        ldx #$0D
        ldy #$0B
        ldz #$06
        jsr set_palette_color

        lda #$82                ; Earth: ocean blue
        ldx #$02
        ldy #$05
        ldz #$0F
        jsr set_palette_color

        lda #$83                ; Earth: land green
        ldx #$02
        ldy #$09
        ldz #$03
        jsr set_palette_color

        lda #$84                ; Moon: light grey
        ldx #$0C
        ldy #$0C
        ldz #$0C
        jsr set_palette_color

        lda #$85                ; Mars: rust red
        ldx #$0C
        ldy #$04
        ldz #$02
        jsr set_palette_color

        lda #$86                ; Jupiter: tan
        ldx #$0C
        ldy #$09
        ldz #$05
        jsr set_palette_color

        lda #$87                ; Jupiter: dark band
        ldx #$08
        ldy #$05
        ldz #$03
        jsr set_palette_color

        lda #$88                ; Saturn: gold
        ldx #$0D
        ldy #$0B
        ldz #$05
        jsr set_palette_color

        lda #$89                ; Saturn ring
        ldx #$0A
        ldy #$09
        ldz #$04
        jsr set_palette_color

        lda #$8A                ; Asteroid: brown
        ldx #$07
        ldy #$05
        ldz #$03
        jsr set_palette_color

        lda #$8B                ; Title text
        ldx #$06
        ldy #$08
        ldz #$0C
        jsr set_palette_color

        ;===================================================================
        ; 4. Orbits (drawn first so sun covers the centers)
        ;===================================================================

        ; All orbits centered at (320, 100)
        lda #<320
        sta circ_cx
        lda #>320
        sta circ_cx+1
        lda #100
        sta circ_cy
        lda #$78
        sta circ_col

        lda #30                 ; Mercury orbit
        sta circ_r
        clc
        jsr draw_circle

        lda #<320
        sta circ_cx
        lda #>320
        sta circ_cx+1
        lda #100
        sta circ_cy
        lda #$78
        sta circ_col
        lda #42                 ; Venus orbit
        sta circ_r
        clc
        jsr draw_circle

        lda #<320
        sta circ_cx
        lda #>320
        sta circ_cx+1
        lda #100
        sta circ_cy
        lda #$78
        sta circ_col
        lda #56                 ; Earth orbit
        sta circ_r
        clc
        jsr draw_circle

        lda #<320
        sta circ_cx
        lda #>320
        sta circ_cx+1
        lda #100
        sta circ_cy
        lda #$78
        sta circ_col
        lda #70                 ; Mars orbit
        sta circ_r
        clc
        jsr draw_circle

        lda #<320
        sta circ_cx
        lda #>320
        sta circ_cx+1
        lda #100
        sta circ_cy
        lda #$78
        sta circ_col
        lda #88                 ; Jupiter orbit
        sta circ_r
        clc
        jsr draw_circle

        lda #<320
        sta circ_cx
        lda #>320
        sta circ_cx+1
        lda #100
        sta circ_cy
        lda #$78
        sta circ_col
        lda #98                 ; Saturn orbit
        sta circ_r
        clc
        jsr draw_circle

        ;===================================================================
        ; 5. Sun - concentric filled circles, largest first
        ;    Center: (320, 100)
        ;===================================================================

        lda #<320
        sta circ_cx
        lda #>320
        sta circ_cx+1
        lda #100
        sta circ_cy

        lda #22                 ; corona
        sta circ_r
        lda #$70
        sta circ_col
        sec
        jsr draw_circle

        lda #<320
        sta circ_cx
        lda #>320
        sta circ_cx+1
        lda #100
        sta circ_cy
        lda #18                 ; deep red
        sta circ_r
        lda #$71
        sta circ_col
        sec
        jsr draw_circle

        lda #<320
        sta circ_cx
        lda #>320
        sta circ_cx+1
        lda #100
        sta circ_cy
        lda #14                 ; orange
        sta circ_r
        lda #$72
        sta circ_col
        sec
        jsr draw_circle

        lda #<320
        sta circ_cx
        lda #>320
        sta circ_cx+1
        lda #100
        sta circ_cy
        lda #10                 ; yellow
        sta circ_r
        lda #$73
        sta circ_col
        sec
        jsr draw_circle

        lda #<320
        sta circ_cx
        lda #>320
        sta circ_cx+1
        lda #100
        sta circ_cy
        lda #5                  ; bright core
        sta circ_r
        lda #$74
        sta circ_col
        sec
        jsr draw_circle

        ;===================================================================
        ; 6. Planets
        ;===================================================================

        ; --- Mercury at (350, 126) r=2 ---
        lda #<350
        sta circ_cx
        lda #>350
        sta circ_cx+1
        lda #126
        sta circ_cy
        lda #2
        sta circ_r
        lda #$80
        sta circ_col
        sec
        jsr draw_circle

        ; --- Venus at (241, 114) r=3 ---
        lda #<241
        sta circ_cx
        lda #>241
        sta circ_cx+1
        lda #114
        sta circ_cy
        lda #3
        sta circ_r
        lda #$81
        sta circ_col
        sec
        jsr draw_circle

        ; --- Earth at (282, 47) r=4 ---
        ;     Two-tone: blue ocean then green land overlay
        lda #<282
        sta circ_cx
        lda #>282
        sta circ_cx+1
        lda #47
        sta circ_cy
        lda #4
        sta circ_r
        lda #$82                ; ocean blue
        sta circ_col
        sec
        jsr draw_circle

        ; Earth land mass: small filled circle offset
        lda #<284
        sta circ_cx
        lda #>284
        sta circ_cx+1
        lda #46
        sta circ_cy
        lda #2
        sta circ_r
        lda #$83                ; green
        sta circ_col
        sec
        jsr draw_circle

        ; --- Moon at (296, 42) r=1 ---
        lda #<296
        sta circ_cx
        lda #>296
        sta circ_cx+1
        lda #42
        sta circ_cy
        lda #1
        sta circ_r
        lda #$84
        sta circ_col
        sec
        jsr draw_circle

        ; --- Mars at (452, 124) r=3 ---
        lda #<452
        sta circ_cx
        lda #>452
        sta circ_cx+1
        lda #124
        sta circ_cy
        lda #3
        sta circ_r
        lda #$85
        sta circ_col
        sec
        jsr draw_circle

        ; --- Jupiter at (433, 33) r=8 ---
        ;     Main body then dark band
        lda #<433
        sta circ_cx
        lda #>433
        sta circ_cx+1
        lda #33
        sta circ_cy
        lda #8
        sta circ_r
        lda #$86                ; tan body
        sta circ_col
        sec
        jsr draw_circle

        ; Jupiter dark band: horizontal line across center
        lda #<419
        sta line_x0
        lda #>419
        sta line_x0+1
        lda #34
        sta line_y0
        lda #<447
        sta line_x1
        lda #>447
        sta line_x1+1
        lda #34
        sta line_y1
        lda #$87
        sta line_col
        jsr draw_line

        ; Second band
        lda #<421
        sta line_x0
        lda #>421
        sta line_x0+1
        lda #31
        sta line_y0
        lda #<445
        sta line_x1
        lda #>445
        sta line_x1+1
        lda #31
        sta line_y1
        lda #$87
        sta line_col
        jsr draw_line

        ; --- Saturn at (170, 163) r=6 ---
        lda #<170
        sta circ_cx
        lda #>170
        sta circ_cx+1
        lda #163
        sta circ_cy
        lda #6
        sta circ_r
        lda #$88                ; gold body
        sta circ_col
        sec
        jsr draw_circle

        ; Saturn ring: angled line through planet
        lda #$89
        sta line_col

        lda #<146
        sta line_x0
        lda #>146
        sta line_x0+1
        lda #166
        sta line_y0
        lda #<194
        sta line_x1
        lda #>194
        sta line_x1+1
        lda #160
        sta line_y1
        jsr draw_line

        lda #<148
        sta line_x0
        lda #>148
        sta line_x0+1
        lda #167
        sta line_y0
        lda #<192
        sta line_x1
        lda #>192
        sta line_x1+1
        lda #161
        sta line_y1
        jsr draw_line

        ; Upper ring (in front of planet)
        lda #<148
        sta line_x0
        lda #>148
        sta line_x0+1
        lda #159
        sta line_y0
        lda #<192
        sta line_x1
        lda #>192
        sta line_x1+1
        lda #165
        sta line_y1
        jsr draw_line

        lda #<150
        sta line_x0
        lda #>150
        sta line_x0+1
        lda #158
        sta line_y0
        lda #<190
        sta line_x1
        lda #>190
        sta line_x1+1
        lda #164
        sta line_y1
        jsr draw_line

        ;===================================================================
        ; 7. Asteroid belt - small polygons between Mars & Jupiter orbits
        ;===================================================================

        ; Asteroid 1: triangle at (320, 179) r=3
        lda #<320
        sta poly_cx
        lda #>320
        sta poly_cx+1
        lda #179
        sta poly_cy
        lda #3
        sta poly_r
        lda #3                  ; triangle
        sta poly_sides
        lda #$8A
        sta poly_col
        sec                     ; filled
        jsr draw_polygon

        ; Asteroid 2: pentagon at (172, 73) r=4
        lda #<172
        sta poly_cx
        lda #>172
        sta poly_cx+1
        lda #73
        sta poly_cy
        lda #4
        sta poly_r
        lda #5                  ; pentagon
        sta poly_sides
        lda #$8A
        sta poly_col
        clc                     ; outline
        jsr draw_polygon

        ; Asteroid 3: triangle at (476, 86) r=3
        lda #<476
        sta poly_cx
        lda #>476
        sta poly_cx+1
        lda #86
        sta poly_cy
        lda #3
        sta poly_r
        lda #3                  ; triangle
        sta poly_sides
        lda #$8A
        sta poly_col
        sec                     ; filled
        jsr draw_polygon

        ; Asteroid 4: quad at (240, 176) r=2
        lda #<240
        sta poly_cx
        lda #>240
        sta poly_cx+1
        lda #176
        sta poly_cy
        lda #2
        sta poly_r
        lda #4                  ; square
        sta poly_sides
        lda #$8A
        sta poly_col
        sec
        jsr draw_polygon

        ; Asteroid 5: triangle at (410, 175) r=2
        lda #<410
        sta poly_cx
        lda #>410
        sta poly_cx+1
        lda #175
        sta poly_cy
        lda #2
        sta poly_r
        lda #3
        sta poly_sides
        lda #$8A
        sta poly_col
        sec
        jsr draw_polygon

        ;===================================================================
        ; 8. Title text
        ;===================================================================
        lda #<msg_solar
        sta str_ptr
        lda #>msg_solar
        sta str_ptr+1
        lda #24                 ; bottom row
        sta str_row
        lda #29                 ; roughly centered in 80 cols
        sta str_col
        lda #$8B
        sta str_color
        jsr draw_petscii_string

        ;===================================================================
        ; Wait for space bar
        ;===================================================================
_solar_wait:
        jsr $FFE4
        cmp #' '
        bne _solar_wait
        rts


;---------------------------------------------------------------------------------------
; Star position data: 25 stars
; Stored as word pairs (x_lo, x_hi) and separate y table
;---------------------------------------------------------------------------------------
_sl_idx:        .byte 0

_star_x_lo:
        .word   18,  580,   42,  598,   12      ;  0- 4
        .word  628,   95,  548,  500,   58      ;  5- 9
        .word  535,   15,  588,   38,  608      ; 10-14
        .word   78,  518,  558,   28,  478      ; 15-19
        .word  140,  400,  260,  570,   68      ; 20-24

_star_y:
        .byte    8,  14,  182,  178,   92       ;  0- 4
        .byte   48,   9,  188,  12,    42       ;  5- 9
        .byte   95,  158,  128,  22,   62       ; 10-14
        .byte  142,  172,   38,  55,  148       ; 15-19
        .byte    6,  190,  192,  25,  170       ; 20-24