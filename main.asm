!cpu 6510

DEBUG = 1
RELEASE = 1
; ==============================================================================
ENABLE              = $20
DISABLE             = $2c

FLAG_LOOP           = %00000001
FLAG_END            = %00000000
FLAG_SW             = %00000010
FLAG_NT             = %00000000

VOLUME_SW           = $11b1
VOLUME_NT           = $108c

NUM_IRQs            = 3

LINE1               = $00

BLACK               = $00
WHITE               = $01
RED                 = $02
CYAN                = $03
PURPLE              = $04
GREEN               = $05
BLUE                = $06
YELLOW              = $07
ORANGE              = $08
BROWN               = $09
PINK                = $0A
DARK_GREY           = $0B
GREY                = $0C
LIGHT_GREEN         = $0D
LIGHT_BLUE          = $0E
LIGHT_GREY          = $0F

DISPLAY_START_X     = $00
DISPLAY_START_Y     = $08

TDISPL_X            = 27
TDISPL_Y            = $07

COLORBG1            = BLACK
COLORBG2            = BLUE
COLORTEXT           = LIGHT_BLUE
COLORHIGHLIGHT      = LIGHT_GREEN

SPIDERCOL           = GREY
LAZYCOL             = DARK_GREY

KEY_CRSRUP          = $91
KEY_CRSRDOWN        = $11
KEY_RETURN          = $0d

; ==============================================================================
zp_start            = $02
num_songs           = zp_start
cur_song            = num_songs+1
zp_temp             = cur_song+1
zp_temp_lo          = zp_temp
zp_temp_hi          = zp_temp_lo+1
new_song            = zp_temp_hi+1
irq_ready           = new_song+1
tune_end_flag       = irq_ready+1
cur_data            = tune_end_flag+1
;cur_data SIZE 8
cur_colram          = cur_data+8
cur_colram_lo       = cur_colram
cur_colram_hi       = cur_colram_lo+1
marker_pos          = cur_colram_hi+1
marker_pos_lo       = marker_pos
marker_pos_hi       = marker_pos_lo+1
marker_song         = marker_pos_hi+1

; ==============================================================================
getin               = $ffe4
keyscan             = $EA87

code_start          = $3500
data_block01        = $4000
data_block02        = $e000
vicbank             = $0000
charset0            = vicbank+$1800
vidmem0             = vicbank+$0400
sprite_data         = vicbank+$0800
sprite_base         = <((sprite_data-vicbank)/$40)
dd00_val0           = <!(vicbank/$4000) & 3
d018_val0           = <(((vidmem0-vicbank)/$400) << 4)+ <(((charset0-vicbank)/$800) << 1)
music_init          = $1000
music_play          = $1003
                    *= sprite_data
                    !bin "spider-sprite.bin"
                    !bin "alazyyear.bin"
spr_myd:            !bin "myd_big.bin"
spr_empty:          !fi $40, 0
spr_myd_base        = <((spr_myd-vicbank)/$40)
; ==============================================================================
                    *= code_start
                    jmp init_code
fake:               rts
!zone IRQ
irq:                pha
                    txa
                    pha
                    tya
                    pha

                    lda $d012
-                   cmp $d012
                    beq -

irq_next:           jmp irq1

irq_end:            ldx #0
                    lda irq_tab_lo,x
                    sta irq_next+1
                    lda irq_tab_hi,x
                    sta irq_next+2
                    lda irq_lines,x
                    sta $d012
                    inc irq_end+1
                    lda irq_end+1
                    cmp #NUM_IRQs
                    bne +
                    lda #0
                    sta irq_end+1
+                   asl $d019
                    pla
                    tay
                    pla
                    tax
                    pla
nmi:                rti

irq1:
enable_music:       bit music_play
enable_timer:       bit timer_increase
enable_timer_check: bit timer_check
                    jsr sprites_set
                    !if DEBUG=1 { dec $d020 }
                    jmp irq_end

irq2:               ldx #$09
-                   dex
                    bne -
                    nop
--                  ldy #$07                ;2
                    lda .raster1,x          ;4
                    sta $d020               ;4
                    sta $d021               ;4
                    inx                     ;2
                    cpx #14                 ;2
                    beq +                   ;2
                    nop                     ;2 _20
-                   lda .raster1,x          ;4
                    sta $d020               ;4
                    sta $d021               ;4
                    jsr fake                ;12
                    jsr fake                ;12
                    jsr fake                ;12 _48
                    nop                     ;2
                    inx                     ;2
                    cpx #14                 ;2
                    beq +                   ;2
                    dey                     ;2
                    beq --                  ;2 / 3 _61 (+2)
                    bne -                   ;3     _63
+                   nop
                    nop
                    lda #1
                    sta irq_ready
                    jmp irq_end

irq3:               ldx #$09
-                   dex
                    bne -
                    nop
--                  ldy #$07                ;2
                    lda .raster2,x          ;4
                    sta $d020               ;4
                    sta $d021               ;4
                    inx                     ;2
                    cpx #16                 ;2
                    beq +                   ;2
                    nop                     ;2 _20
-                   lda .raster2,x          ;4
                    sta $d020               ;4
                    sta $d021               ;4
                    jsr fake                ;12
                    jsr fake                ;12
                    jsr fake                ;12 _48
                    nop                     ;2
                    inx                     ;2
                    cpx #16                 ;2
                    beq +                   ;2
                    dey                     ;2
                    beq --                  ;2 / 3 _61 (+2)
                    bne -                   ;3     _63
+                   nop
                    nop
                    jmp irq_end

irq_tab_lo:         !byte <irq1, <irq2, <irq3
irq_tab_hi:         !byte >irq1, >irq2, >irq3
irq_lines:          !byte LINE1, $59, $e1

.raster1:           !byte COLORBG1, COLORBG2, COLORBG1, COLORBG1
                    !byte COLORBG2, COLORBG1, COLORBG2, COLORBG2
                    !byte COLORBG1, COLORBG2, COLORBG2, COLORBG2
                    !byte COLORBG1, COLORBG2

.raster2:           !byte COLORBG2, COLORBG2, COLORBG2, COLORBG1
                    !byte COLORBG2, COLORBG2, COLORBG2, COLORBG1
                    !byte COLORBG2, COLORBG2, COLORBG1, COLORBG2
                    !byte COLORBG1, COLORBG1, COLORBG2, COLORBG1
; ==============================================================================
!zone DECRUNCH
exod_addr:          !src "wrap.asm"
                    !src "exodecrunch.asm"
decrunch_song:      lda cur_data
                    sta opbase+1
                    lda cur_data+1
                    sta opbase+2
                    jsr exod_addr
                    rts
; ==============================================================================
!zone INIT
init_code:          sei
                    lda #$7f
                    sta $dc0d
                    sta $dd0d
                    lda $dc0d
                    lda $dd0d
                    lda #$35
                    sta $01
                    jsr init_vic
                    jsr init_zp
                    jsr init_scr_and_songs
                    jsr init_irq
                    cli
                    jmp mainloop
init_zp:            lda #0
                    ldx #$fd
-                   sta zp_start-1,x
                    dex
                    bne -
                    lda #0
                    sta tune_end_flag
                    lda #1
                    sta new_song
                    rts
init_irq:           lda #$01
                    sta $d01a
                    lda #LINE1
                    sta $d012
                    lda #<irq
                    sta $fffe
                    lda #>irq
                    sta $ffff
                    lda #<nmi
                    sta $fffa
                    lda #>nmi
                    sta $fffb
                    lda #$1b
                    sta $d011
                    asl $d019
                    rts
init_vic:           lda #$0b
                    sta $d011
                    lda #dd00_val0
                    sta $dd00
                    lda #d018_val0
                    sta $d018
                    lda #$08
                    sta $d016
                    rts
init_scr_and_songs: lda #COLORBG2
                    sta $d020
                    sta $d021
                    ldx #0
-                   lda #COLORTEXT
                    sta $d800+$000,x
                    sta $d800+$100,x
                    sta $d800+$200,x
                    sta $d800+$2e8,x
                    lda #' '
                    sta vidmem0+$000,x
                    sta vidmem0+$100,x
                    sta vidmem0+$200,x
                    sta vidmem0+$2e8,x
                    inx
                    bne -
                    lda #<vidmem0+(40*DISPLAY_START_Y)+DISPLAY_START_X
                    sta marker_pos_lo
                    sta zp_temp_lo
                    lda #>vidmem0+(40*DISPLAY_START_Y)+DISPLAY_START_X
                    sta marker_pos_hi
                    sta zp_temp_hi
                    ldx #$00
-                   stx num_songs
                    txa
                    jsr print_snum
                    lda songtable,x
                    jsr print_stitle
                    clc
                    lda zp_temp_lo
                    adc #40
                    sta zp_temp_lo
                    lda zp_temp_hi
                    adc #0
                    sta zp_temp_hi
                    inx
                    lda songtable,x
                    bne -
                    lda num_songs
                    sta cur_song
                    lda #<(vidmem0+(40*TDISPL_Y)+TDISPL_X)
                    sta zp_temp_lo
                    lda #>(vidmem0+(40*TDISPL_Y)+TDISPL_X)
                    sta zp_temp_hi
                    ldy #$0c
-                   lda time_tmpl,y
                    sta ( zp_temp ),y
                    dey
                    bpl -
                    lda #<$d800+(40*DISPLAY_START_Y)+DISPLAY_START_X
                    sta cur_colram_lo
                    lda #>$d800+(40*DISPLAY_START_Y)+DISPLAY_START_X
                    sta cur_colram_hi
                    ldy #1
-                   lda ( marker_pos ),y
                    eor #$80
                    sta ( marker_pos ),y
                    dey
                    bpl -
                    ldx #18
-                   lda .square_tmpl,x
                    sta vidmem0+(0*40),x
                    sta vidmem0+(1*40),x
                    sta vidmem0+(2*40),x
                    sta vidmem0+(3*40),x
                    lda .sqcol_tmpl,x
                    sta $d800+(0*40),x
                    sta $d800+(1*40),x
                    sta $d800+(2*40),x
                    sta $d800+(3*40),x
                    dex
                    bpl -
                    !if DEBUG = 1 {
                        lda #160
                        sta vidmem0+(4*40)
                        lda #WHITE
                        sta $d800+(4*40)
                    }
                    rts
time_tmpl:          !scr "00:00 / 00:00"
.square_tmpl:       !byte 160, 160, 160, 160, $20, 160, 160, 160, 160, $20
                    !byte 160, 160, 160, 160, $20, 160, 160, 160, 160, $20
.sqcol_tmpl:        !byte GREEN, GREEN, GREEN, GREEN, COLORBG1
                    !byte LIGHT_GREEN, LIGHT_GREEN, LIGHT_GREEN, LIGHT_GREEN, COLORBG1
                    !byte PURPLE, PURPLE, PURPLE, PURPLE, COLORBG1
                    !byte PINK, PINK, PINK, PINK, COLORBG1
init_next_tune:     lda #DISABLE
                    sta enable_music
                    sta enable_timer
                    sta enable_timer_check
                    lda #0
                    sta $d418
                    lda #COLORTEXT
                    ldx cur_song
                    jsr print_colored_line
                    lda cur_song
                    cmp num_songs
                    bne +
                    lda #$ff
                    sta cur_song
+                   inc cur_song
                    jsr read_cur_data
                    jsr decrunch_song
                    lda #COLORHIGHLIGHT
                    ldx cur_song
                    jsr print_colored_line
                    lda #0
                    jsr music_init
                    jsr timer_init
                    jsr wait_irq
                    lda #ENABLE
                    sta enable_music
                    sta enable_timer
                    sta enable_timer_check
                    lda #0
                    sta new_song
                    rts
; ==============================================================================
!zone MAIN
mainloop:           jsr wait_irq
                    !if DEBUG=1 {
                        lda cur_song
                        sta vidmem0
                        lda tune_end_flag
                        sta vidmem0+1
                    }
                    lda new_song
                    beq +
                    jsr init_next_tune
+
                    lda tune_end_flag
                    beq +
                    lda #0
                    sta tune_end_flag
                    lda cur_data+2
                    lsr
                    bcs init_fadeloop
                    lda #1
                    sta new_song
+                   jsr keyboard_get
                    jmp mainloop

init_fadeloop:      lda cur_data+2
                    lsr
                    lsr
                    bcs .sw
.nt:                lda #<VOLUME_NT
                    sta vol_addr+1
                    lda #>VOLUME_NT
                    sta vol_addr+2
                    jmp fadeloop
.sw:                lda #<VOLUME_SW
                    sta vol_addr+1
                    lda #>VOLUME_SW
                    sta vol_addr+2
                    jmp fadeloop

fadeloop:           jsr wait_irq
                    dec .fadetime
                    lda .fadetime
                    bne fadeloop
                    lda #40
                    sta .fadetime
cur_vol:            lda #$0f
vol_addr:           sta $0000
                    beq +
                    dec cur_vol+1
                    jmp fadeloop
+                   lda #$0f
                    sta cur_vol+1
                    lda #1
                    sta new_song
                    jmp mainloop
.fadetime:          !byte 40

read_cur_data:      ldx cur_song
                    ldy songtable,x
                    dey
                    lda sdata_pt_lo,y
                    sta zp_temp_lo
                    lda sdata_pt_hi,y
                    sta zp_temp_hi

                    ldy #7
-                   lda ( zp_temp ),y
                    sta cur_data,y
                    dey
                    bpl -
                    rts
; ==============================================================================
!zone TIMER
timer_init:         ldy #4
-                   lda cur_data+3,y
                    sta time_tmpl+8,y
                    dey
                    bpl -

                    lda #<(vidmem0+(40*TDISPL_Y)+TDISPL_X)
                    sta zp_temp_lo
                    lda #>(vidmem0+(40*TDISPL_Y)+TDISPL_X)
                    sta zp_temp_hi
                    ldy #$0c
-                   lda time_tmpl,y
                    sta ( zp_temp ),y
                    dey
                    bpl -
                    rts
timer_increase:     min_cnt_hi = vidmem0+(40*TDISPL_Y)+TDISPL_X+0
                    min_cnt_lo = vidmem0+(40*TDISPL_Y)+TDISPL_X+1
                    sec_cnt_hi = vidmem0+(40*TDISPL_Y)+TDISPL_X+3
                    sec_cnt_lo = vidmem0+(40*TDISPL_Y)+TDISPL_X+4
                    dec .framecounter
                    beq +
                    rts
+                   lda sec_cnt_lo
                    cmp #$39
                    bne ++++
                    lda #$2f
                    sta sec_cnt_lo
                    lda sec_cnt_hi
                    cmp #$35
                    bne +++
                    lda #$2f
                    sta sec_cnt_hi
                    lda min_cnt_lo
                    cmp #$39
                    bne ++
                    lda #$2f
                    sta min_cnt_lo
                    lda min_cnt_hi
                    cmp #$35
                    bne +
                    lda #$2f
                    sta min_cnt_hi
+                   inc min_cnt_hi
++                  inc min_cnt_lo
+++                 inc sec_cnt_hi
++++                inc sec_cnt_lo
                    lda #50
                    sta .framecounter
                    rts
.framecounter:      !byte 50
timer_check:        min_end_hi = vidmem0+(40*TDISPL_Y)+TDISPL_X+8
                    min_end_lo = vidmem0+(40*TDISPL_Y)+TDISPL_X+9
                    sec_end_hi = vidmem0+(40*TDISPL_Y)+TDISPL_X+11
                    sec_end_lo = vidmem0+(40*TDISPL_Y)+TDISPL_X+12
                    lda min_cnt_hi
                    cmp min_end_hi
                    beq +
                    rts
+                   lda min_cnt_lo
                    cmp min_end_lo
                    beq +
                    rts
+                   lda sec_cnt_hi
                    cmp sec_end_hi
                    beq +
                    rts
+                   lda sec_cnt_lo
                    cmp sec_end_lo
                    beq +
                    rts
+                   lda #1
                    sta tune_end_flag
                    lda #DISABLE
                    sta enable_timer_check
                    rts
; ==============================================================================
!zone PRINT

print_snum:         stx .savex+1
                    tax
                    inx
                    lda .dectab,x
                    jsr lib_hex2screen
                    ldy #0
                    sta ( zp_temp ),y
                    iny
                    txa
                    sta ( zp_temp ),y
                    iny
                    lda #' '
                    sta ( zp_temp ),y
.savex:             ldx #0
                    rts
.dectab:            !byte $00, $01, $02, $03, $04, $05, $06, $07
                    !byte $08, $09, $10, $11, $12, $13, $14, $15

print_stitle:       stx .savex+1
                    tax
                    dex
                    lda sdata_pt_lo,x
                    sta .a0+1
                    lda sdata_pt_hi,x
                    sta .a0+2
                    clc
                    lda .a0+1
                    adc #(8-3)
                    sta .a0+1
                    lda .a0+2
                    adc #0
                    sta .a0+2
                    ldy #3
.a0:                lda $0000,y
                    beq +
                    sta ( zp_temp ),y
                    iny
                    jmp .a0
+                   jsr .savex
                    rts

print_colored_line: pha                         ; color in A
                    lda .d800ytab_lo,x          ; line num in X
                    sta .mod0+1
                    lda .d800ytab_hi,x
                    sta .mod0+2

                    ldx #39
                    pla
.mod0:              sta $0000,x
                    dex
                    bpl .mod0
                    rts
.d800ytab_lo:       !for i, 0, 13 {
                        !byte <$d800+(40*(DISPLAY_START_Y+i))+DISPLAY_START_X
                    }
.d800ytab_hi:       !for i, 0, 13 {
                        !byte >$d800+(40*(DISPLAY_START_Y+i))+DISPLAY_START_X
                    }
; ==============================================================================
!zone WAIT
wait_irq:           lda #0
                    sta irq_ready
-                   lda irq_ready
                    beq -
                    rts
; ==============================================================================
; lib_hex2screen
; ------------+-----------------------------------------------------------------
; depends on: | -
; ------------+-----------------------------------------------------------------
; uses:       | A, X
; ------------+-----------------------------------------------------------------
; preserves:  | Y
; ------------+---+-------------------------------------------------------------
; input:      | A | hexvalue to be converted
; ------------+---+-------------------------------------------------------------
; output:     | A | petscii/screencode high nibble
;             | X | petscii/screencode low nibble
; ------------+---+-------------------------------------------------------------
!zone LIB_HEX2SCREEN

lib_hex2screen:     sta .savea+1
                    and #%00001111
                    tax
                    lda .hextab,x
                    sta .low_nibble+1
.savea              lda #0
                    lsr
                    lsr
                    lsr
                    lsr
                    tax
                    lda .hextab,x           ; high nibble
.low_nibble         ldx #0
                    rts
.hextab:            !scr "0123456789abcdef"
; ==============================================================================
!zone KEYBOARD
keyboard_get:       !if DEBUG=1 { dec $d020 }
                    lda #$36
                    sta $01
                    jsr keyscan
                    jsr getin
                    bne +
                    jmp .key_exit
+                   !if DEBUG=1 { sta vidmem0+3 }
                    cmp #KEY_CRSRUP
                    bne +
                    jmp .mark_up
+                   cmp #KEY_CRSRDOWN
                    bne +
                    jmp .mark_down
+                   cmp #KEY_RETURN
                    bne +
                    jmp .tune_select
+
.key_exit:          lda #$35
                    sta $01
                    !if DEBUG=1 { inc $d020 }
                    rts

.mark_down:         lda marker_pos_lo
                    cmp #<vidmem0+(40*(DISPLAY_START_Y+13))+DISPLAY_START_X
                    bne +
                    lda marker_pos_hi
                    cmp #>vidmem0+(40*(DISPLAY_START_Y+13))+DISPLAY_START_X
                    bne +
                    jmp .key_exit
+                   jsr .mark_update
                    inc marker_song
                    clc
                    lda marker_pos_lo
                    adc #40
                    sta marker_pos_lo
                    lda marker_pos_hi
                    adc #0
                    sta marker_pos_hi
                    jsr .mark_update
                    jmp .key_exit
.mark_up:           lda marker_pos_lo
                    cmp #<vidmem0+(40*(DISPLAY_START_Y+0))+DISPLAY_START_X
                    bne +
                    lda marker_pos_hi
                    cmp #>vidmem0+(40*(DISPLAY_START_Y+0))+DISPLAY_START_X
                    bne +
                    jmp .key_exit
+                   jsr .mark_update
                    dec marker_song
                    sec
                    lda marker_pos_lo
                    sbc #40
                    sta marker_pos_lo
                    lda marker_pos_hi
                    sbc #0
                    sta marker_pos_hi
                    jsr .mark_update
                    jmp .key_exit
.mark_update:       ldy #1
-                   lda ( marker_pos ),y
                    eor #$80
                    sta ( marker_pos ),y
                    dey
                    bpl -
                    rts

.tune_select:       ldx cur_song
                    lda #COLORTEXT
                    jsr print_colored_line
                    ldx marker_song
                    dex
                    stx cur_song
                    lda #1
                    sta new_song
                    jmp .key_exit
; ==============================================================================
!zone SPRITES
                    SPIDERX             = $40
                    SPIDERY             = $30
                    LAZYX               = $0a
                    LAZYY               = $43
                    MYDY                = $37
sprites_set:        lda #sprite_base
                    sta vidmem0+$3f8+7
                    lda #SPIDERCOL
                    sta $d027+7
                    lda #SPIDERX
                    sta $d000+(2*7)
                    lda #SPIDERY
                    sta $d001+(2*7)
                    lda #sprite_base+1
                    sta vidmem0+$3f8+3
                    lda #sprite_base+2
                    sta vidmem0+$3f8+4
                    lda #sprite_base+3
                    sta vidmem0+$3f8+5
                    lda #sprite_base+4
                    sta vidmem0+$3f8+6
                    lda #LAZYCOL
                    !for i, 0, 3 {
                      sta $d027+3+i
                    }
                    !for i, 0, 3 {
                      lda #LAZYX+(i*24)
                      sta $d000+((3+i)*2)
                    }
                    lda #LAZYY
                    !for i, 0, 3 {
                      sta $d001+((3+i)*2)
                    }
                    lda #MYDY
                    sta $d001+(2*2)
                    jsr .spr_anim_myd
                    lda #0
                    sta $d017
                    sta $d01c
                    sta $d01d
                    lda #%11111000
                    sta $d010
                    lda #%11111100
                    sta $d015
                    rts
                    MYDSPEED = $10
.spr_anim_myd:      lda #MYDSPEED
                    beq +
                    dec .spr_anim_myd+1
                    rts
+                   lda #MYDSPEED
                    sta .spr_anim_myd+1
.ct_myd:            ldx #7
                    lda .myd_pt,x
                    sta vidmem0+$3f8+2
                    lda .myd_cols,x
                    sta $d027+2
                    lda .myd_xpos,x
                    sta $d000+(2*2)
                    dex
                    bpl +
                    ldx #7
+                   stx .ct_myd+1
                    rts
.myd_pt:            !byte spr_myd_base+3, spr_myd_base+4, spr_myd_base+2, spr_myd_base+4, spr_myd_base+1, spr_myd_base+4, spr_myd_base, spr_myd_base+4
.myd_cols:          !byte PURPLE, BLACK, PINK, BLACK, GREEN, BLACK, LIGHT_GREEN, BLACK
.myd_xpos:          !byte $9c, $00, $6c, $00, $44, $00, $1c, $00
; ==============================================================================
!zone TABLES
                    ; 01 Be careful what you wish for, Toggle!
                    ; 02 Broken Hearts & Broken Speakers
                    ; 03 Creation Reconciled
                    ; 04 Dudelking Returns
                    ; 05 Fall is on its way
                    ; 06 Final Boss Defeated
                    ; 07 Grillende Zirpe
                    ; 08 Groofie Neu
                    ; 09 Instrument by Accident
                    ; 10 Piepmatz
                    ; 11 Popnudel
                    ; 12 Schadstoff
                    ; 13 Seekrank und Skorbut
                    ; 14 ZeckenSID
songtable:          !byte 06 ; #01
                    !byte 01 ; #02
                    !byte 13 ; #03
                    !byte 11 ; #04
                    !byte 10 ; #05
                    !byte 08 ; #06
                    !byte 02 ; #07
                    !byte 05 ; #08
                    !byte 14 ; #09
                    !byte 09 ; #10
                    !byte 07 ; #11
                    !byte 04 ; #12
                    !byte 12 ; #13
                    !byte 03 ; #14
                    !byte $00

sdata_pt_lo:        !byte <s01_data, <s02_data, <s03_data, <s04_data
                    !byte <s05_data, <s06_data, <s07_data, <s08_data
                    !byte <s09_data, <s10_data, <s11_data, <s12_data
                    !byte <s13_data, <s14_data

sdata_pt_hi:        !byte >s01_data, >s02_data, >s03_data, >s04_data
                    !byte >s05_data, >s06_data, >s07_data, >s08_data
                    !byte >s09_data, >s10_data, >s11_data, >s12_data
                    !byte >s13_data, >s14_data

s01_data:           !byte <s01_end, >s01_end
                    !byte FLAG_END+FLAG_SW
                    !scr "03:56"
                    !scr "Be careful what you wish for, Toggle!"
                    !byte $00
s02_data:           !byte <s02_end, >s02_end
                    !byte FLAG_LOOP+FLAG_SW
                    !scr "02:17"
                    !scr "Broken Hearts & Broken Speakers"
                    !byte $00
s03_data:           !byte <s03_end, >s03_end
                    !byte FLAG_END+FLAG_SW
                    !scr "03:29"
                    !scr "Creation Reconciled"
                    !byte $00
s04_data:           !byte <s04_end, >s04_end
                    !byte FLAG_END+FLAG_SW
                    !scr "03:01"
                    !scr "Dudelking Returns"
                    !byte $00
s05_data:           !byte <s05_end, >s05_end
                    !byte FLAG_END+FLAG_SW
                    !scr "02:38"
                    !scr "Fall is on its way"
                    !byte $00
s06_data:           !byte <s06_end, >s06_end
                    !byte FLAG_LOOP+FLAG_NT
                    !scr "02:13"
                    !scr "Final Boss Defeated"
                    !byte $00
s07_data:           !byte <s07_end, >s07_end
                    !byte FLAG_END+FLAG_NT
                    !scr "03:08"
                    !scr "Grillende Zirpe"
                    !byte $00
s08_data:           !byte <s08_end, >s08_end
                    !byte FLAG_LOOP+FLAG_SW
                    !scr "03:26"
                    !scr "Groofie Neu"
                    !byte $00
s09_data:           !byte <s09_end, >s09_end
                    !byte FLAG_LOOP+FLAG_SW
                    !scr "03:47"
                    !scr "Instrument by Accident"
                    !byte $00
s10_data:           !byte <s10_end, >s10_end
                    !byte FLAG_LOOP+FLAG_SW
                    !scr "02:51"
                    !scr "Piepmatz"
                    !byte $00
s11_data:           !byte <s11_end, >s11_end
                    !byte FLAG_LOOP+FLAG_NT
                    !scr "03:51"
                    !scr "Popnudel"
                    !byte $00
s12_data:           !byte <s12_end, >s12_end
                    !byte FLAG_END+FLAG_SW
                    !scr "01:55"
                    !scr "Schadstoff"
                    !byte $00
s13_data:           !byte <s13_end, >s13_end
                    !byte FLAG_END+FLAG_SW
                    !scr "03:17"
                    !scr "Seekrank und Skorbut!"
                    !byte $00
s14_data:           !byte <s14_end, >s14_end
                    !byte FLAG_END+FLAG_SW
                    !scr "01:34"
                    !scr "ZeckenSID"
                    !byte $00
; ==============================================================================
!zone EXO_DATA
                    *= data_block01
                    !bin "exo/Be_Careful_What_You_Wish_For_Toggle.exo"
s01_end:            !bin "exo/Broken_Hearts_and_Broken_Speakers.exo"
s02_end:            !bin "exo/Creation_Reconciled.exo"
s03_end:            !bin "exo/Dudelking_Returns.exo"
s04_end:            !bin "exo/Fall_is_on_its_way.exo"
s05_end:            !bin "exo/Final_Boss_Defeated.exo"
s06_end:            !bin "exo/Grillende_Zirpe.exo"
s07_end:            !bin "exo/Groofie_Neu.exo"
s08_end:            !bin "exo/Instrument_by_Accident.exo"
s09_end:            !bin "exo/Piepmatz.exo"
s10_end:            !bin "exo/Popnudel.exo"
s11_end:            !bin "exo/Schadstoff.exo"
s12_end:
                    *= data_block02
                    !bin "exo/Seekrank_und_Skorbut!.exo"
s13_end:            !bin "exo/ZeckenSID.exo"
s14_end:

