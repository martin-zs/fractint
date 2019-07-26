;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; calmanp5.asm - pentium floating point version of the calcmand.asm file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This code started from calmanfp.asm as a base.  This provided the code that
; takes care of the overhead that is needed to interface with the Fractint
; engine.  The initial pentium optimizations where provided by Daniele
; Paccaloni back in March of 1995.  For whatever reason, these optimizations
; didn't make it into version 19.6, which was released in May of 1997.  In
; July of 1997, Tim Wegner brought to my attention an article by Agner Fog
; Titled "Pentium Optimizations".  This article can currently be found at:

; http://www.azillionmonkeys.com/qed/p5opt.html

; It's a good article that claims to compare the Mandelbrot FPU code similar
; to what is in Fractint with his (and other's) pentium optimized code.  The
; only similarity I was able to find was they both calculate the Mandelbrot
; set.  Admittedly, the Fractint FPU Mandelbrot code was not optimized for a
; pentium.  So, taking the code segments provided by Agner Fog, Terje Mathisen,
; Thomas Jentzsch, and Damien Jones, I set out to optimize Fractint's FPU
; Mandelbrot code.  Unfortunately, it is not possible to just drop someone
; elses Mandelbrot code into Fractint.  I made good progress, but lost
; interest after several months.

; In April of 1998, Rees Acheson (author of MANDELB), contacted me about
; included his pentium optimized Mandelbrot code in the next release of
; Fractint.  This started a flurry of correspondence resulting in
; faster code in Fractint and faster code in MANDELB.  His code didn't
; drop right in, but his input and feedback are much appreciated.  The
; code in this file is largely due to his efforts.

; July 1998, Jonathan Osuch
;
; Updated 10 Oct 1998 by Chuck Ebbert (CAE) -- 5.17% speed gain on a P133
; Fixed keyboard/periodicity conflict JCO  10 DEC 1999
;

DEFAULT REL

;%use smartalign
%include "xfract_a-64.inc"

; external functions
CEXTERN   keypressed    ;:FAR          ; this routine is in 'general.asm'
CEXTERN   getakey       ;:FAR          ; this routine is in 'general.asm'
CEXTERN   plot_orbit    ;:FAR          ; this routine is in 'fracsubr.c'
CEXTERN   scrub_orbit   ;:FAR          ; this routine is in 'fracsubr.c'

; external data
CEXTERN init       ;:QWORD  8 - DBL each ; declared as type complex
CEXTERN parm       ;:QWORD  8 - DBL each ; declared as type complex
CEXTERN new        ;:QWORD  8 - DBL each ; declared as type complex
CEXTERN maxit      ;:QWORD  8 - long
CEXTERN inside     ;:DWORD  4 - int
CEXTERN outside    ;:DWORD  4 - int
CEXTERN rqlim      ;:QWORD  8 - DBL      ; bailout (I never did figure out
                                ;   what "rqlim" stands for. -Wes)
CEXTERN coloriter      ;:QWORD 8 - long
CEXTERN oldcoloriter      ;:QWORD 8 - long
CEXTERN realcoloriter      ;:QWORD 8 - long
CEXTERN periodicitycheck      ;:DWORD 4 - int
CEXTERN reset_periodicity      ;:DWORD 4 - int
CEXTERN closenuff      ;:QWORD 8 - double
CEXTERN fractype       ;:DWORD 4 - int    ; Mandelbrot or Julia
CEXTERN kbdcount       ;:DWORD 4 - int    ; keyboard counter
CEXTERN dotmode        ;:DWORD 4 - int
CEXTERN show_orbit     ;:DWORD 4 - int   ; "show-orbit" flag
CEXTERN orbit_ptr      ;:DWORD 4 - int   ; "orbit pointer" flag
CEXTERN magnitude      ;:QWORD 8 - DBL   ; when using potential
CEXTERN nextsavedincr  ;:QWORD 8 - long  ; for incrementing AND value
CEXTERN firstsavedand  ;:QWORD 8 - long  ; AND value
CEXTERN bad_outside    ;:DWORD 4 - int   ; old FPU code with bad
                                         ;: real,imag,mult,summ
CEXTERN save_release   ;:DWORD 4 - int
CEXTERN showdot        ;:DWORD 4 - int
CEXTERN orbit_delay    ;:DWORD 4 - int
CEXTERN atan_colors    ;:DWORD 4 - int

JULIAFP  EQU 6                  ; from FRACTYPE.H
MANDELFP EQU 0
KEYPRESSDELAY equ 16383         ; 3FFFh

initx   EQU  init   ; just to make life easier
inity   EQU  init+DBLSZ
parmx   EQU  parm
parmy   EQU  parm+DBLSZ
newx    EQU  new
newy    EQU  new+DBLSZ

section .data
align 16
orbit_real              DQ  0.0
align 16
orbit_imag              DQ  0.0
align 16
round_down_half         DQ  0.5
align 16
inside_color            DQ  0
align 16
periodicity_color       DQ  7
%define savedincr  rdi      ; space, but it doesn't hurt either
;;savedand_p5           DD 0    ;EQU     EDX

align 16
savedx_p5               DT  0.0 ;:TWORD
savedy_p5               DT  0.0 ;:TWORD
;closenuff_p5           DQ  0.0 ;:QWORD

tmp_ten_byte_1   DT 0.0  ;:tbyte
tmp_ten_byte_2   DT 0.0  ;:tbyte
tmp_ten_byte_3   DT 0.0  ;:tbyte
tmp_ten_byte_4   DT 0.0  ;:tbyte
tmp_ten_byte_5   DT 0.0  ;:tbyte

tmp_dword               DD  0
align 16
Control  DW 0   ;:word

;calmanp5_text:
section .text

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This routine is called once per image.
; Put things here that won't change from one pixel to the next.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CGLOBAL calcmandfpasmstart_p5
calcmandfpasmstart_p5:

        sub     rax,rax                  ; needed - inside is 4 bytes
        mov     eax,[inside]
        cmp     eax,0                    ; if (inside color == maxiter)
        jnl     non_neg_inside
        mov     rax,[maxit]              ;   use maxit as inside_color

non_neg_inside:                          ; else
        mov     [inside_color],rax       ;   use inside as inside_color
        cmp     dword [periodicitycheck],0      ; if periodicitycheck < 0
        jnl     non_neg_periodicitycheck
        mov     rax,7                    ;   use color 7 (default white)
non_neg_periodicitycheck:                ; else
        mov     [periodicity_color],rax  ; use inside_color still in ax
        mov     qword [oldcoloriter],0   ; no periodicity checking on 1st pixel
        ret
;;_calcmandfpasmstart_p5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; pentium floating point version of calcmandasm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CGLOBAL calcmandfpasm_p5
ALIGN 16
calcmandfpasm_p5:

; Register usage:  rax: ??????    r8:oldcoloriter
;                  rcx:counter    rdx:savedand_p5
;                  rdi:????       rsi: -1

     FRAME rsi, rdi, rbx

;    {Set up FPU stack for quick access while in the loop}
; initialization stuff
        sub     rax,rax                 ; clear rax
;        cmp     periodicitycheck,ax     ; periodicity checking?
        cmp     dword [periodicitycheck],0     ; periodicity checking?
        je      initoldcolor            ;  no, set oldcolor 0 to disable it
;        cmp     reset_periodicity,ax    ; periodicity reset?
        cmp     dword [reset_periodicity],0    ; periodicity reset?
        je      initparms               ;  no, inherit oldcolor from prior invocation
        mov     rax,[maxit]               ; yup.  reset oldcolor to maxit-250
        sub     rax,255                 ; (avoids slowness at high maxits)

initoldcolor:
        mov     [oldcoloriter],rax   ; reset oldcolor

initparms:
;        fld     qword [closenuff]
;        fstp    qword [closenuff_p5]
        fldz
;        sub     rax,rax                   ; clear rax for below
        mov     dword [orbit_ptr],0    ; clear orbits
        fldz                           ;  0.0, 0.0
        mov     rdx,1                  ; rdx = savedand_p5 = 1
        fstp    tword [savedx_p5]      ; savedx = 0.0
        fstp    tword [savedy_p5]      ; savedy = 0.0
;        mov     savedincr,rdx
        mov     rdi,rdx                ; savedincr is in rdi = 1
;        mov     savedincr,1              ; savedincr = 1
;        mov     rdx,firstsavedand

        mov     rsi,-1
        dec     dword [kbdcount]       ; decrement the keyboard counter
        jns      near nokey            ;  skip keyboard test if still positive
        mov     dword [kbdcount],10    ; stuff in a low kbd count
        cmp     dword [show_orbit],0   ; are we showing orbits?
        jne     quickkbd               ;  yup.  leave it that way.
        cmp     dword [orbit_delay],0  ; are we delaying orbits?
        je      slowkbd                ;  nope.  change it.
        cmp     dword [showdot],0      ; are we showing the current pixel?
        jge     quickkbd               ;  yup.  leave it that way.
;this may need to be adjusted, I'm guessing at the "appropriate" values -Wes
slowkbd:
        mov     dword [kbdcount],5000  ; else, stuff an appropriate count val
        cmp     dword [dotmode],11     ; disk video?
        jne     quickkbd               ;  no, leave as is
        shr     dword [kbdcount],2     ; yes, reduce count

quickkbd:
        sub     rsp, 8                 ; 16 byte align
        call    keypressed      ; has a key been pressed?
        add     rsp, 8
        cmp     ax,0                    ;  ...
        je      nokey                   ; nope.  proceed
        mov     dword [kbdcount],0              ; make sure it goes negative again
        cmp     ax,'o'                  ; orbit toggle hit?
        je      orbitkey                ;  yup.  show orbits
        cmp     ax,'O'                  ; orbit toggle hit?
        jne     keyhit                  ;  nope.  normal key.
orbitkey:
        sub     rsp, 8                 ; 16 byte align
        call    getakey         ; read the key for real and eat it
        add     rsp, 8
        mov     eax,1                   ; reset orbittoggle = 1 - orbittoggle
        sub     eax,[show_orbit]           ;  ...
        mov     [show_orbit],eax           ;  ...
        jmp     short nokey             ; pretend no key was hit
keyhit:
        fninit
        mov     rax,-1                  ; return with -1
        mov     [coloriter],rax         ; set color to -1
;        mov     rdx,rax                  ; put results in ax,dx ; rax is 8 bytes
;        shr     rdx,16                   ; all 1's anyway, don't bother w/shift
      UNFRAME rsi, rdi, rbx
        ret                             ; bail out!
nokey:

        mov     rcx,[maxit]               ; initialize counter
        mov     r8,[oldcoloriter]

        cmp     dword [fractype],JULIAFP        ; julia or mandelbrot set?
        je      near dojulia_p5              ; julia set - go there

; Mandelbrot _p5 initialization of stack
        dec     rcx                     ;  always do one already
                                        ; the fpu stack is shown below
                                        ; st(0) ... st(7)

        fld     qword [initx]           ; Cx
        fld     qword [inity]           ; Cy Cx
        fld     qword [rqlim]           ; b Cy Cx
        fld     st2                     ; Cx b Cy Cx
        fld     qword [parmx]           ; Px Cx b Cy Cx
        faddp   st1,st0                 ; Px+Cx b Cy Cx
        fld     st0                     ; Px+Cx Px+Cx b Cy Cx
        fmul    st0,st0                 ; (Px+Cx)^2 Px+Cx b Cy Cx
        fld     st3                     ; Cy (Px+Cx)^2 Px+Cx b Cy Cx
        fld     qword [parmy]           ; Py Cy (Px+Cx)^2 Px+Cx b Cy Cx
        faddp   st1,st0                 ; Py+Cy (Px+Cx)^2 Px+Cx b Cy Cx
        jmp     bottom_of_dojulia_p5

ALIGN 16
DoKeyCheck:
        push    rax
        push    rcx
        push    r8
        call    keypressed      ; has a key been pressed?
        pop     r8
        pop     rcx
;        cmp     ax,0                   ;  ...
        or      ax,ax
        je      SkipKeyCheck            ; nope.  proceed
        pop     rax
        jmp     keyhit

ALIGN 16
save_new_old_value:
        fld     st2                     ; y y^2 x^2 y x b Cy Cx
        fstp    tword [savedy_p5]       ; y^2 x^2 y x b Cy Cx
        fld     st3                     ; x y^2 x^2 y x b Cy Cx
        fstp    tword [savedx_p5]       ; y^2 x^2 y x b Cy Cx
        dec     savedincr               ; time to lengthen the periodicity?
        jnz     near JustAfterFnstsw    ; if not 0, then skip
;        add     rdx,rdx                ; savedand = (savedand * 2) + 1
;        inc     rdx                    ; for longer periodicity
        lea     rdx,[rdx*2+1]
        mov     savedincr,[nextsavedincr]  ; and restart counter

;        test    cx,KEYPRESSDELAY       ; rcx holds the loop count
;        test    cx,0FFFFh
        test    rcx,rsi                 ; put -1 (all 1's) into rsi above
        jz      DoKeyCheck
        jmp     JustAfterFnstsw

SkipKeyCheck:
        pop     rax
        jmp     JustAfterFnstsw

ALIGN 16
do_check_p5_fast:
;        call    near ptr periodicity_check_p5  ; y x b Cy Cx
; REMEMBER, the rcx counter is counting BACKWARDS from maxit to 0
                                       ; fpu stack is
                                       ; y2 x2 y x b Cy Cx
        fld     tword [savedx_p5]      ; savedx y2 x2 y x ...
        fsub    st0,st4                ; x-savedx y2 x2 y x ...
        fabs                           ; |x-savedx| y2 x2 y x ...
        fcomp   qword [closenuff]      ; y2 x2 y x ...
        push    rax                    ; push AX for later
        fnstsw  ax
        and     ah,41h                 ; if |x-savedx| > closenuff
        jz      near per_check_p5_ret_fast   ; we're done
        fld     tword [savedy_p5]      ; savedy y2 x2 y x ...
        fsub    st0,st3                ; y-savedy y2 x2 y x ...
        fabs                           ; |y-savedy| y2 x2 y x ...
        fcomp   qword [closenuff]      ; y2 x2 y x ...
        fnstsw  ax
        and     ah,41h                 ; if |y-savedy| > closenuff
        jz      near per_check_p5_ret_fast   ; we're done
                                       ; caught a cycle!!!
        pop     rax                    ; undo push
        fcompp                         ; pop off y2 and x2, leaving y x ...
        mov     rax,[maxit]
        mov     qword [oldcoloriter],-1  ; check periodicity immediately next time
        mov     [realcoloriter],rax      ; save unadjusted realcolor as maxit
        mov     rax,[periodicity_color]  ; set color
        jmp     overiteration_p5


dojulia_p5:
                                        ; Julia p5 initialization of stack
                                        ; note that init and parm are "reversed"
        fld     qword [parmx]           ; Cx
        fld     qword [parmy]           ; Cy Cx
        fld     qword [rqlim]           ; b Cy Cx

        fld     qword [initx]           ; x b Cy Cx
        fld     st0                     ; x x b Cy Cx
        fmul    st0,st0                 ; x^2 x b Cy Cx
        fld     qword [inity]           ; y x^2 x b Cy Cx

bottom_of_dojulia_p5:
        fmul    st2,st0                ; y x^2 xy b Cy Cx
        fmul    st0,st0                ; y^2 x^2 xy b Cy Cx

        fsubp   st1,st0                ; x^2-y^2 xy b Cy Cx
        fadd    st0,st4                ; x^2-y^2+Cx xy b Cy Cx
        fxch                           ; xy x^2-y^2+Cx b Cy Cx

        fadd    st0,st0                ; 2xy x^2-y^2+Cx b Cy Cx
        fadd    st0,st3                ; 2xy+Cy x^2-y^2+Cx b Cy Cx

; first iteration complete
; {FPU stack all set, we're ready for the start of the loop}
ALIGN 16
LoopStart:

; {While (Sqr(x) + Sqr(y) < b) and (Count < MaxIterations) do}
;    {square both numbers}
        fld     st1                   ;  {x, y, x, b, Cy, Cx}
        fmul    st0,st0               ;  {x^2, y, x, b, Cy, Cx}
        fld     st1                   ;  {y, x^2, y, x, b, Cy, Cx}
        fmul    st0,st0               ;  {y^2, x^2, y, x, b, Cy, Cx}

;    {add both squares and leave at top of stack ready for the compare}
        fld     st1                   ;  {x^2, y^2, x^2, y, x, b, Cy, Cx}
        fadd    st0,st1               ;  {(y^2)+(x^2), y^2, x^2, y, x, b, Cy, Cx}
;    {Check to see if (x^2)+(y^2) < b and discard (x^2)+(y^2)}
        fcomp   st5                   ;  {y^2, x^2, y, x, b, Cy, Cx}

        cmp     rcx,r8    ; put oldcoloriter in r8 above
        jae     SkipTasks  ; don't check periodicity

        fnstsw  ax         ;Get the pending NPX info into AX

        test    rcx,rdx         ; save on 0, check on anything else
        jnz     near do_check_p5_fast        ;  time to save a new "old" value
        jmp     save_new_old_value
;        jz      save_new_old_value
;        jmp     do_check_p5_fast        ;  time to save a new "old" value
ALIGN 16
per_check_p5_ret_fast:

        pop     rax             ;pop AX to continue with the FCOMP test
        jz      short JustAfterFnstsw ;test that got us here, & pairable
        jmp     short JustAfterFnstsw ;since we have done the FNSTSW,
                                ; Skip over next instruction
align 16
SkipTasks:

        fnstsw ax             ;  {Store the NPX status word in AX, no FWAIT}

JustAfterFnstsw:

;  {FPU stack again has all the required elements for terminating the loop }
;  {Continue with the FCOMP test.}
;  {The following does the same as SAHF; JA @LoopEnd; but in 3 fewer cycles}
        shr     ah,1            ; {Shift right, shifts low bit into carry flag }
        jnc     short overbailout_p5  ; {Jmp if not carry.  Do while waiting for FPU }

;  {Temp = Sqr(x) - Sqr(y) + Cx}  {Temp = Newx}
;    {Subtract y^2 from Cx ...}
        fsubr   st0,st6             ;  {Cx-y^2, x^2, y, x, b, Cy, Cx}

;  CAE changed this around for Pentium, 10 Oct 1998
;  exchange this pending result with y so there's no wait for fsubr to finish
        fxch    st2                   ; {y, x^2, Cx-y^2, x, b, Cy, Cx}

;  now compute x*y while the above fsubr is still running
        fmulp   st3,st0                ; {x^2, Cx-y^2, xy, b, Cy, Cx}

;    {... then add x^2 to Cx-y^2}
        faddp   st1,st0             ; {Newx, xy, b, Cy, Cx}

;    {Place the temp (Newx) in the x slot ready for next time in the
;     loop, while placing xy in ST(0) to use below.}
        fxch    st1                   ;  {xy, Newx, b, Cy, Cx}

; {y = (y * x * 2) + Cy   (Use old x, not temp)}
;    {multiply y * x was already done above so it was removed here -- CAE}


;    {Now multiply x*y by 2 (add ST to ST)}
        fadd    st0,st0                ;  {x*y*2, Newx, b, Cy, Cx}

;  compare was moved down so it would run concurrently with above add -- CAE
        cmp     dword [show_orbit],0            ; is show_orbit clear

;    {Finally, add Cy to x*y*2}
        fadd    st0,st3              ;  {Newy, Newx, b, Cy, Cx}

        jz      no_show_orbit_p5         ; if so then skip
        sub     rsp, 8
        call    show_orbit_xy_p5  ; y x b Cy Cx
        add     rsp, 8
ALIGN 16
no_show_orbit_p5:

        dec     rcx
        jnz     LoopStart
;        jmp     LoopStart    ;  {FPU stack has required elements for next loop}
ALIGN 16
LoopEnd:                                ;  {Newy, Newx, b, Cy, Cx}

; reached maxit, inside
        mov     rax,[maxit]
        mov     qword [oldcoloriter],-1  ; check periodicity immediately next time
        mov     [realcoloriter],rax      ; save unadjusted realcolor
        mov     rax,[inside_color]
        jmp     short overiteration_p5
ALIGN 16
overbailout_p5:

        faddp   st1,st0                       ; x^2+y^2 y x b Cy Cx
        mov     rax,rcx
        fstp    qword [magnitude]               ; y x b Cy Cx
        sub     rax,10                  ; 10 more next time before checking

        jns     no_fix_underflow_p5
; if the number of iterations was within 10 of maxit, then subtracting
; 10 would underflow and cause periodicity checking to start right
; away.  Catching a period doesn't occur as often in the pixels at
; the edge of the set anyway.
        sub     rax,rax                 ; don't check next time
no_fix_underflow_p5:
        mov     [oldcoloriter],rax        ; check when past this - 10 next time
        mov     rax,[maxit]
        sub     rax,rcx                 ; leave 'times through loop' in rax

; zero color fix
        jnz     zero_color_fix_p5
        inc     rax                     ; if (rax == 0 ) rax = 1
zero_color_fix_p5:
        mov     [realcoloriter],rax       ; save unadjusted realcolor
        sub     [kbdcount],eax             ; adjust the keyboard count

        cmp     dword [outside],-1              ; iter ? (most common case)
        je      overiteration_p5
        cmp     dword [outside],-2              ; outside <= -2 ?
        jle     to_special_outside_p5   ; yes, go do special outside options
        sub     rax,rax                 ; clear top half of rax for next
        mov     eax,[outside]              ; use outside color
        jmp     short overiteration_p5

to_special_outside_p5:

        sub     rsp, 8
        call    special_outside_p5
        add     rsp, 8
ALIGN 16
overiteration_p5:

        fstp    qword [newy]                    ; x b Cy Cx
        fstp    qword [newx]                    ; b Cy Cx


;    {Pop 3 used registers from FPU stack, discarding the values.
;       All we care about is RCX, the count.}

        fcompp
        fstp    st0
        mov     [coloriter],rax

        cmp     dword [orbit_ptr],0             ; any orbits to clear?
        je      calcmandfpasm_ret_p5    ; nope.
        sub     rsp, 8
        call    scrub_orbit     ; clear out any old orbits
        add     rsp, 8
        mov     rax,[coloriter]           ; restore color
                                        ; speed not critical here in orbit land

calcmandfpasm_ret_p5:

;        mov     edx,eax       ;     {The low 16 bits already in AX}
;        shr     edx,16        ;     {Shift high 16 bits to low 16 bits position}

      UNFRAME rsi, rdi, rbx

        ret

;;; _calcmandfpasm_p5

ALIGN 16
show_orbit_xy_p5:             ;PROC NEAR USES r8 rcx rdx rsi rdi
; USES is needed because in all likelyhood, plot_orbit surely
; uses these registers.  It's ok to have to push/pop's here in the
; orbits as speed is not crucial when showing orbits.

    FRAME r8, rcx, rdx, rsi, rdi
                                        ; fpu stack is either
                                        ; y x b Cx Cy (p5)
        fld     st1                   ;
                                        ; x y ...
                                        ; and needs to returned as
                                        ; y ...

        fstp    qword [orbit_real]              ; y ...
        fst     qword [orbit_imag]              ; y ...
        mov     edi,-1                   ; color for plot orbit, 4 bytes
;        push    rax                      ;       ...  passed in register
; since the number fpu registers that plot_orbit() preserves is compiler
; dependant, it's best to fstp the entire stack into 10 byte memories
; and fld them back after plot_orbit() returns.
        fstp    tword [tmp_ten_byte_1]          ; store the stack in 80 bit form
        fstp    tword [tmp_ten_byte_2]
        fstp    tword [tmp_ten_byte_3]
        fstp    tword [tmp_ten_byte_4]
        fstp    tword [tmp_ten_byte_5]
;        fwait                           ; just to be safe
;        push    word ptr orbit_imag+6   ; co-ordinates for plot orbit
;        push    dword [orbit_imag+4]   ;       ...
;        push    word ptr orbit_imag+2   ;       ...
        movsd   xmm1, qword [orbit_imag]     ;       ... should be second
;        push    word ptr orbit_real+6   ; co-ordinates for plot orbit
;        push    dword [orbit_real+4]   ;       ...
;        push    word ptr orbit_real+2   ;       ...
        movsd   xmm0, qword [orbit_real]     ;       ... should be first
        sub     rsp, 8
        call    plot_orbit      ; display the orbit
        add     rsp, 8
;        add     sp,5*4                  ; clear out the parameters - not on stack

        fld     tword [tmp_ten_byte_5]
        fld     tword [tmp_ten_byte_4]
        fld     tword [tmp_ten_byte_3]
        fld     tword [tmp_ten_byte_2]
        fld     tword [tmp_ten_byte_1]
;        fwait                           ; just to be safe
    UNFRAME r8, rcx, rdx, rsi, rdi
        ret
;;;   show_orbit_xy_p5   ENDP

ALIGN 16
special_outside_p5:       ; PROC NEAR
; When type casting floating point variables to integers in C, the decimal
; is truncated.  When using FIST in asm, the value is rounded.  Using
; "FSUB round_down_half" causes the values to be rounded down.
; Boo-Hiss if values are negative, change FPU control word to truncate values.
        fstcw [Control]
        push  word [Control]                       ; Save control word on the stack
        or    word [Control], 0000110000000000b
        fldcw [Control]                       ; Set control to round towards zero

        cmp     dword [outside],-2
        jne     short not_real

        fld     st1                  ; newx
        test    dword [bad_outside],1h
        jz      over_bad_real
        fsub    qword [round_down_half]
        jmp     over_good_real
over_bad_real:
        frndint
over_good_real:
        fistp   qword [tmp_dword]
        add     eax,7
        add     eax,[tmp_dword]
        jmp     check_color
not_real:
        cmp     dword [outside],-3
        jne     short not_imag
        fld     st0            ; newy
        test    dword [bad_outside],1h
        jz      short over_bad_imag
        fsub    qword [round_down_half]
        jmp     short over_good_imag
over_bad_imag:
        frndint
over_good_imag:
        fistp   dword [tmp_dword]
        add     eax,7
        add     eax,[tmp_dword]
        jmp     check_color
not_imag:
        cmp     dword [outside],-4
        jne     short not_mult
        push    ax              ; save current ax value
        fld     st0           ; newy
        ftst                    ; check to see if newy == 0
        fstsw   ax
        sahf
        pop     ax              ; retrieve ax (does not affect flags)
        jne     short non_zero_y
        fcomp   st0           ; pop it off the stack
;        ret                     ; if y==0, return with normal ax
        jmp     special_outside_ret
non_zero_y:
        fdivr   st0,st2         ; newx/newy
        mov     [tmp_dword],eax
        fimul   dword [tmp_dword]   ; (ax,dx)*newx/newy  (Use FIMUL instead of MUL
        test    dword [bad_outside],1h
        jz      short over_bad_mult
        fsub    qword [round_down_half] ; to make it match the C code.)
        jmp     short over_good_mult
over_bad_mult:
        frndint
over_good_mult:
        fistp   dword [tmp_dword]
;        fwait
        mov     eax,[tmp_dword]
        jmp     short check_color
not_mult:
        cmp     dword [outside],-5
        jne     short not_sum
        fld     st1           ; newx
        fadd    st0,st1     ; newx+newy
        test    dword [bad_outside],1h
        jz     short over_bad_summ
        fsub    qword [round_down_half]
        jmp     short over_good_summ
over_bad_summ:
        frndint
over_good_summ:
        fistp   dword [tmp_dword]
;        fwait
        add     eax,[tmp_dword]
        jmp     short check_color
not_sum:
        cmp     dword [outside],-6      ; currently always equal, but put here
        jne     short not_atan        ; for future outside types
        fld     st0           ; newy
        fld     st2           ; newx newy
        fpatan                  ; arctan(y/x)
        fimul   dword [atan_colors] ; atan_colors*atan
        fldpi                    ; pi atan_colors*atan
        fdivp   st1,st0             ; atan_colors*atan/pi
        fabs
        frndint
        fistp   dword [tmp_dword]
;        fwait
        mov     eax,[tmp_dword]

not_atan:
check_color:
        cmp     rax,[maxit]               ; use UNSIGNED comparison
        jbe     short check_release     ; color < 0 || color > maxit
        sub     rax,rax                 ; rax = 0
check_release:
        cmp     dword [save_release],1961
        jb      short special_outside_ret
        cmp     rax,0
        jne     special_outside_ret
        mov     rax,1                   ; rax = 1
special_outside_ret:
        pop   word [Control]
        fldcw [Control]              ; Restore control word
        ret
;;;   special_outside_p5 ENDP

;;;calmanp5_text  ENDS

;END

