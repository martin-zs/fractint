; bignuma.asm

; based on:
; bbignuma.asm - asm routines for bignumbers
; Wesley Loewer's Big Numbers.        (C) 1994-95, Wesley B. Loewer
; based pointer version

; See BIGLIB.TXT for further documentation.

; general programming notes
; single arg procedures, p(r), r = ebx (or esi when required)
; two arg procedures,    p(r,n), r=edi, n=ebx(or esi when required)
; two arg procedures,    p(n1,n2), n1=ebx(or esi when required), n2=edi
; three arg proc,        p(r,n1,n2), r=edi, n1=esi, n2=ebx
; unless otherwise noted, such as full_mult, mult, full_square, square

; This code started from bignuma.asm as a base.
; This code assumes that bnlength is a multiple of 8.

%include "xfract_a.inc"

; external functions

CEXTERN neg_a_bf

;external variables

CEXTERN bnlength      ;:dword 4 - int
CEXTERN rlength       ;:dword 4 - int
CEXTERN bflength      ;:dword 4 - int
CEXTERN intlength     ;:dword 4 - int

section .text

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = 0
; clear_bn   PROC USES di, r:bn_t
; r is passed in           ebp + 8
CGLOBAL clear_bn
clear_bn:

%define r       ebp + 8

        push    ebp                     ; if FRAME not used, do this
        mov     ebp, esp

        mov     ecx, [bnlength]
        mov     edi, dword [r]          ; load pointer in edi

        sub     eax, eax                ; clear eax
        shr     ecx, 2                  ; 1 byte = 1/4 word
        rep     stosd                   ; clear r, dword at a time

        mov     eax, dword [r]          ; return r in eax

        mov     esp, ebp                ; if UNFRAME not used, do this
        pop     ebp

        ret

;; clear_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = max positive value
; max_bn   PROC USES di, r:bn_t
; r is passed in         ebp + 8
CGLOBAL max_bn
max_bn:

%define r       ebp + 8

        push    ebp                     ; if FRAME not used, do this
        mov     ebp, esp

        mov     ecx, [bnlength]
        mov     edi, dword [r]

        mov     eax, 0FFFFFFFFh ; set eax to max value
        shr     ecx, 2                  ; 1 byte = 1/4 dword
        rep     stosd                   ; max out r, dword at a time

        ; when the above stos is finished, edi points to the byte past the end
        mov     byte [edi-1], 7Fh       ; turn off the sign bit

        mov     eax, dword [r]          ; return r in eax

        mov     esp, ebp                ; if UNFRAME not used, do this
        pop     ebp

        ret

;; max_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n
; copy_bn   PROC USES di si, r:bn_t, n:bn_t
; r & n passed in            ebp + 8 ebp + 12
CGLOBAL copy_bn
copy_bn:

%define r       ebp + 8
%define n       ebp + 12

        push    ebp                     ; if FRAME not used, do this
        mov     ebp, esp

        mov     ecx, [bnlength]
        mov     edi, dword [r]
        mov     esi, dword [n]

        shr     ecx, 2                  ; 1 byte = 1/4 dword
        rep     movsd                   ; copy dword at a time

        mov     eax, dword [r]          ; return r in eax

        mov     esp, ebp                ; if UNFRAME not used, do this
        pop     ebp

        ret

;; copy_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; n1 != n2 ?
; RETURNS: if n1 == n2 returns 0
;          if n1 > n2 returns a positive (steps left to go when mismatch occured)
;          if n1 < n2 returns a negative (steps left to go when mismatch occured)
; cmp_bn   PROC USES di, n1:bn_t, n2:bn_t
; n1 & n2 passed in      ebp + 8  ebp + 12
CGLOBAL cmp_bn
cmp_bn:

%define n1      ebp + 8
%define n2      ebp + 12

    FRAME ebx
        mov     ecx, [bnlength]
        mov     edx, ecx                ; save bnlength for later comparison
        mov     edi, dword [n2]         ; load n2 pointer in edi
        mov     ebx, dword [n1]         ; load n1 pointer in ebx

        add     ebx, ecx                ; point to end of bignumbers
        add     edi, ecx                ; where the msb is

        shr     ecx, 2                  ; byte = 1/4 dword
.top_loop_32:
        sub     ebx, 4                  ; decrement to previous dword
        sub     edi, 4
        mov     eax, dword [ebx]        ; load n1
        cmp     eax, dword [edi]        ; compare to n2
        jne     .not_match_32           ; don't match
        loop    .top_loop_32
        jmp     .match                  ; ecx is zero
.not_match_32:
        ; now determine which byte of the four did not match
        shl     ecx, 2                  ; convert back to bytes
        mov     ebx, eax
        shr     ebx, 16                 ; shift ebx_high to bx
        cmp     bh, [edi+3]             ; compare to n2
        jne     .bottom                 ; jump if bh doesn't match
        dec     ecx                     ; decrement ecx by 1 to show match
        cmp     bl, [edi+2]             ; compare to n2
        jne     .bottom                 ; jump if bl doesn't match
        dec     ecx                     ; decrement ecx by 1 to show match
        cmp     ah, [edi+1]             ; compare to n2
        jne     .bottom                 ; jump if ah doesn't match
        ; if bh,bl,ah do match, then mismatch was in al
        dec     ecx                     ; decrement ecx by 1 to show match
        cmp     al, [edi]               ; reset the flags for below
;        jmp     .bottom

.bottom:

; flags are still set from last cmp
; if ecx == edx, then most significant part didn't match, use signed comparison
; else the decimals didn't match, use unsigned comparison
        lahf                            ; load results of last cmp
        cmp     ecx, edx                ; did they differ on very first cmp
        jne     .not_first_step         ; no

        sahf                            ; yes
        jg      .n1_bigger              ; signed comparison
        jmp     .n2_bigger

.not_first_step:
        sahf
        ja      .n1_bigger              ; unsigned comparison

.n2_bigger:
        neg     ecx                     ; make it negative
.n1_bigger:                             ; leave it positive
.match:                                 ; leave it zero
        mov     eax, ecx
    UNFRAME ebx
        ret

;; cmp_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r < 0 ?
; returns 1 if negative, 0 if positive or zero
; is_bn_neg   PROC n:bn_t
; n is passed in   ebp + 8
CGLOBAL is_bn_neg
is_bn_neg:

%define n       ebp + 8

    FRAME ebx
        sub     eax, eax                    ; clear upper eax
        mov     ebx, dword [n]              ; load n pointer in ebx

        add     ebx, [bnlength]             ; find sign bit
        mov     al, byte [ebx-1]            ; got it

        and     al, 80h                     ; check the sign bit
        rol     al, 1                       ; rotate sign big to bit 0
        sub     ah, ah                      ; clear upper ax
    UNFRAME ebx
        ret

;; is_bn_neg   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; n != 0 ?
; RETURNS: if n != 0 returns 1
;          else returns 0
; is_bn_not_zero   PROC n:bn_t
; n is passed in        ebp + 8
CGLOBAL is_bn_not_zero
is_bn_not_zero:

%define n       ebp + 8

    FRAME ebx
        mov     ecx, dword [bnlength]
        mov     ebx, dword [n]

        shr     ecx, 2                  ; byte = 1/4 dword
.top_loop_32:
        cmp     dword [ebx], 0          ; compare to n to 0
        jnz     .bottom                 ; not zero
        add     ebx, 4                  ; increment to next dword
;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32
;        jmp     .bottom

.bottom:
        ; if ecx is zero, then n was zero
        mov     eax, ecx
    UNFRAME ebx
        ret

;; is_bn_not_zero   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n1 + n2
; add_bn   PROC USES di si, r:bn_t, n1:bn_t, n2:bn_t
; r, n1, & n2 passed in     ebp + 8 ebp + 12 ebp + 16
CGLOBAL add_bn
add_bn:

%define r       ebp + 8
%define n1      ebp + 12
%define n2      ebp + 16

    FRAME ebx
        mov     ecx, dword [bnlength]
        mov     edi, dword [r]
        mov     esi, dword [n1]
        mov     ebx, dword [n2]

        shr     ecx, 2                  ; byte = 1/4 dword
        clc                             ; clear carry flag
        lahf                            ; save carry flag - uses ah

.top_loop_32:
        sahf                            ; restore carry flag - so we can use cmp below
        mov     eax, dword [esi]        ; n1
        adc     eax, dword [ebx]        ; n1+n2
        mov     dword [edi], eax        ; r = n1+n2

        lahf                            ; save carry flag
        add     edi, 4                  ; increment by double word size
        add     esi, 4
        add     ebx, 4

;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx
        ret

;; add_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r += n
; add_a_bn   PROC USES di, r:bn_t, n:bn_t
; r & n passed in          ebp + 8 ebp + 12
CGLOBAL add_a_bn
add_a_bn:

%define r       ebp + 8
%define n       ebp + 12

    FRAME ebx
        mov     ecx, [bnlength]
        mov     edi, dword [r]
        mov     ebx, dword [n]

        shr     ecx, 2                  ; byte = 1/4 dword
        clc                             ; clear carry flag
        lahf                            ; save carry flag - uses ah

.top_loop_32:
        sahf                            ; restore carry flag - so we can use cmp below
        mov     eax, dword [ebx]        ; n
        adc     dword [edi], eax        ; r += n

        lahf                            ; save carry flag
        add     edi, 4                  ; increment by dword size
        add     ebx, 4

;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx
        ret

;; add_a_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n1 - n2
; sub_bn   PROC USES di si, r:bn_t, n1:bn_t, n2:bn_t
; r, n1, & n2 passed in     ebp + 8 ebp + 12 ebp + 16
CGLOBAL sub_bn
sub_bn:

%define r       ebp + 8
%define n1      ebp + 12
%define n2      ebp + 16

    FRAME ebx
        mov     ecx, [bnlength]
        mov     edi, dword [r]
        mov     esi, dword [n1]
        mov     ebx, dword [n2]

        shr     ecx, 2                  ; byte = 1/4 dword
        clc                             ; clear carry flag
        lahf                            ; save carry flag - uses ah

.top_loop_32:
        sahf                            ; restore carry flag - so we can use cmp below
        mov     eax, dword [esi]        ; n1
        sbb     eax, dword [ebx]        ; n1-n2
        mov     dword [edi], eax        ; r = n1-n2

        lahf                            ; save carry flag
        add     edi, 4                  ; increment by dword size
        add     esi, 4
        add     ebx, 4

;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx
        ret

;; sub_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r -= n
; sub_a_bn   PROC USES di, r:bn_t, n:bn_t
; r & n passed in          ebp + 8 ebp + 12
CGLOBAL sub_a_bn
sub_a_bn:

%define r       ebp + 8
%define n       ebp + 12

    FRAME ebx
        mov     ecx, [bnlength]
        mov     edi, dword [r]
        mov     ebx, dword [n]

        shr     ecx, 2                  ; byte = 1/4 dword
        clc                             ; clear carry flag
        lahf                            ; save carry flag - uses ah

.top_loop_32:
        sahf                            ; restore carry flag - so we can use cmp below
        mov     eax, dword [ebx]        ; n
        sbb     dword [edi], eax        ; r -= n - done with eax, so next okay

        lahf                            ; save carry flag
        add     edi, 4                  ; increment by dword size
        add     ebx, 4

;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx
        ret

;; sub_a_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = -n
; neg_bn   PROC USES di, r:bn_t, n:bn_t
; r & n passed in        ebp + 8 ebp + 12
CGLOBAL neg_bn
neg_bn:

%define r       ebp + 8
%define n       ebp + 12

    FRAME ebx
        mov     ecx, [bnlength]
        mov     edi, dword [r]
        mov     ebx, dword [n]

        shr     ecx, 2                  ; byte = 1/4 dword

.top_loop_32:
        mov     eax, dword [ebx]
        neg     eax
        mov     dword [edi], eax
        jc      short .no_more_carry_32 ; notice the "reverse" logic here

        add     edi, 4                  ; increment by dword size
        add     ebx, 4

;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

        jmp     short .bottom

.no_more_carry_32:
        add     edi, 4                  ; increment by dword size
        add     ebx, 4
;        loop    .top_loop_no_more_carry_32   ; jump down
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_no_more_carry_32   ; jump down

        jmp     short .bottom

.top_loop_no_more_carry_32:
        mov     eax, dword [ebx]
        not     eax
        mov     dword [edi], eax

        add     edi, 4                  ; increment by dword size
        add     ebx, 4

;        loop    .top_loop_no_more_carry_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_no_more_carry_32

.bottom:

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx
        ret

;; neg_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r *= -1
;neg_a_bn   PROC r:bn_t
; r is passed in ebp + 8
CGLOBAL neg_a_bn
neg_a_bn:

%define r       ebp + 8

    FRAME ebx
        mov     ecx, [bnlength]
        mov     ebx, dword [r]

        shr     ecx, 2                   ; byte = 1/4 dword

.top_loop_32:
        neg     dword [ebx]
        jc      short .no_more_carry_32  ; notice the "reverse" logic here

        add     ebx, 4

;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

        jmp     short .bottom

.no_more_carry_32:
        add     ebx, 4
;        loop    .top_loop_no_more_carry_32   ; jump down
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_no_more_carry_32   ; jump down

        jmp     short .bottom

.top_loop_no_more_carry_32:
        not     dword [ebx]

        add     ebx, 4

;        loop    .top_loop_no_more_carry_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_no_more_carry_32

.bottom:

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx
        ret

;; neg_a_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = 2*n
; double_bn   PROC USES di, r:bn_t, n:bn_t
; r & n passed in           ebp + 8 ebp + 12
CGLOBAL double_bn
double_bn:

%define r       ebp + 8
%define n       ebp + 12

    FRAME ebx
        mov     ecx, [bnlength]
        mov     edi, dword [r]
        mov     ebx, dword [n]

        shr     ecx, 2                  ; byte = 1/4 dword
        clc                             ; clear carry flag
        lahf                            ; save carry flag - uses ah

.top_loop_32:
        sahf                            ; restore carry flag - so we can use cmp below
        mov     eax, dword [ebx]
        rcl     eax, 1                  ; rotate with carry left
        mov     dword [edi], eax

        lahf                            ; save carry flag
        add     edi, 4                  ; increment by dword size
        add     ebx, 4

;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx
        ret

;; double_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r *= 2
; double_a_bn   PROC r:bn_t
; r is passed in     ebp + 8
CGLOBAL double_a_bn
double_a_bn:

%define r       ebp + 8

    FRAME ebx
        mov     ecx, [bnlength]
        mov     ebx, dword [r]

        shr     ecx, 2                  ; byte = 1/4 dword
        clc                             ; clear carry flag
        lahf                            ; save carry flag - uses ah

.top_loop_32:
        sahf                            ; restore carry flag - so we can use cmp below
        rcl     dword [ebx], 1          ; rotate with carry left

        lahf                            ; save carry flag
        add     ebx, 4                  ; increment by dword size

;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx
        ret

;; double_a_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n/2
; half_bn   PROC USES di, r:bn_t, n:bn_t
; r & n passed in         ebp + 8 ebp + 12
CGLOBAL half_bn
half_bn:

%define r       ebp + 8
%define n       ebp + 12

    FRAME ebx
        mov     ecx, [bnlength]
        mov     edi, dword [r]
        mov     ebx, dword [n]

        add     edi, ecx                ; start with msb
        add     ebx, ecx

        shr     ecx, 2                  ; byte = 1/4 dword

        sub     edi, 4                  ; decrement by double word size
        sub     ebx, 4

        mov     eax, dword [ebx]
        sar     eax, 1                  ; shift arithmetic right
        mov     dword [edi], eax
        lahf                            ; save carry flag

;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

        jmp     short .bottom

.top_loop_32:
        sub     edi, 4                  ; decrement by dword size
        sub     ebx, 4
        sahf                            ; restore carry flag

        mov     eax, dword [ebx]
        rcr     eax, 1                  ; rotate with carry right
        mov     dword [edi], eax
        lahf                            ; save carry flag

;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

.bottom:

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx
        ret

;; half_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r /= 2
; half_a_bn   PROC r:bn_t
; r is passed in   ebp + 8
CGLOBAL half_a_bn
half_a_bn:

%define r       ebp + 8

    FRAME ebx
        mov     ecx, [bnlength]
        mov     ebx, dword [r]

        add     ebx, ecx                ; start with msb

        shr     ecx, 2                  ; byte = 1/4 dword
        sub     ebx, 4                  ; decrement by dword size
        sar     dword [ebx], 1          ; shift arithmetic right
        lahf                            ; save carry flag

;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32
        jmp     short .bottom

.top_loop_32:
        sub     ebx, 4                  ; decrement by dword size
        sahf                            ; restore carry flag

        rcr     dword [ebx], 1          ; rotate with carry right

;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

.bottom:

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx
        ret

;;half_a_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n1 * n2
; Note: r will be a double wide result, 2*bnlength
;       n1 and n2 can be the same pointer
; SIDE-EFFECTS: n1 and n2 are changed to their absolute values
;
; unsafe_full_mult_bn   PROC USES di si, r:bn_t, n1:bn_t, n2:bn_t
; r, n1, & n2 passed in                  ebp + 8 ebp + 12 ebp + 16
CGLOBAL unsafe_full_mult_bn
unsafe_full_mult_bn:

%define r       ebp + 8
%define n1      ebp + 12
%define n2      ebp + 16

;LOCAL sign1:byte, sign2:byte, samevar:byte, \
;      i:dword, j:dword, steps:dword, doublesteps:dword, carry_steps:dword, \
;      n1p: ptr, n2p: ptr

; pushed 12 bytes for saved ebx, esi, & edi, plus 40 for locals

%define sign1       ebp - 16
%define sign2       ebp - 16 + 2
%define samevar     ebp - 20
%define n1p         ebp - 24
%define n2p         ebp - 28
%define i           ebp - 32
%define j           ebp - 36
%define doublesteps ebp - 40
%define carry_steps ebp - 44
%define steps       ebp - 48

    FRAME ebx, esi, edi

        sub     esp, 40                 ; save space for locals

; Test to see if n1 and n2 are the same variable.  It would be better to
; use square_bn(), but it could happen.

        mov     word [samevar], 0       ; assume they are not the same
        mov     ebx, dword [n1]
        cmp     ebx, dword [n2]         ; compare offset
        jne     .end_samevar_check      ; not the same
        mov     word [samevar], 1       ; they are the same
.end_samevar_check:

; By forcing the bignumber to be positive and keeping track of the sign
; bits separately, quite a few multiplies are saved.

                                        ; check for sign bits
        add     ebx, [bnlength]
        mov     al, byte [ebx-1]
        and     al, 80h                 ; check the sign bit
        mov     byte [sign1], al
        jz      .already_pos1
;        invoke  neg_a_bn, n1
        mov     ebx, dword [n1]
        push    ebx
        call    neg_a_bn
        add     esp, 4
.already_pos1:

        cmp     word [samevar], 1       ; if it's the same variable
        je      .already_pos2           ; then skip this second check
        mov     ebx, dword [n2]
        add     ebx, [bnlength]
        mov     al, byte [ebx-1]
        and     al, 80h                 ; check the sign bit
        mov     byte [sign2], al
        jz      .already_pos2
;        invoke  neg_a_bn, n2
        mov     ebx, dword [n2]
        push    ebx
        call    neg_a_bn
        add     esp, 4
.already_pos2:

; in the following loops, the following pointers are used
;   n1p, n2p = points to the part of n1, n2 being used
;   edi = points to part of doublebignumber r used in outer loop
;   esi = points to part of doublebignumber r used in inner loop
;   ebx = points to part of doublebignumber r for carry flag loop

        ; set variables
        mov     edx, [bnlength]         ; set outer loop counter
        shr     edx, 2                  ; byte = 1/4 dword
        mov     [steps], edx            ; save in steps
        mov     [i], edx
        shl     edx, 1                  ; double steps

        ; clear r
        sub     eax, eax                ; clear eax
        mov     ecx, edx                ; size of doublebignumber in dwords
        mov     edi, dword [r]          ; load r in edi for stos
        rep     stosd                   ; initialize r to 0

        sub     edx, 2                  ; only 2*s-2 steps are really needed
        mov     [doublesteps], edx
        mov     [carry_steps], edx

        ; prepare segments and offsets for loops
        mov     edi, dword [r]
        mov     esi, edi                ; both esi and edi are used here
        mov     eax, dword [n1]         ; load pointers
        mov     dword [n1p], eax

.top_outer_loop_32:
        mov     eax, dword [n2]         ; set n2p pointer
        mov     [n2p], eax
        mov     eax, [steps]            ; set inner loop counter
        mov     [j], eax

.top_inner_loop_32:
        mov     ebx, [n1p]
        mov     eax, dword [ebx]
        mov     ebx, [n2p]
        mul     dword [ebx]

        mov     ebx, esi
        add     ebx, 4                  ; increase by size of dword
        add     dword [ebx-4], eax      ; add low dword
        adc     dword [ebx], edx        ; add high dword
        jnc     .no_more_carry_32       ; carry loop not necessary

        mov     ecx, [carry_steps]      ; how many till end of double big number
        jcxz    .no_more_carry_32
        add     ebx, 4                  ; move pointer to next dword

        ; loop until no more carry or until end of double big number
.top_carry_loop_32:
        add     dword [ebx], 1          ; use add, not inc
        jnc     .no_more_carry_32
        add     ebx, 4                  ; increase by size of dword
        loop    .top_carry_loop_32

.no_more_carry_32:
        add     dword [n2p], 4          ; increase by dword size
        add     esi, 4
        sub     dword [carry_steps], 1  ; use one less step
        sub     dword [j], 1
        ja      .top_inner_loop_32

        add     dword [n1p], 4          ; increase by dword size
        add     edi, 4
        mov     esi, edi                ; start with esi=edi

        sub     dword [doublesteps], 1  ; reduce the carry steps needed
        mov     eax, [doublesteps]
        mov     [carry_steps], eax


        sub     dword [i], 1
        ja      .top_outer_loop_32

        ; result is now r, a double wide bignumber

.bottom:

        cmp     word [samevar], 1       ; were the variable the same ones?
        je      .pos_answer             ; if yes, then jump

        mov     al, byte [sign1]        ; is result + or - ?
        cmp     al, byte [sign2]        ; sign(n1) == sign(n2) ?
        je      .pos_answer             ; yes
        shl     dword [bnlength], 1     ; temporarily double bnlength
                                        ; for double wide bignumber
;        invoke  neg_a_bn, r             ; does not affect ES
        mov     ebx, dword [r]
        push    ebx
        call    neg_a_bn
        add     esp, 4
        shr     dword [bnlength], 1     ; restore bnlength
.pos_answer:

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx, esi, edi 
        ret

;; unsafe_full_mult_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n1 * n2 calculating only the top rlength bytes
; Note: r will be of length rlength
;       2*bnlength <= rlength < bnlength
;       n1 and n2 can be the same pointer
; SIDE-EFFECTS: n1 and n2 are changed to their absolute values
;
; unsafe_mult_bn   PROC USES di si, r:bn_t, n1:bn_t, n2:bn_t
; r, n1, & n2 passed in             ebp + 8 ebp + 12 ebp + 16
CGLOBAL unsafe_mult_bn
unsafe_mult_bn:

%define r       ebp + 8
%define n1      ebp + 12
%define n2      ebp + 16

;LOCAL sign1:byte, sign2:byte, samevar:byte, \
;      i:dword, j:dword, steps:dword, doublesteps:dword, \
;      carry_steps:dword, skips:dword, \
;      n1p: ptr, n2p: ptr

; pushed 12 bytes for saved ebx, esi, & edi, plus 40 for locals

%define sign1       ebp - 16
%define sign2       ebp - 16 + 2
%define samevar     ebp - 20
%define n1p         ebp - 24
%define n2p         ebp - 28
%define i           ebp - 32
%define j           ebp - 36
%define doublesteps ebp - 40
%define carry_steps ebp - 44
%define steps       ebp - 48
%define skips       ebp - 52

    FRAME ebx, esi, edi

        sub     esp, 40                 ; save space for locals

; Test to see if n1 and n2 are the same variable.  It would be better to
; use square_bn(), but it could happen.

        mov     word [samevar], 0       ; assume they are not the same
        mov     ebx, dword [n1]
        cmp     ebx, dword [n2]         ; compare offset
        jne     .end_samevar_check      ; not the same
        mov     word [samevar], 1       ; they are the same
.end_samevar_check:

; By forcing the bignumber to be positive and keeping track of the sign
; bits separately, quite a few multiplies are saved.

                                        ; check for sign bits
        add     ebx, [bnlength]
        mov     al, byte [ebx-1]
        and     al, 80h                 ; check the sign bit
        mov     byte [sign1], al
        jz      .already_pos1
;        invoke  neg_a_bn, n1
        mov     ebx, dword [n1]
        push    ebx
        call    neg_a_bn
        add     esp, 4
.already_pos1:

        cmp     word [samevar], 1       ; if it's the same variable
        je      .already_pos2           ; then skip this second check
        mov     ebx, dword [n2]
        add     ebx, [bnlength]
        mov     al, byte [ebx-1]
        and     al, 80h                 ; check the sign bit
        mov     byte [sign2], al
        jz      .already_pos2
;        invoke  neg_a_bn, n2
        mov     ebx, dword [n2]
        push    ebx
        call    neg_a_bn
        add     esp, 4
.already_pos2:

        ; adjust n2 pointer for partial precision
        mov     eax, [bnlength]
        shl     eax, 1                  ; 2*bnlength
        sub     eax, [rlength]          ; 2*bnlength-rlength
        add     dword [n2], eax         ; n2 = n2+2*bnlength-rlength

; in the following loops, the following pointers are used
;   n1p, n2p = points to the part of n1, n2 being used
;   edi = points to part of doublebignumber used in outer loop
;   esi = points to part of doublebignumber used in inner loop
;   ebx = points to part of doublebignumber for carry flag loop

        ; clear r
        sub     eax, eax                ; clear eax
        mov     ecx, [rlength]          ; size of r in bytes
        shr     ecx, 2                  ; byte = 1/4 dword
        mov     edi, dword [r]          ; load r in edi for stos
        rep     stosd                   ; initialize r to 0

        ; set variables
        mov     eax, [rlength]          ; set steps for first loop
        sub     eax, [bnlength]
        shr     eax, 2                  ; byte = 1/4 dword
        mov     [steps], eax            ; save in steps

        mov     eax, [bnlength]
        shr     eax, 2                  ; byte = 1/4 dword
        mov     [i], eax

        sub     eax, [steps]
        mov     [skips], eax            ; how long to skip over pointer shifts

        mov     eax, [rlength]          ; set steps for first loop
        shr     eax, 2                  ; byte = 1/4 dword
        sub     eax, 2                  ; only rlength/4-2 steps are really needed
        mov     [doublesteps], eax
        mov     [carry_steps], eax

        ; prepare segments and offsets for loops
        mov     edi, dword [r]
        mov     esi, edi                ; both si and di are used here
        mov     eax, dword [n1]         ; load pointers
        mov     [n1p], eax


.top_outer_loop_32:
        mov     eax, dword [n2]         ; set n2p pointer
        mov     [n2p], eax
        mov     eax, [steps]            ; set inner loop counter
        mov     [j], eax

.top_inner_loop_32:
        mov     ebx, [n1p]
        mov     eax, dword [ebx]
        mov     ebx, [n2p]
        mul     dword [ebx]

        mov     ebx, esi
        add     ebx, 4                  ; increase by size of dword
        add     dword [ebx-4], eax      ; add low dword
        adc     dword [ebx], edx        ; add high dword
        jnc     .no_more_carry_32       ; carry loop not necessary

        mov     ecx, [carry_steps]      ; how many till end of double big number
        jcxz    .no_more_carry_32
        add     ebx, 4                  ; move pointer to next dword

        ; loop until no more carry or until end of r
.top_carry_loop_32:
        add     dword [ebx], 1          ; use add, not inc
        jnc     .no_more_carry_32
        add     ebx, 4                  ; increase by size of dword
        loop    .top_carry_loop_32

.no_more_carry_32:
        add     dword [n2p], 4          ; increase by dword size
        add     esi, 4
        sub     dword [carry_steps], 1  ; use one less step
        sub     dword [j], 1
        ja      .top_inner_loop_32

        add     dword [n1p], 4          ; increase by dword size

        cmp     dword [skips], 0
        je      .type2_shifts_32
        sub     dword [n2], 4           ; shift n2 back a dword
        add     dword [steps], 1        ; one more step this time
        ; leave edi and doublesteps where they are
        sub     dword [skips], 1        ; keep track of how many times we've done this
        jmp     .shifts_bottom_32
.type2_shifts_32:
        add     edi, 4                  ; shift edi forward a dword
        sub     dword [doublesteps], 1  ; reduce the carry steps needed
.shifts_bottom_32:
        mov     esi, edi                ; start with esi=edi
        mov     eax, [doublesteps]
        mov     [carry_steps], eax

        sub     dword [i], 1
        ja      .top_outer_loop_32

        ; result is in r

bottom:

        cmp     word [samevar], 1       ; were the variable the same ones?
        je      .pos_answer             ; if yes, then jump

        mov     al, byte [sign1]        ; is result + or - ?
        cmp     al, byte [sign2]        ; sign(n1) == sign(n2) ?
        je      .pos_answer             ; yes
        push    dword [bnlength]        ; save bnlength
        mov     eax, [rlength]
        mov     [bnlength], eax         ; set bnlength = rlength
;        invoke  neg_a_bn, r             ; does not affect ES
        mov     ebx, dword [r]
        push    ebx
        call    neg_a_bn
        add     esp, 4
        pop     dword [bnlength]        ; restore bnlength
.pos_answer:

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx, esi, edi
        ret

;;unsafe_mult_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n^2
;   because of the symetry involved, n^2 is much faster than n*n
;   for a bignumber of length l
;      n*n takes l^2 multiplications
;      n^2 takes (l^2+l)/2 multiplications
;          which is about 1/2 n*n as l gets large
;  uses the fact that (a+b+c+...)^2 = (a^2+b^2+c^2+...)+2(ab+ac+bc+...)
;
; Note: r will be a double wide result, 2*bnlength
; SIDE-EFFECTS: n is changed to its absolute value
;
; unsafe_full_square_bn   PROC USES di si, r:bn_t, n:bn_t
; r & n passed in                          ebp + 8 ebp + 12
CGLOBAL unsafe_full_square_bn
unsafe_full_square_bn:

%define r       ebp + 8
%define n      ebp + 12

;LOCAL i:dword, j:dword, steps:dword, doublesteps:dword, carry_steps:dword, \
;      rp1: ptr, rp2: ptr

; pushed 12 bytes for saved ebx, esi, & edi, plus 40 for locals

%define rp1         ebp - 16
%define rp2         ebp - 20
%define i           ebp - 24
%define j           ebp - 28
%define doublesteps ebp - 32
%define carry_steps ebp - 36
%define steps       ebp - 40

    FRAME ebx, esi, edi

        sub     esp, 40                 ; save space for locals

; By forcing the bignumber to be positive and keeping track of the sign
; bits separately, quite a few multiplies are saved.

                                            ; check for sign bit
        mov     ebx, dword [n]
        add     ebx, [bnlength]
        mov     al, byte [ebx-1]
        and     al, 80h                     ; check the sign bit
        jz      .already_pos
;        invoke  neg_a_bn, n
        mov     ebx, dword [n]
        push    ebx
        call    neg_a_bn
        add     esp, 4
.already_pos:

; in the following loops, the following pointers are used
;   n1p(edi), n2p(esi) = points to the parts of n being used
;   rp1 = points to part of doublebignumber used in outer loop
;   rp2 = points to part of doublebignumber used in inner loop
;   ebx = points to part of doublebignumber for carry flag loop

        mov     ecx, dword [bnlength]   ; size of doublebignumber in words

        ; clear r
        sub     eax, eax                ; clear eax
        ; 2{twice the size}*bnlength/4{bytes per word}
        shr     ecx, 1                  ; size of doublebignumber in dwords
        mov     edi, dword [r]          ; load r pointer in edi for stos
        rep     stosd                   ; initialize r to 0

        ; initialize vars
        mov     edx, [bnlength]         ; set outer loop counter
        shr     edx, 2                  ; byte = 1/4 dword
        sub     edx, 1                  ; don't need to do last one
        mov     [i], edx                ; loop counter
        mov     [steps], edx            ; save in steps
        shl     edx, 1                  ; double steps
        sub     edx, 1                  ; only 2*s-1 steps are really needed
        mov     [doublesteps], edx
        mov     [carry_steps], edx

        ; initialize pointers
        mov     edi, dword [n]          ; load n1p pointer
        mov     eax, dword [r]

        add     eax, 4                  ; start with second dword
        mov     [rp1], eax
        mov     [rp2], eax              ; start with rp2=rp1

        cmp     dword [i], 0            ; if bignumberlength is 4
        je      .skip_middle_terms_32

.top_outer_loop_32:
        mov     esi, edi                ; set n2p pointer
        add     esi, 4                  ; to 1 dword beyond n1p(di)
        mov     eax, [steps]            ; set inner loop counter
        mov     [j], eax

.top_inner_loop_32:
        mov     eax, dword [edi]
        mul     dword [esi]

        mov     ebx, [rp2]
        add     ebx, 4                  ; increase by size of dword
        add     dword [ebx-4], eax      ; add low dword
        adc     dword [ebx], edx        ; add high dword
        jnc     .no_more_carry_32       ; carry loop not necessary

        mov     ecx, [carry_steps]      ; how many till end of double big number
        jcxz    .no_more_carry_32
        add     ebx, 4                  ; move pointer to next dword

        ; loop until no more carry or until end of double big number
.top_carry_loop_32:
        add     dword [ebx], 1          ; use add, not inc
        jnc     .no_more_carry_32
        add     ebx, 4                  ; increase by size of dword
        loop    .top_carry_loop_32

.no_more_carry_32:
        add     esi, 4                  ; increase by dword size
        add     dword [rp2], 4
        sub     dword [carry_steps], 1  ; use one less step
        sub     dword [j], 1
        ja      .top_inner_loop_32

        add     edi, 4                  ; increase by dword size
        add     dword [rp1], 8         ; increase by 2*dword size
        mov     eax, [rp1]
        mov     [rp2], eax              ; start with rp2=rp1

        sub     dword [doublesteps], 2  ; reduce the carry steps needed
        mov     eax, [doublesteps]
        mov     [carry_steps], eax

        sub     dword [steps], 1        ; use one less step
        sub     dword [i], 1
        ja      .top_outer_loop_32

        ; All the middle terms have been multiplied.  Now double it.
        shl     dword [bnlength], 1     ; r is a double wide bignumber
;        invoke  double_a_bn, r
        mov     ebx, dword [r]
        push    ebx
        call    double_a_bn
        add     esp, 4
        shr     dword [bnlength], 1     ; restore bnlength

.skip_middle_terms_32:                   ; ds is not necessarily restored here

; Now go back and add in the squared terms.
; In the following loops, the following pointers are used
;   n1p(edi) = points to the parts of n being used
;   rp1(esi) = points to part of doublebignumber used in outer loop
;   ebx = points to part of doublebignumber for carry flag loop

        mov     edi, dword [n]          ; load n1p pointer in edi

        mov     edx, [bnlength]         ; set outer loop counter
        shr     edx, 2                  ; 1 bytes = 1/4 dword
        mov     dword [i], edx          ; loop counter
        shl     edx, 1                  ; double steps

        sub     edx, 2                  ; only 2*s-2 steps are really needed
        mov     [doublesteps], edx
        mov     [carry_steps], edx
        mov     esi, dword [r]          ; set rp1

.top_outer_loop_squares_32:

        mov     eax, dword [edi]
        mul     eax                     ; square it

        mov     ebx, esi
        add     ebx, 4                  ; increase by size of dword
        add     dword [ebx-4], eax      ; add low dword
        adc     dword [ebx], edx        ; add high dword
        jnc     .no_more_carry_squares_32 ; carry loop not necessary

        mov     ecx, dword [carry_steps] ; how many till end of double big number
        jcxz    .no_more_carry_squares_32
        add     ebx, 4                  ; move pointer to next dword

        ; loop until no more carry or until end of double big number
.top_carry_loop_squares_32:
        add     dword [ebx], 1          ; use add, not inc
        jnc     .no_more_carry_squares_32
        add     ebx, 4                  ; increase by size of dword
        loop    .top_carry_loop_squares_32

.no_more_carry_squares_32:
        add     edi, 4                  ; increase by dword size
        add     esi, 8                  ; increase by 2*dword size

        sub     dword [doublesteps], 2  ; reduce the carry steps needed
        mov     eax, [doublesteps]
        mov     [carry_steps], eax

        sub     dword [i], 1
        ja      .top_outer_loop_squares_32

        ; result is in r, a double wide bignumber

; since it is a square, the result has to already be positive

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx, esi, edi
        ret
;;unsafe_full_square_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n^2
;   because of the symetry involved, n^2 is much faster than n*n
;   for a bignumber of length l
;      n*n takes l^2 multiplications
;      n^2 takes (l^2+l)/2 multiplications
;          which is about 1/2 n*n as l gets large
;  uses the fact that (a+b+c+...)^2 = (a^2+b^2+c^2+...)+2(ab+ac+bc+...)
;
; Note: r will be of length rlength
;       2*bnlength >= rlength > bnlength
; SIDE-EFFECTS: n is changed to its absolute value
;
; unsafe_square_bn   PROC USES di si, r:bn_t, n:bn_t
; r & n passed in                     ebp + 8 ebp + 12
CGLOBAL unsafe_square_bn
unsafe_square_bn:

%define r           ebp + 8
%define n           ebp + 12

;LOCAL i:dword, j:dword, steps:dword, doublesteps:dword, carry_steps:dword, \
;      skips:dword, rodd:dword, \
;      n3p: ptr, \
;      rp1: ptr, rp2: ptr

%define rp1         ebp - 16
%define rp2         ebp - 20
%define n3p         ebp - 24
%define i           ebp - 28
%define j           ebp - 32
%define doublesteps ebp - 36
%define carry_steps ebp - 40
%define steps       ebp - 44
%define skips       ebp - 48
%define rodd        ebp - 52

    FRAME ebx, esi, edi

        sub     esp, 40                 ; save space for locals

; This whole procedure would be a great deal simpler if we could assume that
; rlength < 2*bnlength (that is, not =).  Therefore, we will take the
; easy way out and call full_square_bn() if it is.
        mov     eax, [rlength]
        shr     eax, 1                  ; 1/2 * rlength
        cmp     eax, dword [bnlength]   ; 1/2 * rlength == bnlength?
        jne     .not_full_square
;        invoke  unsafe_full_square_bn, r, n
        mov     eax, [n]
        push    eax
        mov     eax, [r]
        push    eax
        call    unsafe_full_square_bn
        add     esp, 8
        ; eax is still loaded with return value
        jmp     .quit_proc               ; we're outa here
.not_full_square:

; By forcing the bignumber to be positive and keeping track of the sign
; bits separately, quite a few multiplies are saved.

                                        ; check for sign bit
        mov     ebx, dword [n]          ; load n1 pointer in ebx
        add     ebx, [bnlength]
        mov     al, byte [ebx-1]
        and     al, 80h                 ; check the sign bit
        jz      .already_pos
;        invoke  neg_a_bn, n
        mov     ebx, dword [n]
        push    ebx
        call    neg_a_bn
        add     esp, 4
.already_pos:

; in the following loops, the following pointers are used
;   n1p(edi), n2p(esi) = points to the parts of n being used
;   rp1 = points to part of doublebignumber used in outer loop
;   rp2 = points to part of doublebignumber used in inner loop
;   ebx = points to part of doublebignumber for carry flag loop

        ; clear r
        sub     eax, eax                ; clear eax
        mov     ecx, [rlength]          ; size of rlength in bytes
        shr     ecx, 2                  ; byte = 1/4 dword
        mov     edi, dword [r]          ; load r pointer in edi for stos
        rep     stosd                   ; initialize r to 0

        ; initialize vars

        ; determine whether r is on an odd or even dword in the number
        ; (even if rlength==2*bnlength, dec r alternates odd/even)
        mov     eax, [bnlength]
        shl     eax, 1                  ; double wide width
        sub     eax, [rlength]          ; 2*bnlength-rlength
        shr     eax, 2                  ; 1 byte = 1/4 dword
        and     eax, 0001h              ; check the odd sign bit
        mov     [rodd], eax

        mov     eax, [bnlength]         ; set outer loop counter
        shr     eax, 2                  ; byte = 1/4 dword
        sub     eax, 1                  ; don't need to do last one
        mov     dword [i], eax          ; loop counter

        mov     eax, [rlength]          ; set steps for first loop
        sub     eax, [bnlength]
        shr     eax, 2                  ; byte = 1/4 dword
        mov     dword [steps], eax      ; save in steps

        mov     edx, [bnlength]
        shr     edx, 2                  ; bnlength/4
        add     eax, edx                ; steps+bnlength/4
        sub     eax, 2                  ; steps+bnlength/4-2
        mov     [doublesteps], eax
        mov     [carry_steps], eax

        mov     eax, [i]
        sub     eax, [steps]
        shr     eax, 1                  ; for both words and dwords
        mov     dword [skips], eax      ; how long to skip over pointer shifts

        ; initialize pointers
        mov     edi, dword [n]          ; load n1p pointer
        mov     esi, edi
        mov     eax, [bnlength]
        shr     eax, 2                  ; 1 byte = 1/4 dword
        sub     eax, [steps]
        shl     eax, 2                  ; 1 byte = 1/4 dword
        add     esi, eax                ; n2p = n1p + bnlength/4 - steps
        mov     dword [n3p], esi        ; save for later use
        mov     eax, dword [r]
        mov     [rp1], eax
        mov     [rp2], eax              ; start with rp2=rp1

        cmp     dword [i], 0            ; if bignumberlength is 8
        je      .skip_middle_terms_32

.top_outer_loop_32:
        mov     eax, [steps]            ; set inner loop counter
        mov     [j], eax

.top_inner_loop_32:
        mov     eax, dword [edi]
        mul     dword [esi]

        mov     ebx, [rp2]
        add     ebx, 4                  ; increase by size of dword
        add     dword[ebx-4], eax       ; add low dword
        adc     dword [ebx], edx        ; add high dword
        jnc     .no_more_carry_32       ; carry loop not necessary

        mov     ecx, [carry_steps]      ; how many till end of double big number
        jcxz    .no_more_carry_32
        add     ebx, 4                  ; move pointer to next dword

        ; loop until no more carry or until end of double big number
.top_carry_loop_32:
        add     dword [ebx], 1          ; use add, not inc
        jnc     .no_more_carry_32
        add     ebx, 4                  ; increase by size of dword
        loop    .top_carry_loop_32

.no_more_carry_32:
        add     esi, 4                  ; increase by dword size
        add     dword [rp2], 4
        sub     dword [carry_steps], 1  ; use one less step
        sub     dword [j], 1
        ja      .top_inner_loop_32

        add     edi, 4                  ; increase by dword size

        mov     eax, [rodd]             ; whether r is on an odd or even dword

        cmp     dword [skips], 0
        jle     .type2_shifts_32
        sub     dword [n3p], 4          ; point to previous dword
        mov     esi, [n3p]
        add     dword [steps], 1        ; one more step this time
        ; leave rp1 and doublesteps where they are
        sub     dword [skips], 1
        jmp     .shifts_bottom_32
.type2_shifts_32:    ; only gets executed once
        jl      .type3_shifts_32
        sub     [steps], eax            ; steps -= (0 or 1)
        add     eax, 1                  ; eax = 1 or 2 now
        sub     [doublesteps], eax      ; decrease double steps by 1 or 2
        shl     eax, 2                  ; 1 byte = 1/4 dword
        add     [rp1], eax              ; add 1 or 2 dwords
        mov     esi, edi
        add     esi, 4                  ; esi = edi + dword
        sub     dword [skips], 1        ; make skips negative
        jmp     .shifts_bottom_32
.type3_shifts_32:
        sub     dword [steps], 1
        sub     dword [doublesteps], 2
        add     dword [rp1], 8          ; + two dwords
        mov     esi, edi
        add     esi, 4                  ; esi = edi + dword
.shifts_bottom_32:

        mov     eax, [rp1]
        mov     [rp2], eax              ; start with rp2=rp1

        mov     eax, [doublesteps]
        mov     [carry_steps], eax

        sub     dword [i], 1
        ja      .top_outer_loop_32

        ; All the middle terms have been multiplied.  Now double it.
        push    dword [bnlength]        ; save bnlength
        mov     eax, [rlength]
        mov     [bnlength], eax         ; r is of length rlength
;        invoke  double_a_bn, r
        mov     ebx, dword [r]
        push    ebx
        call    double_a_bn
        add     esp, 4
        pop     dword [bnlength]

.skip_middle_terms_32:
; Now go back and add in the squared terms.
; In the following loops, the following pointers are used
;   n1p(edi) = points to the parts of n being used
;   rp1(esi) = points to part of doublebignumber used in outer loop
;   ebx = points to part of doublebignumber for carry flag loop

        ; be careful, the next dozen or so lines are confusing!

        ; determine whether r is on an odd or even word in the number
        mov     eax, [bnlength]
        shl     eax, 1                  ; double wide width
        sub     eax, [rlength]          ; 2*bnlength-rlength
        mov     edx, eax                ; save this for a moment
        and     eax, 0004h              ; check the odd sign bit

        mov     esi, dword [r]          ; load r pointer in esi
        add     esi, eax                ; depending on odd or even byte

        shr     edx, 2                  ; assumes dword size
        add     edx, 1
        and     edx, 0FFFEh             ; ~2+1, turn off last bit, mult of 2
        shl     edx, 2
        mov     edi, dword [n]          ; load n1p pointer in edi

        add     edi, edx

        mov     eax, [bnlength]
        sub     eax, edx
        shr     eax, 2                  ; 1 byte = 1/4 dword
        mov     [i], eax

        shl     eax, 1                  ; double steps
        sub     eax, 2                  ; only 2*s-2 steps are really needed
        mov     [doublesteps], eax
        mov     [carry_steps], eax

.top_outer_loop_squares_32:

        mov     eax, dword [edi]
        mul     eax                     ; square it

        mov     ebx, esi
        add     ebx, 4                  ; increase by size of dword
        add     dword [ebx-4], eax      ; add low dword
        adc     dword [ebx], edx        ; add high dword
        jnc     .no_more_carry_squares_32 ; carry loop not necessary

        mov     ecx, [carry_steps]      ; how many till end of double big number
        jcxz    .no_more_carry_squares_32
        add     ebx, 4                  ; move pointer to next dword

        ; loop until no more carry or until end of double big number
.top_carry_loop_squares_32:
        add     dword [ebx], 1          ; use add, not inc
        jnc     .no_more_carry_squares_32
        add     ebx, 4                  ; increase by size of dword
        loop    .top_carry_loop_squares_32

.no_more_carry_squares_32:
        add     edi, 4                  ; increase by dword size
        add     esi, 8                  ; increase by 2*dword size

        sub     dword [doublesteps], 2  ; reduce the carry steps needed
        mov     eax, [doublesteps]
        mov     [carry_steps], eax

        sub     dword [i], 1
        ja      .top_outer_loop_squares_32

        ; result is in r

; since it is a square, the result has to already be positive

        mov     eax, dword [r]          ; return r in eax

.quit_proc:
    UNFRAME ebx, esi, edi
        ret

;;unsafe_square_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n * u  where u is an unsigned integer
; mult_bn_int   PROC USES di si, r:bn_t, n:bn_t, u:dword
; r, n, & u passed in            ebp + 8 ebp + 12 ebp + 16
CGLOBAL mult_bn_int
mult_bn_int:

%define r       ebp + 8
%define n       ebp + 12
%define u       ebp + 16
;;%define lu      r9d

; LOCAL   lu:dword  ; long unsigned integer in 32 bit math

    FRAME ebx
        mov     ecx, [bnlength]
        mov     edi, dword [r]
        mov     esi, dword [n]

        ; no need to clear r

        shr     ecx, 2                  ; byte = 1/4 dword
        sub     ebx, ebx                ; use ebx for temp holding carried dword

        sub     eax, eax                ; clear upper eax
;;        mov     eax, [u]                ; convert u (unsigned int)
;;        mov     lu, eax                 ;   to lu (long unsigned int)

.top_loop_32:
        mov     eax, dword [esi]        ; load next dword from n
        mul     dword [u]                      ; n * u
        add     eax, ebx                ; add last carried upper dword
        adc     edx, 0                  ; inc the carried dword if carry flag set
        mov     ebx, edx                ; save high dword in ebx
        mov     dword [edi], eax        ; save low dword

        add     edi, 4                  ; next dword in r
        add     esi, 4                  ; next dword in n
;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx
        ret

;; mult_bn_int   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r *= u  where u is an unsigned integer
; mult_a_bn_int   PROC USES di si, r:bn_t, u:dword
; r & u passed in                  ebp + 8 ebp + 12
CGLOBAL mult_a_bn_int
mult_a_bn_int:

%define r       ebp + 8
%define u       ebp + 12

    FRAME ebx
        mov     ecx, [bnlength]         ; set outer loop counter
        mov     esi, dword [r]

        ; no need to clear r
        shr     ecx, 2                  ; byte = 1/4 dword
        sub     ebx, ebx                ; use ebx for temp holding carried dword
        sub     edi, edi                ; clear upper edi
        mov     edi, [u]                ; save u in lower edi

.top_loop_32:
        mov     eax, dword [esi]        ; load next dword from r
        mul     edi                     ; r * u
        add     eax, ebx                ; add last carried upper dword
        adc     edx, 0                  ; inc the carried dword if carry flag set
        mov     ebx, edx                ; save high dword in ebx
        mov     dword [esi], eax        ; save low dword

        add     esi, 4                  ; next dword in r
;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

        mov     eax, dword [r]          ; return r in eax
    UNFRAME ebx
        ret

;; mult_a_bn_int   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n / u  where u is an unsigned integer
; unsafe_div_bn_int   PROC USES di si, r:bn_t, n:bn_t, u:dword
; r, n, & u passed in                  ebp + 8 ebp + 12 ebp + 16
CGLOBAL unsafe_div_bn_int
unsafe_div_bn_int:

%define r       ebp + 8
%define n       ebp + 12
%define u       ebp + 16

; LOCAL  sign:byte
%define sign    ebp - 8

    FRAME ebx
        sub     esp, 8                  ; room for local variable "sign"

                                        ; check for sign bits
        mov     ebx, dword [n]
        add     ebx, [bnlength]
        mov     al, byte [ebx-1]
        and     al, 80h                 ; check the sign bit
        mov     byte [sign], al
        jz      .already_pos
;        invoke  neg_a_bn, n
        mov     eax, dword [n]
        push    eax                     ; pass n
        call    neg_a_bn 
        add     esp, 4

.already_pos:

        mov     ecx, [bnlength]         ; set outer loop counter
        mov     edi, dword [r]
        mov     esi, dword [n]          ; load pointers edi, esi
        ; past most significant portion of the number
        add     esi, ecx
        add     edi, ecx

        ; no need to clear r here, values get mov'ed, not add'ed
        shr     ecx, 2                  ; byte = 1/4 dword
        sub     ebx, ebx                ; clear upper word of ebx
        mov     ebx, [u]

        ; need to start with most significant portion of the number
        sub     esi, 4                  ; most sig dword
        sub     edi, 4                  ; most sig dword

        sub     edx, edx                ; clear edx register
                                        ; for first time through loop
.top_loop_32:
        mov     eax, dword [esi]        ; load next dword from n
        div     ebx
        mov     dword [edi], eax        ; store low dword
                                        ; leave remainder in edx

        sub     esi, 4                   ; next dword in n
        sub     edi, 4                   ; next dword in r

;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

        cmp     byte [sign], 0          ; is result + or - ?
        je      .pos_answer             ; yes

;        invoke  neg_a_bn, r
        mov     eax, dword [r]
        push    eax                     ; pass r
        call    neg_a_bn 
        add     esp, 4

.pos_answer:

        mov     eax, dword [r]          ; return r in eax

    UNFRAME ebx

        ret

;;unsafe_div_bn_int   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r /= u  where u is an unsigned integer
; div_a_bn_int   PROC USES si, r:bn_t, u:word
; r & u passed in              ebp + 8 ebp + 12
CGLOBAL div_a_bn_int
div_a_bn_int:

%define r       ebp + 8
%define u       ebp + 12

;LOCAL  sign:byte
%define sign    ebp - 8

    FRAME ebx
        sub     esp, 8                  ; room for local variable "sign"

        mov     ebx, dword [r]
        add     ebx, [bnlength]
        mov     al, byte [ebx-1]
        and     al, 80h                     ; check the sign bit
        mov     byte [sign], al
        jz      .already_pos
;        invoke  neg_a_bn, r
        mov     eax, dword [r]
        push    eax                     ; pass r
        call    neg_a_bn 
        add     esp, 4

.already_pos:

        mov     ecx, [bnlength]         ; set outer loop counter
        mov     esi, dword [r]
        ; past most significant portion of the number
        add     esi, ecx

        ; no need to clear r here, values get mov'ed, not add'ed
        shr     ecx, 2                  ; byte = 1/4 dword
        sub     ebx, ebx                ; clear upper word of ebx
        mov     ebx, [u]

        ; need to start with most significant portion of the number
        sub     esi, 4                  ; most sig dword

        sub     edx, edx                ; clear edx register
                                        ; for first time through loop
.top_loop_32:
        mov     eax, dword [esi]        ; load next dword from r
        div     ebx
        mov     dword [esi], eax        ; store low dword
                                        ; leave remainder in edx

        sub     esi, 4                  ; next dword in r
;        loop    .top_loop_32
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_32

        cmp     byte [sign], 0          ; is result + or - ?
        je      .pos_answer             ; yes
;        invoke  neg_a_bn, r
        mov     eax, dword [r]
        push    eax                     ; pass r
        call    neg_a_bn 
        add     esp, 4

.pos_answer:

        mov     eax, dword [r]          ; return r in eax

    UNFRAME ebx

        ret

;; div_a_bn_int   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; bf_t routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = 0    (just like clear_bn() but loads bflength+2 instead of bnlength)
; clear_bf   PROC USES di, r:bf_t
; r is passed in           ebp + 8
CGLOBAL clear_bf
clear_bf:

%define r       ebp + 8

        push    ebp                     ; if FRAME not used, do this
        mov     ebp, esp

        mov     ecx, [bflength]
        mov     edi, dword [r]

        sub     eax, eax                ; clear eax
        shr     ecx, 2                  ; 1 byte = 1/4 dword
        rep     stosd                   ; clear r, dword at a time
        stosw                           ; plus the exponent

        mov     eax, dword [r]          ; return r in eax

        mov     esp, ebp                ; if UNFRAME not used, do this
        pop     ebp

        ret

;;clear_bf   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n
; copy_bf   PROC USES di si, r:bf_t, n:bf_t
; r & n passed in            ebp + 8 ebp + 12
CGLOBAL copy_bf
copy_bf:

%define r       ebp + 8
%define n       ebp + 12

        push    ebp                     ; if FRAME not used, do this
        mov     ebp, esp

        mov     ecx, [bflength]
        add     ecx, 2
        mov     edi, dword [r]
        mov     esi, dword [n]

        shr     ecx, 2                  ; 1 byte = 1/4 dword
        rep     movsd                   ; copy dword at a time
        movsw                           ; plus the exponent

        mov     eax, dword [r]          ; return r in eax

        mov     esp, ebp                ; if UNFRAME not used, do this
        pop     ebp

        ret

;;copy_bf   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LDBL bftofloat(bf_t n);
; converts a bf number to a 10 byte real
;
; bftofloat   PROC USES di si, n:bf_t
; n is passed in               ebp + 8
CGLOBAL bftofloat
bftofloat:

%define n       ebp + 8

;   LOCAL value[11]:BYTE   ; 11=10+1
; pushed 4 bytes for saved registers
%define value(x)   [ebp - 20 + x]
;                         20 = pushed + allocated local memory

    FRAME ebx
      sub      esp, 16                  ; room for local variable "value"

      mov      ecx, 9                   ; need up to 9 bytes
      cmp      dword [bflength], 10     ; but no more than bflength-1
      jae      .movebytes_set
      mov      ecx, [bflength]          ; bflength is less than 10
      sub      ecx, 1                   ; ecx=movebytes=bflength-1, 1 byte padding
.movebytes_set:

      ; clear value
      sub      eax, eax
      mov      dword value(0), eax      ; clear first 4 bytes
      mov      dword value(4), eax      ; clear next 4 bytes
      mov      word value(8), ax        ; clear next 2 bytes
      mov      byte value(10), al       ; clear last byte

      ; copy bytes from n to value
      lea      edi, value(9)
      sub      edi, ecx                 ; ecx holds movebytes
      mov      ebx, [bflength]
      sub      ebx, 1
      sub      ebx, ecx                 ; ecx holds movebytes
      mov      esi, dword [n]
      add      esi, ebx                 ; n+bflength-1-movebytes
      rep movsb
      mov      bl, byte [esi]           ; save sign byte, esi now points to it
      add      esi, 1                   ; point to exponent
      mov      dx, word [esi]           ; use dx as exponent
      shl      dx, 3                    ; 256^n = 2^(8n)

      ; adjust for negative values
      and      bl, 10000000b            ; determine sign
      jz       .not_neg_32
      neg      dword value(0)           ; take the negative of the 9 byte number
      cmc                               ; toggle carry flag
      not      dword value(4)
      adc      dword value(4), 0
      not      byte value(8)            ; notice this last one is a byte
      adc      byte value(8), 0
.not_neg_32:

      cmp      byte value(8), 0         ; test for 0
      jnz      .top_shift_32
      fldz
      jmp      .return

      ; Shift until most signifcant bit is set.
.top_shift_32:
      test     byte value(8), 10000000b  ; test msb
      jnz      .bottom_shift_32
      sub      dx, 1                    ; decrement exponent
      shl      dword value(0), 1        ; shift left the 9 byte number
      rcl      dword value(4), 1
      rcl      byte value(8), 1         ; notice this last one is byte ptr
      jmp      .top_shift_32
.bottom_shift_32:

      ; round last byte
      cmp      byte value(0), 80h       ;
      jb       .bottom                  ; no rounding necessary
      add      dword value(1), 1
      adc      dword value(5), 0
      jnc      .bottom

      ; to get to here, the pattern was rounded from +FFFF...
      ; to +10000... with the 1 getting moved to the carry bit

.rounded_past_end:

      mov      byte value(8), 10000000b
      add      dx, 1                    ; adjust the exponent

.bottom:
      ; adjust exponent
      add      dx, 3FFFh+7              ; unbiased -> biased, + adjusted
      or       dh, bl                   ; set sign bit if set
      mov      word value(9), dx

      ; unlike float and double, long double is returned on fpu stack
      fld      tword value(1)           ; load return value
.return:
    UNFRAME ebx
      ret

;;bftofloat   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LDBL floattobf(bf_t n, LDBL f);
; converts a 10 byte real to a bf number
;
; floattobf   PROC USES di si, n:bf_t, f:REAL10
; n is passed in               ebp + 8 ebp + 12
; f is passed on the stack, pointed to by flt(x)
CGLOBAL floattobf
floattobf:

%define n       ebp + 8
%define flt(x)    [ebp + 12 + x]
;   LOCAL value[9]:BYTE   ; 9=8+1
%define value(x)   [ebp - 16 + x]
;                         16 = allocated local memory

      push     ebp                      ; if FRAME not used, do this
      mov      ebp, esp
      sub      esp, 16                  ; room for local variable "value"

;      invoke   clear_bf, n
      mov      eax, [n]
      push     eax
      call clear_bf
      add      esp, 4

      ; check to see if f is 0
      cmp      byte flt(7), 0           ; f[7] can only be 0 if f is 0
;      jz       return                  ; if f is 0, bailout now
      jnz      .over_return
      jmp      .return                  ; if f is 0, bailout now
.over_return:

      mov      ecx, 9                   ; need up to 9 bytes
      cmp      dword [bflength], 10     ; but no more than bflength-1
      jae      .movebytes_set
      mov      ecx, [bflength]          ; bflength is less than 10
      sub      ecx, 1                   ; movebytes = bflength-1, 1 byte padding
.movebytes_set:

      ; copy bytes from flt's mantissa to value
      mov      byte value(0), 0         ; clear least sig byte
      mov      eax, dword flt(0)
      mov      dword value(1), eax
      mov      eax, dword flt(4)
      mov      dword value(5), eax

      ; get exponent in dx
      mov      dx, word flt(8)          ; location of exponent
      and      dx, 7FFFh                ; remove sign bit
      sub      dx, 3FFFh+7              ; biased -> unbiased, + adjust

      ; Shift down until exponent is a mult of 8 (2^8n=256n)
.top_shift_32:
      test     dx, 111b                 ; expon mod 8
      jz       .bottom
      add      dx, 1                    ; increment exponent
      shr      dword value(5), 1        ; shift right the 9 byte number
      rcr      dword value(1), 1
      rcr      byte value(0), 1         ; notice this last one is a byte
      jmp      .top_shift_32

.bottom:

      ; Don't bother rounding last byte as it would only make a difference
      ; when bflength < 9, and then only on the last bit.

      ; move data into place, from value to n
      lea      esi, value(9)
      sub      esi, ecx                 ; ecx holds movebytes
      mov      edi, dword [n]
      add      edi, [bflength]
      sub      edi, 1
      sub      edi, ecx                 ; ecx holds movebytes
      rep movsb
      add      edi, 1
      sar      dx, 3                    ; divide expon by 8, 256^n=2^8n
      mov      word [edi], dx           ; store exponent

      ; get sign
      test     byte flt(9), 10000000b   ; test sign bit
      jz       .not_negative
;      invoke   neg_a_bf, n
      mov      eax, [n]
      push     eax
      call neg_a_bf
      add      esp, 4

.not_negative:
.return:
      mov      eax, dword [n]           ; return r in eax
      mov      esp, ebp                 ; if UNFRAME not used, do this
      pop      ebp

      ret

;;floattobf   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LDBL bntofloat(bf_t n);
; converts a bn number to a 10 byte real
; (the most speed critical of these to/from float routines)
;bntofloat   PROC USES di si, n:bn_t
; n is passed in              ebp + 8
CGLOBAL bntofloat
bntofloat:

%define n       ebp + 8

;   LOCAL value[11]:BYTE   ; 11=10+1
%define value(x)   [ebp - 20 + x]
%define value9     ebp - 20 + 9
;                        20 = pushed ebx + allocated local memory

      FRAME ebx
      sub      esp, 16                  ; room for local variable "value"


      ; determine the most significant byte, not 0 or FF
      mov      esi, dword [n]
      sub      esi, 1
      add      esi, [bnlength]          ; n+bnlength-1
      mov      bl, byte [esi]           ; top byte
      mov      ecx, [bnlength]          ; initialize ecx with full bnlength
      cmp      bl, 0                    ; test top byte against 0
      je       .determine_sig_bytes
      cmp      bl, 0FFh                 ; test top byte against -1
      jne      .sig_bytes_determined

.determine_sig_bytes:
      sub      ecx, 1                   ; now bnlength-1
.top_sig_byte:
      sub      esi, 1                   ; previous byte
      cmp      byte [esi], bl           ; does it have the right stuff?
      jne      .sig_bytes_determined    ; (ie: does it match top byte?)
;      loop     top_sig_byte            ; decrement ecx and repeat
      sub      ecx, 1
      cmp      ecx, 0
      jg       .top_sig_byte

; At this point, it must be 0 with no sig figs at all
; or -1/(256^bnlength), one bit away from being zero.
      cmp      bl, 0                    ; was it zero?
      jnz      .not_zero                ; no, it was a very small negative
                                        ; yes
      fldz                              ; return zero
      jmp      .return
.not_zero:
      mov      eax, [intlength]
      sub      eax, [bnlength]
      shl      eax, 3                   ; 256^n=2^8n, now more like movebits
      add      ax, 3FFFh+0              ; bias, no adjustment necessary
      or       ah, 10000000b            ; turn on sign flag
      mov      word value(9), ax        ; store exponent
      mov      word value(7), 8000h     ; store mantissa of 1 in most sig bit
      ; clear rest of value that is actually used
      mov      dword value(1), 0
      mov      word value(5), 0

      fld      tword value(1)
      jmp      .return

.sig_bytes_determined:
      mov      edx, ecx                 ; save in edx for later
      cmp      ecx, 9-1                 ; no more than ecx bytes
      jb       .set_movebytes
      mov      ecx, 9-1                 ; up to 8 bytes
.set_movebytes:                         ; ecx now holds movebytes
                                        ; esi still points to most non-0 sig byte
      sub      esi, ecx                 ; esi now points to first byte to be moved
      add      ecx, 1                   ; can be up to 9

      ; clear value
      mov      dword value(0), 0
      mov      dword value(4), 0
      mov      word value(8),  0
      mov      byte value(10), 0

      ; copy bytes from n to value      ; es:si still holds first move byte of n
      lea      edi, [value9]
      sub      edi, ecx                 ; ecx holds movebytes
      ; value[9] is in edi, first move byte of n is now in esi
      rep movsb

      ; adjust for negative values
      xor      eax, eax                 ; use ax as a flag
      ; get sign flag                   ; top byte is still in bl
      and      bl, 10000000b            ; determine sign
      jz       .not_neg_32
      neg      dword value(0)           ; take the negative of the 9 byte number
      cmc                               ; toggle carry flag
      not      dword value(4)
      adc      dword value(4), 0
      not      byte value(8)            ; notice this last one is a byte
      adc      byte value(8), 0
      jnc      .not_neg_32              ; normal
      mov      byte value(8), 10000000b ;n was FFFF...0000...
      add      eax, 1                   ; set eax to 1 to flag this special case

.not_neg_32:
      sub      edx, [bnlength]          ; adjust exponent
      add      edx, [intlength]         ; adjust exponent
      shl      edx, 3                   ; 256^n=2^8n
      add      edx, eax                 ; see special case above
      ; Shift until most signifcant bit is set.
.top_shift_32:
      test     byte value(8), 10000000b ; test msb
      jnz      .bottom
      sub      edx, 1                   ; decrement exponent
      shl      dword value(0), 1        ; shift left the 9 byte number
      rcl      dword value(4), 1
      rcl      byte value(8), 1         ; notice this last one is a byte
      jmp      .top_shift_32

; don't bother rounding, not really needed while speed is.

.bottom:

      ; adjust exponent
      add      dx, 3FFFh+7-8            ; unbiased -> biased, + adjusted
      or       dh, bl                   ; set sign bit if set
      mov      word value(9), dx

      ; unlike float and double, long double is returned on fpu stack
      fld      tword value(1)           ; load return value
.return:
    UNFRAME ebx

      ret

;;bntofloat   endp

;
; LDBL floattobn(bf_t n, LDBL f) is in BIGNUM.C
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; These last two functions do not use bignum type numbers, but take
; long doubles as arguments.  These routines are called by the C code.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LDBL extract_256(LDBL f, int *exp_ptr)
;
; extracts the mantissa and exponant of f
; finds m and n such that 1<=|m|<256 and f = m*256^n
; n is stored in *exp_ptr and m is returned, sort of like frexp()

; extract_256   PROC f:real10, exp_ptr: ptr sword
; f & exp_ptr passed ebp + 8   ebp + 20
CGLOBAL extract_256
extract_256:

%define f  [ebp + PTRSZ + PTRSZ]
;                8 = return address + saved rbp
%define exp_ptr  [ebp + 20]

; local  expon:sword, exf:real10, tmp_word:word
; location = pushed ebx + room for local variables = 36
%define expon  [ebp - 36]
%define exf    [ebp - 36 + 8]
%define tmp_word [ebp - 36 + 24]

    FRAME ebx
      sub      esp, 32                  ; room for local variables

      fld     tword f                   ; f
      ftst                              ; test for zero
      fstsw   word tmp_word
      fwait
      mov     ax, word tmp_word
      sahf
      jnz     .not_zero                 ; proceed

      mov     ebx, exp_ptr
      mov     dword [ebx], 0            ; save = in *exp_ptr
      jmp     .bottom                   ; f, which is zero, is already on stack

.not_zero:

                                        ; f is already on stack
      fxtract                           ; mant exp, where f=mant*2^exp
      fxch                              ; exp mant
      fistp   word expon                ; mant
      fwait
      mov     ax, expon
      mov     dx, ax                    ; make copy for later use

      cmp     ax, 0
      jge     .pos_exp                  ; jump if exp >= 0
                                        ; exp is neg, adjust exp
      add     ax, 8                     ; exp+8

.pos_exp:
; adjust mantissa
      and     ax, 7                     ; ax mod 8
      jz      .adjust_exponent          ; don't bother with zero adjustments
      mov     word expon, ax            ; use expon as a temp var
      fild    word expon                ; exp mant

      fxch                              ; mant exp
      fscale                            ; mant*2^exp exp
      fstp    st1                       ; mant*2^exp (store in 1 and pop)

.adjust_exponent:
      sar     dx, 3                     ; exp / 8
      mov     ebx, exp_ptr
      mov     word [ebx], dx            ; save in *exp_ptr

      fwait

.bottom:
        ; unlike float and double, long double is returned on fpu stack
    UNFRAME ebx
      ret

;; extract_256   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LDBL scale_256( LDBL f, int n );
; calculates and returns the value of f*256^n
; sort of like ldexp()
;
; n must be in the range -2^12 <= n < 2^12 (2^12=4096),
; which should not be a problem

; scale_256   PROC f:real10, n: sword
; f & n passed     ebp + 8   ebp + 20
CGLOBAL scale_256
scale_256:

%define f [ebp + PTRSZ + PTRSZ]
;                8 = return address + pushed ebp
%define n [ebp + 20]

      cmp     dword n, 0
      jne     .non_zero
      fld     tword f
      jmp     .bottom                   ; don't bother with scales of zero

.non_zero:
      shl     dword n, 3                ; 8n
      fild    dword n                   ; 8n
      fld     tword f                   ; f 8n

      fscale                            ; new_f=f*2^(8n)=f*256^n  8n
      fstp    st1                       ; new_f

.bottom:
        ; unlike float and double, long double is returned on fpu stack
      ret

;;scale_256   ENDP
