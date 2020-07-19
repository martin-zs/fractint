; bignuma.asm

; based on:
; bbignuma.asm - asm routines for bignumbers
; Wesley Loewer's Big Numbers.        (C) 1994-95, Wesley B. Loewer
; based pointer version

; See BIGLIB.TXT for further documentation.

; general programming notes
; single arg procedures, p(r), r = rbx (or rsi when required)
; two arg procedures,    p(r,n), r=rdi, n=rbx(or rsi when required)
; two arg procedures,    p(n1,n2), n1=rbx(or rsi when required), n2=rdi
; three arg proc,        p(r,n1,n2), r=rdi, n1=rsi, n2=rbx
; unless otherwise noted, such as full_mult, mult, full_square, square
; But note that parameters are passed as p(rdi, rsi, rdx)

; This code started from bignuma.asm as a base.
; This code assumes that bnlength is a multiple of 8.

%include "xfract_a-64.inc"

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
; r is passed in rdi
CGLOBAL clear_bn
clear_bn:

        sub     rcx, rcx                ; clear upper rcx
        mov     r8, rdi
        mov     ecx, [bnlength]

        sub     rax, rax                ; clear rax
        shr     ecx, 3                  ; 1 byte = 1/8 qword
        rep     stosq                   ; clear r, qword at a time
        mov     rax, r8                 ; return r in rax
        ret

;; clear_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = max positive value
; max_bn   PROC USES di, r:bn_t
; r is passed in rdi
CGLOBAL max_bn
max_bn:

        sub     rcx, rcx                ; clear upper rcx
        mov     r8, rdi                 ; save for return value
        mov     ecx, [bnlength]

        mov     rax, 0FFFFFFFFFFFFFFFFh ; set rax to max value
        shr     ecx, 3                  ; 1 byte = 1/8 qword
        rep     stosq                   ; max out r, qword at a time

        ; when the above stos is finished, rdi points to the byte past the end
        mov     byte [rdi-1], 7Fh       ; turn off the sign bit

        mov     rax, r8                 ; return r in rax
        ret

;; max_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n
; copy_bn   PROC USES di si, r:bn_t, n:bn_t
; r is passed in rdi
; n is passed in rsi
CGLOBAL copy_bn
copy_bn:

        mov     r8, rdi                 ; save for return value
        mov     ecx, [bnlength]

        shr     ecx, 3                  ; 1 byte = 1/8 qword
        rep     movsq                   ; copy qword at a time

        mov     rax, r8                 ; return r in rax
        ret

;; copy_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; n1 != n2 ?
; RETURNS: if n1 == n2 returns 0
;          if n1 > n2 returns a positive (steps left to go when mismatch occured)
;          if n1 < n2 returns a negative (steps left to go when mismatch occured)
; cmp_bn   PROC USES di, n1:bn_t, n2:bn_t
; n1 is passed in rdi
; n2 is passed in rsi
CGLOBAL cmp_bn
cmp_bn:

     FRAME rbx

        sub     rcx, rcx                ; clear upper rcx
        mov     ecx, [bnlength]
        mov     edx, ecx                ; save bnlength for later comparison

        add     rdi, rcx                ; point to end of bignumbers
        add     rsi, rcx                ; where the msb is

        shr     ecx, 3                  ; byte = 1/8 qword

.top_loop_64:
        sub     rdi, 8                  ; decrement to previous qword
        sub     rsi, 8
        mov     rax, [rdi]              ; load n1
        cmp     rax, [rsi]              ; compare to n2
        jne     .not_match_64           ; don't match
;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64
        jmp     .match                  ; ecx is zero
.not_match_64:
        ; now determine which byte of the eight did not match
        shl     ecx, 3                  ; convert back to bytes
        mov     rbx, rax
        shr     rbx, 24                 ; shift rbx_high to bx (bits 48-64)
        shr     rbx, 24                 ; shift rbx_high to bx
        cmp     bh, [rsi+7]             ; compare to n2
        jne     .bottom                 ; jump if bh doesn't match
        dec     ecx                     ; decrement ecx by 1 to show match
        cmp     bl, [rsi+6]             ; compare to n2
        jne     .bottom                 ; jump if bl doesn't match
        dec     ecx                     ; decrement cx by 1 to show match
        mov     rbx, rax
        shr     rbx, 16                 ; shift rbx_high to bx (bits 32-48)
        shr     rbx, 16                 ; shift rbx_high to bx
        cmp     bh, [rsi+5]             ; compare to n2
        jne     .bottom                 ; jump if bh doesn't match
        dec     ecx                     ; decrement ecx by 1 to show match
        cmp     bl, [rsi+4]             ; compare to n2
        jne     .bottom                 ; jump if bl doesn't match
        dec     ecx                     ; decrement cx by 1 to show match
        mov     rbx, rax
        shr     rbx, 16                 ; shift rbx_high to bx (bits 16-32)
        cmp     bh, [rsi+3]             ; compare to n2
        jne     .bottom                 ; jump if bh doesn't match
        dec     ecx                     ; decrement ecx by 1 to show match
        cmp     bl, [rsi+2]             ; compare to n2
        jne     .bottom                 ; jump if bl doesn't match
        dec     ecx                     ; decrement cx by 1 to show match
        cmp     ah, [rsi+1]             ; compare to n2
        jne     .bottom                 ; jump if ah doesn't match
        ; if bh,bl,ah do match, then mismatch was in al
        dec     ecx                     ; decrement cx by 1 to show match
        cmp     al, [rsi]               ; reset the flags for below
;        jmp     bottom

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
        sub     rax, rax
        mov     eax, ecx

     UNFRAME rbx
        ret

;; cmp_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r < 0 ?
; returns 1 if negative, 0 if positive or zero
;is_bn_neg   PROC n:bn_t
; n is passed in rdi
CGLOBAL is_bn_neg
is_bn_neg:

        sub     rax, rax                ; clear upper rax
        mov     eax, [bnlength]
        add     rdi, rax                ; find sign bit
        sub     rax, rax                ; clear upper rax
        mov     al, [rdi-1]             ; got it

        and     al, 80h                 ; check the sign bit
        rol     al, 1                   ; rotate sign bit to bit 0
;        sub     ah, ah                 ; clear upper ax
        ret

;; is_bn_neg   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; n != 0 ?
; RETURNS: if n != 0 returns 1
;          else returns 0
; is_bn_not_zero   PROC n:bn_t
; n is passed in rdi
CGLOBAL is_bn_not_zero
is_bn_not_zero:

        mov     ecx, [bnlength]
        sub     rax, rax                ; clear top of rax for return

        shr     ecx, 3                  ; byte = 1/8 qword
.top_loop_64:
        cmp     qword [rdi], 0          ; compare to n to 0
        jnz     .bottom                 ; not zero
        add     rdi, 8                  ; increment to next qword

;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64
;        jmp     bottom

.bottom:
        ; if ecx is zero, then n was zero
        mov     eax, ecx
        ret

;; is_bn_not_zero   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n1 + n2
; add_bn   PROC USES di si, r:bn_t, n1:bn_t, n2:bn_t
; r is passed in rdi
; n1 is passed in rsi
; n2 is passed in rdx
CGLOBAL add_bn
add_bn:

        mov     ecx, [bnlength]
        mov     r8, rdi                 ; save for return value

        shr     ecx, 3                  ; byte = 1/8 qword
        clc                             ; clear carry flag
        lahf                            ; save carry flag - uses ah

.top_loop_64:
        sahf                            ; restore carry flag - so we can use cmp below
        mov     rax, [rsi]              ; n1
        adc     rax, [rdx]              ; n1+n2
        mov     [rdi], rax              ; r = n1+n2 - done with rax, so next okay

        lahf                            ; save carry flag
        add     rdi, 8                  ; increment by qword size
        add     rsi, 8
        add     rdx, 8
        
;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64

        mov     rax, r8                 ; return r in rax
        ret

;; add_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r += n
; add_a_bn   PROC USES di, r:bn_t, n:bn_t
; r is passed in rdi
; n is passed in rsi
CGLOBAL add_a_bn
add_a_bn:

        mov     ecx, [bnlength]
        mov     r8, rdi                 ; save for return value

        shr     ecx, 3                  ; byte = 1/8 qword
        clc                             ; clear carry flag
        lahf                            ; save carry flag - uses ah

.top_loop_64:
        sahf                            ; restore carry flag - so we can use cmp below
        mov     rax, [rsi]              ; n
        adc     [rdi], rax              ; r += n - done with rax, so next okay

        lahf                            ; save carry flag
        add     rdi, 8                  ; increment by qword size
        add     rsi, 8

;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64

        mov     rax, r8                 ; return r in rax
        ret

;; add_a_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n1 - n2
; sub_bn   PROC USES di si, r:bn_t, n1:bn_t, n2:bn_t
; r is passed in rdi
; n1 is passed in rsi
; n2 is passed in rdx
CGLOBAL sub_bn
sub_bn:

        mov     ecx, [bnlength]
        mov     r8, rdi                 ; save for return value

        shr     ecx, 3                  ; byte = 1/8 qword
        clc                             ; clear carry flag
        lahf                            ; save carry flag - uses ah

.top_loop_64:
        sahf                            ; restore carry flag - so we can use cmp below
        mov     rax, [rsi]              ; n1
        sbb     rax, [rdx]              ; n1-n2
        mov     [rdi], rax              ; r = n1-n2 - done with rax, so next okay

        lahf                            ; save carry flag
        add     rdi, 8                  ; increment by qword size
        add     rsi, 8
        add     rdx, 8

;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64

        mov     rax, r8                 ; return r in rax
        ret

;; sub_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r -= n
; sub_a_bn   PROC USES di, r:bn_t, n:bn_t
; r is passed in rdi
; n is passed in rsi
CGLOBAL sub_a_bn
sub_a_bn:

        mov     ecx, [bnlength]
        mov     r8, rdi                 ; save for return value

        shr     ecx, 3                  ; byte = 1/8 qword
        clc                             ; clear carry flag
        lahf                            ; save carry flag - uses ah

.top_loop_64:
        sahf                            ; restore carry flag - so we can use cmp below
        mov     rax, [rsi]              ; n
        sbb     [rdi], rax              ; r -= n - done with rax, so next okay

        lahf                            ; save carry flag
        add     rdi, 8                  ; increment by qword size
        add     rsi, 8

;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64

        mov     rax, r8                 ; return r in rax
        ret

;; sub_a_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = -n
; neg_bn   PROC USES di, r:bn_t, n:bn_t
; r is passed in rdi
; n is passed in rsi
CGLOBAL neg_bn
neg_bn:

        mov     ecx, [bnlength]
        mov     r8, rdi                 ; save for return value

        shr     ecx, 3                  ; byte = 1/8 qword

.top_loop_64:
        mov     rax, [rsi]
        neg     rax
        mov     [rdi], rax
        jc      short .no_more_carry_64   ; notice the "reverse" logic here

        add     rdi, 8                  ; increment by qword size
        add     rsi, 8

;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64

        jmp     short .bottom

.no_more_carry_64:
        add     rdi, 8                  ; increment by qword size
        add     rsi, 8
;        loop    top_loop_no_more_carry_64   ; jump down
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_no_more_carry_64   ; jump down

        jmp     short .bottom

.top_loop_no_more_carry_64:
        mov     rax, [rsi]
        not     rax
        mov     [rdi], rax

        add     rdi, 8                  ; increment by qword size
        add     rsi, 8

;        loop    top_loop_no_more_carry_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_no_more_carry_64

.bottom:

        mov     rax, r8                 ; return r in rax
        ret

;; neg_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r *= -1
; neg_a_bn   PROC r:bn_t
; r is passed in rdi
CGLOBAL neg_a_bn
neg_a_bn:

        mov     ecx, [bnlength]
        mov     r8, rdi                 ; save for return value

        shr     ecx, 3                  ; byte = 1/8 qword

.top_loop_64:
        neg     qword [rdi]
        jc      short .no_more_carry_64  ; notice the "reverse" logic here

        add     rdi, 8

;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64

        jmp     short .bottom

.no_more_carry_64:
        add     rdi, 8
;        loop    top_loop_no_more_carry_64   ; jump down
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_no_more_carry_64   ; jump down

        jmp     short .bottom

.top_loop_no_more_carry_64:
        not     qword [rdi]

        add     rdi, 8

;        loop    top_loop_no_more_carry_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_no_more_carry_64

.bottom:

        mov     rax, r8                 ; return r in rax
        ret

;; neg_a_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = 2*n
; double_bn   PROC USES di, r:bn_t, n:bn_t
; r is passed in rdi
; n is passed in rsi
CGLOBAL double_bn
double_bn:

        mov     ecx, [bnlength]
        mov     r8, rdi                 ; save for return value

        shr     ecx, 3                  ; byte = 1/8 qword
        clc                             ; clear carry flag
        lahf                            ; save carry flag - uses ah

.top_loop_64:
        sahf                            ; restore carry flag - so we can use cmp below
        mov     rax, [rsi]
        rcl     rax, 1                  ; rotate with carry left
        mov     [rdi], rax              ; r = 2*n - done with rax, so next okay

        lahf                            ; save carry flag
        add     rdi, 8                  ; increment by qword size
        add     rsi, 8

;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64

        mov     rax, r8                 ; return r in rax
        ret

;;  double_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r *= 2
; double_a_bn   PROC r:bn_t
; r is passed in rdi
CGLOBAL double_a_bn
double_a_bn:

        mov     ecx, [bnlength]
        mov     r8, rdi                 ; save for return value

        shr     ecx, 3                  ; byte = 1/8 qword
        clc                             ; clear carry flag
        lahf                            ; save carry flag - uses ah

.top_loop_64:
        sahf                            ; restore carry flag - so we can use cmp below
        rcl     qword [rdi], 1          ; rotate with carry left

        lahf                            ; save carry flag
        add     rdi, 8                  ; increment by qword size

;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64

        mov     rax, r8                 ; return r in rax
        ret

;; double_a_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n/2
; half_bn   PROC USES di, r:bn_t, n:bn_t
; r is passed in rdi
; n is passed in rsi
CGLOBAL half_bn
half_bn:

        sub     rcx, rcx                ; clear upper rcx
        mov     ecx, [bnlength]
        mov     r8, rdi                 ; save for return value

        add     rdi, rcx                ; start with msb
        add     rsi, rcx

        shr     ecx, 3                  ; byte = 1/8 qword

        sub     rdi, 8                  ; decrement by double word size
        sub     rsi, 8

        mov     rax, [rsi]
        sar     rax, 1                  ; shift arithmetic right
        mov     [rdi], rax
        lahf                            ; save carry flag

;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64
        jmp     short .bottom

.top_loop_64:
        sub     rdi, 8                  ; decrement by qword size
        sub     rsi, 8
        sahf                            ; restore carry flag

        mov     rax, [rsi]
        rcr     rax, 1                  ; rotate with carry right
        mov     [rdi], rax
        lahf                            ; save carry flag

;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64

.bottom:

        mov     rax, r8                 ; return r in rax
        ret

;; half_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r /= 2
; half_a_bn   PROC r:bn_t
; r is passed in rdi
CGLOBAL half_a_bn
half_a_bn:

        sub     rcx, rcx                ; clear upper rcx
        mov     ecx, [bnlength]
        mov     r8, rdi                 ; save for return value

        add     rdi, rcx                ; start with msb

        shr     ecx, 3                  ; byte = 1/8 qword
        sub     rdi, 8                  ; decrement by qword size
        sar     qword [rdi], 1          ; shift arithmetic right
        lahf                            ; save carry flag

;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64
        jmp     short .bottom

.top_loop_64:
        sub     rdi, 8                  ; decrement by qword size
        sahf                            ; restore carry flag

        rcr     qword [rdi], 1          ; rotate with carry right
        lahf                            ; save carry flag

;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64

.bottom:

        mov     rax, r8                 ; return r in rax
        ret

;; half_a_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n1 * n2
; Note: r will be a double wide result, 2*bnlength
;       n1 and n2 can be the same pointer
; SIDE-EFFECTS: n1 and n2 are changed to their absolute values
;
; unsafe_full_mult_bn   PROC USES di si, r:bn_t, n1:bn_t, n2:bn_t
; r is passed in rdi
; n1 is passed in rsi
; n2 is passed in rdx
CGLOBAL unsafe_full_mult_bn
unsafe_full_mult_bn:

;LOCAL sign1:byte, sign2:byte, samevar:word, \
;      i:dword, j:dword, steps:dword, doublesteps:dword, carry_steps:dword, \
;      n1p: ptr , n2p: ptr 

; r           ==> r8
; n1p ==> rsi ==> r9
; n2p ==> rdx ==> r10
%define n1p         r9
%define i           r11d
%define j           r12d
%define doublesteps r13d
%define carry_steps r14d
%define steps       r15d

; pushed 40 bytes for saved registers, keep n2p 16 byte aligned
%define sign1     rbp - 48
%define sign2     rbp - 48 + 2
%define samevar   rbp - 56
;%define n1p       rbp - 64
%define n2p       rbp - 64

     FRAME rbx, r12, r13, r14, r15

       sub     rsp, 24                  ; save space for locals

; Test to see if n1 and n2 are the same variable.  It would be better to
; use square_bn(), but it could happen.

        mov     word [samevar], 0       ; assume they are not the same
        cmp     rsi, rdx                ; compare offset
        jne     .end_samevar_check      ; not the same
        mov     word [samevar], 1       ; they are the same
.end_samevar_check:

; By forcing the bignumber to be positive and keeping track of the sign
; bits separately, quite a few multiplies are saved.

                                        ; check for sign bits
        sub     rax, rax                ; clear uppper rax
        mov     eax, [bnlength]
        mov     rbx, rsi
        add     rbx, rax
        mov     al, byte [rbx-1]
        and     al, 80h                 ; check the sign bit
        mov     byte [sign1], al
        jz      .already_pos1

;        invoke  neg_a_bn, n1
        push    rdi                     ; save r
        mov     rdi, rsi                ; pass n1
        call    neg_a_bn                ; uses rdi & r8
        mov     rsi, rax
        pop     rdi
.already_pos1:

        cmp     word [samevar], 1       ; if it's the same variable
        je      .already_pos2           ; then skip this second check
        sub     rax, rax                ; clear uppper rax
        mov     eax, [bnlength]
        mov     rbx, rdx
        add     rbx, rax
        mov     al, byte [rbx-1]
        and     al, 80h                 ; check the sign bit
        mov     byte [sign2], al
        jz      .already_pos2

;        invoke  neg_a_bn, n2
        push    rdi                     ; save r
        mov     rdi, rdx                ; pass n2
        call    neg_a_bn                ; uses rdi & r8
        mov     rdx, rax
        pop     rdi
.already_pos2:

; in the following loops, the following pointers are used
;   n1p, n2p = points to the part of n1, n2 being used, n1 = n1p = r9
;   rdi = points to part of doublebignumber r used in outer loop
;   rsi = points to part of doublebignumber r used in inner loop
;   rbx = points to part of doublebignumber r for carry flag loop

        mov     r8, rdi                 ; save for return value, r
        mov     n1p, rsi                ; save for later, r9 = n1 = n1p
        mov     r10, rdx                ; save for later, n2
        mov     [n2p], rdx              ; same

        ; set variables
        mov     edx, [bnlength]         ; set outer loop counter
        shr     edx, 3                  ; byte = 1/8 qword
        mov     steps, edx              ; save steps r15d
        mov     i, edx                  ; i = r11d
        shl     edx, 1                  ; double steps

        ; clear r
        sub     rax, rax                ; clear rax
        sub     rcx, rcx                ; clear upper rcx
        mov     ecx, edx                ; size of doublebignumber in qwords
;        mov     rdi, word ptr r         ; load r in rdi for stos
;                                       ; pointer to r still in rdi
        rep     stosq                   ; initialize r to 0

        sub     edx, 2                  ; only 2*s-2 steps are really needed
        mov     doublesteps, edx        ; r13d = doublesteps
        mov     carry_steps, edx        ; r14d = carry_steps

        ; prepare offsets for loops
        mov     rdi, r8                 ; reset rdi
        mov     rsi, rdi                ; both rsi and rdi are used here

.top_outer_loop_64:
        mov     r10, [n2p]              ; reset n2p pointer
        mov     j, steps                ; set inner loop counter - j = steps = r12d

.top_inner_loop_64:
        mov     rax, [n1p]               ; r9 = n1p, don't need pointer to n1 again
;        mov     rbx, [r10]
;        mul     qword [rbx]
        mul     qword [r10]

        mov     rbx, rsi
        add     rbx, 8                  ; increase by size of qword
        add     [rbx-8], rax            ; add low qword
        adc     [rbx], rdx              ; add high qword
        jnc     .no_more_carry_64       ; carry loop not necessary

        mov     ecx, carry_steps        ; how many till end of double big number
        jecxz   .no_more_carry_64
        add     rbx, 8                  ; move pointer to next qword

        ; loop until no more carry or until end of double big number
.top_carry_loop_64:
        add     qword [rbx], 1          ; use add, not inc
        jnc     .no_more_carry_64
        add     rbx, 8                  ; increase by size of qword
        loop    .top_carry_loop_64
;        sub     ecx, 1
;        cmp     ecx, 0
;        jg      .top_carry_loop_64

.no_more_carry_64:
        add     r10, 8                  ; increase by qword size
        add     rsi, 8
        sub     carry_steps, 1          ; use one less step - carry_steps
        sub     j, 1                    ; j
        ja      .top_inner_loop_64

        add     n1p, 8                  ; increase by qword size n1p = r9
        add     rdi, 8
        mov     rsi, rdi                ; start with rsi=rdi

        sub     doublesteps, 1          ; reduce the carry steps needed - doublesteps
        mov     carry_steps, doublesteps  ; carry_steps

        sub     i, 1                    ; i
        ja      .top_outer_loop_64

        ; result is now r, a double wide bignumber

.bottom:

        cmp     word [samevar], 1       ; were the variable the same ones?
        je      .pos_answer             ; if yes, then jump

        mov     al, byte [sign1]        ; is result + or - ?
        cmp     al, byte [sign2]        ; sign(n1) == sign(n2) ?
        je      .pos_answer             ; yes
        shl     dword [bnlength], 1     ; temporarily double bnlength
;        invoke  neg_a_bn, r             ; 
        mov     rdi, r8                 ; pass r
        sub     rsp, 8                  ; 16 byte align
        call    neg_a_bn                ; uses rdi & r8
        add     rsp, 8                  ; in this case rax = r8
        shr     dword [bnlength], 1     ; restore bnlength
.pos_answer:

        mov     rax, r8                 ; return r in rax
     UNFRAME rbx, r12, r13, r14, r15
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
; r is passed in rdi
; n1 is passed in rsi
; n2 is passed in rdx
CGLOBAL unsafe_mult_bn
unsafe_mult_bn:

; LOCAL sign1:byte, sign2:byte, samevar:word, \
;       i:dword, j:dword, steps:dword, doublesteps:dword, \
;       carry_steps:dword, skips:dword, \
;       n1p: ptr byte, n2p: ptr byte

; r           ==> r8
; n1p ==> rsi ==> r9
; n2p ==> rdx ==> r10
%define n1p         r9
%define i           r11d
%define j           r12d
%define doublesteps r13d
%define carry_steps r14d
%define steps       r15d

; pushed 40 bytes for saved registers, keep n2p 16 byte aligned
%define sign1     rbp - 48
%define sign2     rbp - 48 + 2
%define samevar   rbp - 56
%define skips     rbp - 56 + 4
;%define n1p       rbp - 64    ; r9 = n1p, used once, don't need to save
%define n2p       rbp - 64

     FRAME rbx, r12, r13, r14, r15

       sub     rsp, 24                  ; save space for locals

; Test to see if n1 and n2 are the same variable.  It would be better to
; use square_bn(), but it could happen.

        mov     word [samevar], 0       ; assume they are not the same
        cmp     rsi, rdx                ; compare offset
        jne     .end_samevar_check      ; not the same
        mov     word [samevar], 1       ; they are the same
.end_samevar_check:

; By forcing the bignumber to be positive and keeping track of the sign
; bits separately, quite a few multiplies are saved.

                                        ; check for sign bits
        sub     rax, rax                ; clear uppper rax
        mov     eax, [bnlength]
        mov     rbx, rsi
        add     rbx, rax
        mov     al, byte [rbx-1]
        and     al, 80h                 ; check the sign bit
        mov     byte [sign1], al
        jz      .already_pos1

;        invoke  neg_a_bn, n1
        push    rdi                     ; save r
        mov     rdi, rsi                ; pass n1
        call    neg_a_bn                ; uses rdi & r8
        mov     rsi, rax
        pop     rdi
.already_pos1:

        cmp     word [samevar], 1       ; if it's the same variable
        je      .already_pos2           ; then skip this second check
        sub     rax, rax                ; clear uppper rax
        mov     eax, [bnlength]
        mov     rbx, rdx
        add     rbx, rax
        mov     al, byte [rbx-1]
        and     al, 80h                 ; check the sign bit
        mov     byte [sign2], al
        jz      .already_pos2

;        invoke  neg_a_bn, n2
        push    rdi                     ; save r
        mov     rdi, rdx                ; pass n2
        call    neg_a_bn                ; uses rdi & r8
        mov     rdx, rax
        pop     rdi
.already_pos2:

        ; adjust n2 pointer for partial precision
        xor     rax, rax
        mov     eax, [bnlength]
        shl     eax, 1                  ; 2*bnlength
        sub     eax, [rlength]          ; 2*bnlength-rlength
        add     rdx, rax                ; n2 = n2+2*bnlength-rlength

; in the following loops, the following pointers are used
;   n1p, n2p = points to the part of n1, n2 being used
;   rdi = points to part of doublebignumber used in outer loop
;   rsi = points to part of doublebignumber used in inner loop
;   rbx = points to part of doublebignumber for carry flag loop

        mov     r8, rdi                 ; save for return value, r
        mov     n1p, rsi                ; save for later, r9 = n1 = n1p
        mov     r10, rdx                ; save for later, n2
        mov     [n2p], rdx              ; same

        ; clear r
        sub     rax, rax                ; clear rax
        sub     rcx, rcx                ; clear upper rcx
        mov     ecx, [rlength]          ; size of r in bytes
        shr     ecx, 3                  ; byte = 1/8 qword
;        mov     rdi, word ptr r         ; load r in rdi for stos
;                                       ; pointer to r still in rdi
        rep     stosq                   ; initialize r to 0
        mov     rdi, r8                 ; reset rdi

        ; set variables
        mov     eax, dword [rlength]    ; set steps for first loop
        sub     eax, dword [bnlength]
        shr     eax, 3                  ; byte = 1/8 qword
        mov     steps, eax              ; save in steps = r15d

        mov     eax, [bnlength]
        shr     eax, 3                  ; byte = 1/8 qword
        mov     i, eax                  ; i = r11d

        sub     eax, steps
        mov     [skips], eax            ; how long to skip over pointer shifts

        mov     eax, [rlength]          ; set steps for first loop
        shr     eax, 3                  ; byte = 1/8 qword
        sub     eax, 2                  ; only rlength/8-2 steps are really needed
        mov     doublesteps, eax        ; doublesteps = r13d
        mov     carry_steps, eax        ; carry_steps = r14d

        ; prepare offsets for loops
        mov     rsi, rdi
;        mov     ax, word ptr n1         ; load pointers
;        mov     n1p, ax


.top_outer_loop_64:
        mov     r10, [n2p]              ; set n2p pointer
        mov     j, steps                ; set inner loop counter, j = steps

.top_inner_loop_64:
        mov     rax, [n1p]              ; r9 = n1 = n1p
;        mov     rbx, r10                ; r10 = n2p
;        mul     qword [rbx]
        mul     qword [r10]

        mov     rbx, rsi
        add     rbx, 8                  ; increase by size of qword
        add     [rbx-8], rax            ; add low qword
        adc     [rbx], rdx              ; add high qword
        jnc     .no_more_carry_64       ; carry loop not necessary

        mov     ecx, carry_steps        ; how many till end of double big number, carry_steps
        jecxz   .no_more_carry_64
        add     rbx, 8                  ; move pointer to next qword

        ; loop until no more carry or until end of r
.top_carry_loop_64:
        add     qword [rbx], 1          ; use add, not inc
        jnc     .no_more_carry_64
        add     rbx, 8                  ; increase by size of qword
        loop    .top_carry_loop_64
;        sub     ecx, 1
;        cmp     ecx, 0
;        jg      .top_carry_loop_64

.no_more_carry_64:
        add     r10, 8                  ; increase n2p by qword size
        add     rsi, 8
        sub     carry_steps, 1          ; use one less step r14d = carry_steps
        sub     j, 1                    ; j = r12d
        ja      .top_inner_loop_64

        add     n1p, 8                  ; increase by qword size n1p = r9

        cmp     dword [skips], 0
        je      .type2_shifts_64
        sub     qword [n2p], 8          ; shift n2 back a qword, n2 = r10
        add     steps, 1                ; one more step this time r15d = steps
        ; leave rdi and doublesteps where they are
        sub     dword [skips], 1        ; keep track of how many times we've done this
        jmp     .shifts_bottom_64
.type2_shifts_64:
        add     rdi, 8                  ; shift rdi forward a qword
        sub     doublesteps, 1          ; reduce the carry steps needed, = doublesteps
.shifts_bottom_64:
        mov     rsi, rdi                ; start with rsi=rdi
        mov     carry_steps, doublesteps  ; carry_steps = doublesteps

        sub     i, 1                    ; dec i = r11d
        ja      .top_outer_loop_64

        ; result is in r

.bottom:

        cmp     word [samevar], 1       ; were the variable the same ones?
        je      .pos_answer             ; if yes, then jump

        mov     al, byte [sign1]        ; is result + or - ?
        cmp     al, byte [sign2]        ; sign(n1) == sign(n2) ?
        je      .pos_answer             ; yes
        mov     r9d, [bnlength]         ; save bnlength
        mov     eax, [rlength]
        mov     [bnlength], eax         ; set bnlength = rlength
;        invoke  neg_a_bn, r
        mov     rdi, r8                 ; pass r
        sub     rsp, 8                  ; 16 byte align
        call    neg_a_bn                ; uses rdi & r8
        add     rsp, 8                  ; in this case rax = r8
        mov     [bnlength], r9d         ; restore bnlength
.pos_answer:

        mov     rax, r8                 ; return r in rax
     UNFRAME rbx, r12, r13, r14, r15
        ret

;; unsafe_mult_bn   ENDP

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
; r is passed in rdi
; n is passed in rsi
CGLOBAL unsafe_full_square_bn
unsafe_full_square_bn:

;LOCAL i:dword, j:dword, steps:dword, doublesteps:dword, carry_steps:dword,
;      rp1: ptr byte, rp2: ptr byte

; r           ==> r8
; n   ==> rsi ==> r9
%define rp2         r10
%define i           r11d
%define j           r12d
%define doublesteps r13d
%define carry_steps r14d
%define steps       r15d

; pushed 40 bytes for saved registers, keep rp1 16 byte aligned
%define rp1       rbp - 48
;%define rp2       rbp - 64

     FRAME rbx, r12, r13, r14, r15

        sub     rsp, 8                  ; save space for locals

; By forcing the bignumber to be positive and keeping track of the sign
; bits separately, quite a few multiplies are saved.

                                        ; check for sign bit
        sub     rax, rax                ; clear upper rax
        mov     eax, [bnlength]
        mov     rbx, rsi
        add     rbx, rax
        mov     al, byte [rbx-1]
        and     al, 80h                 ; check the sign bit
        jz      .already_pos

;        invoke  neg_a_bn, n
        push    rdi                     ; save r
        mov     rdi, rsi                ; pass n
        call    neg_a_bn                ; uses rdi & r8
        mov     rsi, rax
        pop     rdi
.already_pos:

; in the following loops, the following pointers are used
;   n1p(rdi), n2p(rsi) = points to the parts of n being used
;   rp1 = points to part of doublebignumber used in outer loop
;   rp2 = points to part of doublebignumber used in inner loop
;   rbx = points to part of doublebignumber for carry flag loop

        mov     r8, rdi                 ; save for return value, r
        mov     r9, rsi                 ; save for later, n

        ; clear r
        sub     rax, rax                ; clear rax
        sub     rcx, rcx                ; clear upper rcx
        ; 2{twice the size}*bnlength/8{bytes per qword}
        mov     ecx, [bnlength]         ; (bnlength / 8) * 2
        shr     ecx, 2                  ; size of doublebignumber in qwords
;        mov     rdi, word ptr r         ; load r in rdi for stos
;                                       ; pointer to r still in rdi
        rep     stosq                   ; initialize r to 0

        ; initialize vars
        mov     edx, [bnlength]         ; set outer loop counter
        shr     edx, 3                  ; byte = 1/8 qword
        sub     edx, 1                  ; don't need to do last one
        mov     i, edx                  ; loop counter = r11d
        mov     steps, edx              ; save in steps = r15d
        shl     edx, 1                  ; double steps
        sub     edx, 1                  ; only 2*s-1 steps are really needed
        mov     doublesteps, edx        ; doublesteps = r13d
        mov     carry_steps, edx        ; carry_steps = r14d

        ; initialize pointers
        mov     rdi, r9                 ; load n1p pointer
        mov     rax, r8                 ; load r pointer

        add     rax, 8                  ; start with second qword
        mov     [rp1], rax
        mov     rp2, rax                ; start with r10 = rp2=rp1

        cmp     i, 0                    ; if bignumberlength is 8
        je      .skip_middle_terms_64

.top_outer_loop_64:
        mov     rsi, rdi                ; set n2p pointer
        add     rsi, 8                  ; to 1 qword beyond n1p(rdi)
        mov     j, steps                ; set inner loop counter, j = steps

.top_inner_loop_64:
        mov     rax, [rdi]
        mul     qword [rsi]

        mov     rbx, rp2                ; rp2
        add     rbx, 8                  ; increase by size of qword
        add     [rbx-8], rax            ; add low qword
        adc     [rbx], rdx              ; add high qword
        jnc     .no_more_carry_64       ; carry loop not necessary

        mov     ecx, carry_steps        ; how many till end of double big number, carry_steps
        jecxz   .no_more_carry_64
        add     ebx, 8                  ; move pointer to next qword

        ; loop until no more carry or until end of double big number
.top_carry_loop_64:
        add     qword [rbx], 1          ; use add, not inc
        jnc     .no_more_carry_64
        add     rbx, 8                  ; increase by size of qword
        loop    .top_carry_loop_64
;        sub     ecx, 1
;        cmp     ecx, 0
;        jg      .top_carry_loop_64

.no_more_carry_64:
        add     rsi, 8                  ; increase by qword size
        add     rp2, 8
        sub     carry_steps, 1          ; use one less step
        sub     j, 1                    ; dec     j
        ja      .top_inner_loop_64

        add     rdi, 8                  ; increase by qword size
        add     qword [rp1], 16         ; increase by 2*qword size
        mov     rax, [rp1]
        mov     rp2, rax                ; start with rp2=rp1

        sub     doublesteps, 2          ; reduce the carry steps needed
        mov     carry_steps, doublesteps ; carry_steps = doublesteps

        sub     steps, 1                ; use one less step
        sub     i, 1                    ; i
        ja      .top_outer_loop_64

        ; All the middle terms have been multiplied.  Now double it.
        shl     dword [bnlength], 1     ; r is a double wide bignumber
;        invoke  double_a_bn, r
        mov     rdi, r8                 ; pass r
        sub     rsp, 8                  ; 16 byte align
        call    double_a_bn             ; uses rdi & r8
        add     rsp, 8                  ; in this case rax = r8

        shr     dword [bnlength], 1     ; restore bnlength

.skip_middle_terms_64:                   ; ds is not necessarily restored here

; Now go back and add in the squared terms.
; In the following loops, the following pointers are used
;   n1p(rdi) = points to the parts of n being used
;   rp1(rsi) = points to part of doublebignumber used in outer loop
;   rbx = points to part of doublebignumber for carry flag loop

        mov     rdi, r9                 ; load n1p pointer in rdi

        mov     edx, [bnlength]         ; set outer loop counter
        shr     edx, 3                  ; 1 bytes = 1/8 dword
        mov     i, edx                  ; loop counter
        shl     edx, 1                  ; double steps

        sub     edx, 2                  ; only 2*s-2 steps are really needed
        mov     doublesteps, edx        ; doublesteps
        mov     carry_steps, edx        ; carry_steps
        mov     rsi, r8                 ; set rp1

.top_outer_loop_squares_64:

        mov     rax, [rdi]
        mul     rax                     ; square it

        mov     rbx, rsi
        add     rbx, 8                  ; increase by size of qword
        add     [rbx-8], rax            ; add low qword
        adc     [rbx], rdx              ; add high qword
        jnc     .no_more_carry_squares_64 ; carry loop not necessary

        mov     ecx, carry_steps        ; how many till end of double big number
        jecxz   .no_more_carry_squares_64
        add     rbx, 8                  ; move pointer to next qword

        ; loop until no more carry or until end of double big number
.top_carry_loop_squares_64:
        add     qword [rbx], 1          ; use add, not inc
        jnc     .no_more_carry_squares_64
        add     rbx, 8                  ; increase by size of qword
        loop    .top_carry_loop_squares_64
;        sub     ecx, 1
;        cmp     ecx, 0
;        jg      .top_carry_loop_squares_64

.no_more_carry_squares_64:
        add     rdi, 8                  ; increase by qword size
        add     rsi, 16                 ; increase by 2*qword size

        sub     doublesteps, 2          ; reduce the carry steps needed
        mov     carry_steps, doublesteps

        sub     i, 1                    ; dec     i
        ja      .top_outer_loop_squares_64

        ; result is in r, a double wide bignumber

.bottom:

; since it is a square, the result has to already be positive

        mov     rax, r8                 ; return r in rax
     UNFRAME rbx, r12, r13, r14, r15
        ret

;; unsafe_full_square_bn   ENDP

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
; r is passed in rdi
; n is passed in rsi
CGLOBAL unsafe_square_bn
unsafe_square_bn:

; LOCAL i:dword, j:dword, steps:dword, doublesteps:dword, carry_steps:dword,
;      skips:dword, rodd:dword,
;      n3p: ptr byte, \
;      rp1: ptr byte, rp2: ptr byte

; r   ==> rdi ==> r8
; n   ==> rsi ==> r9
%define rp2         r10
%define i           r11d
%define j           r12d
%define doublesteps r13d
%define carry_steps r14d
%define steps       r15d

; pushed 40 bytes for saved registers, keep n3p and rp1 16 byte aligned
%define skips     rbp - 48
%define rodd      rbp - 48 + 4
%define n3p       rbp - 64
%define rp1       rbp - 80
;%define rp2       rbp - 96

     FRAME rbx, r12, r13, r14, r15

        sub     rsp, 48                 ; save space for locals + 8 so NOT 16 byte aligned

; This whole procedure would be a great deal simpler if we could assume that
; rlength < 2*bnlength (that is, not =).  Therefore, we will take the
; easy way out and call full_square_bn() if it is.
        mov     eax, [rlength]
        shr     eax, 1                  ; 1/2 * rlength
        cmp     eax, dword [bnlength]   ; 1/2 * rlength == bnlength?
        jne     .not_full_square
;        invoke  unsafe_full_square_bn, r, n
        sub     rsp, 8                  ; 16 byte align, r = rdi, n = rsi
        call    unsafe_full_square_bn
        add     rsp, 8
        ; rax is still loaded with return value
        jmp     .quit_proc              ; we're outa here
.not_full_square:

; By forcing the bignumber to be positive and keeping track of the sign
; bits separately, quite a few multiplies are saved.

                                        ; check for sign bit
        sub     rbx, rbx                ; clear upper rbx
        mov     ebx, [bnlength]
        add     rbx, rsi                ; add n pointer to rbx
        mov     al, byte [rbx-1]
        and     al, 80h                 ; check the sign bit
        jz      .already_pos

;        invoke  neg_a_bn, n
        push    rdi                     ; save r, now 16 byte aligned
        mov     rdi, rsi                ; pass n
        call    neg_a_bn                ; uses rdi & r8
        mov     rsi, rax                ; reset rsi
        pop     rdi                     ; restore rdi

.already_pos:

; in the following loops, the following pointers are used
;   n1p(rdi), n2p(rsi) = points to the parts of n being used
;   rp1 = points to part of doublebignumber used in outer loop
;   rp2 = points to part of doublebignumber used in inner loop
;   rbx = points to part of doublebignumber for carry flag loop

        mov     r8, rdi                 ; save for return value, r
        mov     r9, rsi                 ; save for later, n

        ; clear r
        sub     rax, rax                ; clear rax
        sub     rcx, rcx                ; clear upper rcx
        mov     ecx, [rlength]          ; size of rlength in bytes
        shr     ecx, 3                  ; byte = 1/8 qword
;        mov     rdi, word ptr r         ; load r in rdi for stos
;                                       ; pointer to r still in rdi
        rep     stosq                   ; initialize r to 0

        ; initialize vars

        ; determine whether r is on an odd or even qword in the number
        ; (even if rlength==2*bnlength, dec r alternates odd/even)
        mov     eax, [bnlength]
        shl     eax, 1                  ; double wide width
        sub     eax, [rlength]          ; 2*bnlength-rlength
        shr     eax, 3                  ; 1 byte = 1/8 qword
        and     eax, 00000001h          ; check the odd sign bit
        mov     [rodd], eax

        mov     eax, [bnlength]         ; set outer loop counter
        shr     eax, 3                  ; byte = 1/8 qword
        sub     eax, 1                  ; don't need to do last one
        mov     i, eax                  ; loop counter, i = r11d

        mov     eax, [rlength]          ; set steps for first loop
        sub     eax, [bnlength]
        shr     eax, 3                  ; byte = 1/8 qword
        mov     steps, eax              ; save in steps = r15d

        mov     edx, [bnlength]
        shr     edx, 3                  ; bnlength/8
        add     eax, edx                ; steps+bnlength/8
        sub     eax, 2                  ; steps+bnlength/8-2
        mov     doublesteps, eax
        mov     carry_steps, eax

        mov     eax, i                  ; i
        sub     eax, steps              ; i - steps
        shr     eax, 1                  ; for both words and dwords
        mov     [skips], eax            ; how long to skip over pointer shifts

        ; initialize pointers
        sub     rax, rax                ; rax = 0
        mov     rdi, r9                 ; load n1p pointer
        mov     rsi, rdi
        mov     eax, [bnlength]
        shr     eax, 3                  ; 1 byte = 1/8 qword
        sub     eax, steps              ; -steps
        shl     eax, 3                  ; 1 byte = 1/8 qword
        add     rsi, rax                ; n2p = n1p + bnlength/8 - steps
        mov     [n3p], rsi              ; save for later use
        mov     [rp1], r8
        mov     rp2, r8                 ; start with r10 = rp2=rp1

        cmp     i, 0                    ; if bignumberlength is 8
        je      .skip_middle_terms_64

.top_outer_loop_64:
        mov     j, steps                ; set inner loop counter, j = steps

.top_inner_loop_64:
        mov     rax, [rdi]
        mul     qword [rsi]

        mov     rbx, rp2
        add     rbx, 8                  ; increase by size of qword
        add     [rbx-8], rax            ; add low qword
        adc     [rbx], rdx              ; add high qword
        jnc     .no_more_carry_64       ; carry loop not necessary

        mov     ecx, carry_steps        ; how many till end of double big number
        jecxz   .no_more_carry_64
        add     rbx, 8                  ; move pointer to next qword

        ; loop until no more carry or until end of double big number
.top_carry_loop_64:
        add     qword [rbx], 1          ; use add, not inc
        jnc     .no_more_carry_64
        add     rbx, 8                  ; increase by size of qword
        loop    .top_carry_loop_64      ; for simplicity

.no_more_carry_64:
        add     rsi, 8                  ; increase by qword size
        add     rp2, 8
        sub     carry_steps, 1          ; use one less step
        sub     j, 1                    ; dec     j
        ja      .top_inner_loop_64

        add     rdi, 8                  ; increase by qword size

        sub     rax, rax                ; clear all of rax
        mov     eax, [rodd]             ; whether r is on an odd or even qword

        cmp     dword [skips], 0
        jle     .type2_shifts_64
        sub     qword [n3p], 8          ; point to previous qword
        mov     rsi, [n3p]
        add     steps, 1                ; one more step this time
        ; leave rp1 and doublesteps where they are
        sub     dword [skips], 1
        jmp     .shifts_bottom_64
.type2_shifts_64:    ; only gets executed once
        jl      .type3_shifts_64
        sub     steps, eax              ; steps -= (0 or 1)
        add     eax, 1                  ; eax = 1 or 2 now
        sub     doublesteps, eax        ; decrease double steps by 1 or 2
        shl     eax, 3                  ; 1 byte = 1/8 qword
        add     qword [rp1], rax        ; add 1 or 2 qwords
        mov     rsi, rdi
        add     rsi, 8                  ; rsi = rdi + qword
        sub     dword [skips], 1        ; make skips negative
        jmp     .shifts_bottom_64
.type3_shifts_64:
        sub     steps, 1                ; steps - 1
        sub     doublesteps, 2          ; doublesteps - 2
        add     qword [rp1], 16         ; + two qwords
        mov     rsi, rdi
        add     rsi, 8                  ; rsi = rdi + qword
.shifts_bottom_64:

        mov     rp2, [rp1]              ; start with r10 = rp2=rp1

        mov     carry_steps, doublesteps

        sub     i, 1                    ; dec     i
        ja      .top_outer_loop_64

        ; All the middle terms have been multiplied.  Now double it.

        mov     eax, [bnlength]         ; save bnlength
        push    rax                     ; keep stack aligned
        mov     eax, [rlength]
        mov     [bnlength], eax         ; r is of length rlength
;        invoke  double_a_bn, r
        mov     rdi, r8                 ; pass r
        call    double_a_bn             ; uses rdi & r8, and returns rax = r8
        pop     rax
        mov     [bnlength], eax         ; restore bnlength

.skip_middle_terms_64:
; Now go back and add in the squared terms.
; In the following loops, the following pointers are used
;   n1p(rdi) = points to the parts of n being used
;   rp1(rsi) = points to part of doublebignumber used in outer loop
;   rbx = points to part of doublebignumber for carry flag loop

        ; be careful, the next dozen or so lines are confusing!

        sub     rax, rax                ; zero out rax

        ; determine whether r is on an odd or even qword in the number
        mov     eax, [bnlength]
        shl     eax, 1                  ; double wide width
        sub     eax, [rlength]          ; 2*bnlength-rlength
        mov     rdx, rax                ; save this for a moment, keep top of rdx clear
        and     eax, 0008h              ; check the odd sign bit - qword (8)

        mov     rsi, r8                 ; load r pointer in rsi
        add     rsi, rax                ; depending on odd or even byte

        shr     edx, 3                  ; assumes qword size 
        add     edx, 1                  ; inc dx
        and     edx, 0FFFFFFFEh         ; ~2+1, turn off last bit, mult of 2
        shl     edx, 2
        mov     rdi, r9                 ; load n1p pointer in rdi

        add     rdi, rdx

        mov     eax, [bnlength]
        sub     eax, edx
        shr     eax, 3                  ; 1 byte = 1/8 qword
        mov     i, eax

        shl     eax, 1                  ; double steps
        sub     eax, 2                  ; only 2*s-2 steps are really needed
        mov     doublesteps, eax
        mov     carry_steps, eax

.top_outer_loop_squares_64:

        mov     rax, [rdi]
        mul     rax                     ; square it

        mov     rbx, rsi
        add     rbx, 8                  ; increase by size of qword
        add     [rbx-8], rax            ; add low qword
        adc     [rbx], rdx              ; add high qword
        jnc     .no_more_carry_squares_64 ; carry loop not necessary

        mov     ecx, carry_steps        ; how many till end of double big number
        jecxz   .no_more_carry_squares_64
        add     rbx, 8                  ; move pointer to next qword

        ; loop until no more carry or until end of double big number
.top_carry_loop_squares_64:
        add     qword [rbx], 1          ; use add, not inc
        jnc     .no_more_carry_squares_64
        add     rbx, 8                  ; increase by size of qword
        loop    .top_carry_loop_squares_64

.no_more_carry_squares_64:
        add     rdi, 8                  ; increase by qword size
        add     rsi, 16                 ; increase by 2*qword size

        sub     doublesteps, 2          ; reduce the carry steps needed
        mov     carry_steps, doublesteps

        sub     i, 1                    ; dec     i
        ja      .top_outer_loop_squares_64

        ; result is in r

.bottom:

; since it is a square, the result has to already be positive

        mov     rax, r8                 ; return r in rax

.quit_proc:
     UNFRAME rbx, r12, r13, r14, r15
        ret

;; unsafe_square_bn   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n * u  where u is an unsigned integer
; mult_bn_int   PROC USES di si, r:bn_t, n:bn_t, u:word
; r is passed in rdi
; n is passed in rsi
; u is passed in edx
CGLOBAL mult_bn_int
mult_bn_int:

; LOCAL   lu:qword  ; long unsigned integer in 64 bit math
; use r9 instead of lu

     FRAME rbx

        mov     ecx, [bnlength]

        mov     r8, rdi                 ; save for return value, r
        sub     r9, r9                  ; clear upper r9
        mov     r9d, edx

        ; no need to clear r

        shr     ecx, 3                  ; byte = 1/8 qword
        sub     rbx, rbx                ; use rbx for temp holding carried qword

.top_loop_64:
        mov     rax, [rsi]              ; load next qword from n
        mul     r9                      ; n * lu
        add     rax, rbx                ; add last carried upper qword
        adc     rdx, 0                  ; inc the carried qword if carry flag set
        mov     rbx, rdx                ; save high dword in rbx
        mov     [rdi], rax              ; save low qword

        add     rdi, 8                  ; next qword in r
        add     rsi, 8                  ; next qword in n
;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64

        mov     rax, r8                 ; return r in rax

     UNFRAME rbx
        ret

;; mult_bn_int   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r *= u  where u is an unsigned integer
; mult_a_bn_int   PROC USES di si, r:bn_t, u:word
; r is passed in rdi
; u is passed in esi
CGLOBAL mult_a_bn_int
mult_a_bn_int:

     FRAME rbx
        mov     ecx, [bnlength]         ; set outer loop counter

        mov     r8, rdi                 ; save for return value, r
        sub     r9, r9                  ; clear upper r9
        mov     r9d, esi

        ; no need to clear r
        shr     ecx, 3                  ; byte = 1/8 qword
        sub     rbx, rbx                ; use rbx for temp holding carried qword

.top_loop_64:
        mov     rax, [rdi]              ; load next qword from r
        mul     r9                      ; r * u
        add     rax, rbx                ; add last carried upper qword
        adc     rdx, 0                  ; inc the carried qword if carry flag set
        mov     rbx, rdx                ; save high qword in rbx
        mov     [rdi], rax              ; save low qword

        add     rdi, 8                  ; next qword in r
;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64

        mov     rax, r8                 ; return r in rax

     UNFRAME rbx
        ret

;; mult_a_bn_int   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n / u  where u is an unsigned integer
; unsafe_div_bn_int   PROC USES di si, r:bn_t, n:bn_t, u:word
; r is passed in rdi
; n is passed in rsi
; u is passed in edx
CGLOBAL unsafe_div_bn_int
unsafe_div_bn_int:

; LOCAL  sign:byte
; pushed 8 bytes for saved registers
%define sign   rbp - 16
;                    24 = pushed + allocated local memory

     FRAME rbx
        sub     rsp, 16                 ; room for local variable "sign"

        sub     r10, r10                ; clear upper r10
        mov     r10d, edx               ; save u in r10

        sub     rax, rax                ; clear uppper rax
        mov     eax, [bnlength]
        mov     rbx, rsi
        add     rbx, rax
        mov     al, byte [rbx-1]
        and     al, 80h                 ; check the sign bit
        mov     byte [sign], al
        jz      .already_pos

;        invoke  neg_a_bn, n
        push    rdi                     ; save r
        mov     rdi, rsi                ; pass n
        call    neg_a_bn                ; uses rdi & r8
        mov     rsi, rax
        pop     rdi

.already_pos:

        mov     r8, rdi                 ; save for return value, r

        sub     rcx, rcx                ; clear upper rcx
        mov     ecx, [bnlength]         ; set outer loop counter
        ; past most significant portion of the number
        add     rsi, rcx                ; r + bnlength
        add     rdi, rcx                ; n + bnlength

        ; no need to clear r here, values get mov'ed, not add'ed
        shr     ecx, 3                  ; byte = 1/8 qword

        ; need to start with most significant portion of the number
        sub     rsi, 8                  ; most sig qword
        sub     rdi, 8                  ; most sig qword

        sub     rdx, rdx                ; clear rdx register
                                        ; for first time through loop
.top_loop_64:
        mov     rax, [rsi]              ; load next qword from n
        div     r10                     ; r10 = u
        mov     [rdi], rax              ; store low qword
                                        ; leave remainder in rdx

        sub     rsi, 8                  ; next qword in n
        sub     rdi, 8                  ; next qword in r

;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64

        cmp     byte [sign], 0         ; is result + or - ?
        je      .pos_answer             ; yes

;        invoke  neg_a_bn, r
        mov     rdi, r8                 ; pass r
        sub     rsp, 8                  ; 16 byte align
        call    neg_a_bn                ; uses rdi & r8
        add     rsp, 8                  ; in this case rax = r8

.pos_answer:

        mov     rax, r8                 ; return r in ax
     UNFRAME rbx
        ret

;; unsafe_div_bn_int   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r /= u  where u is an unsigned integer
; div_a_bn_int   PROC USES si, r:bn_t, u:word
; r is passed in rdi
; u is passed in esi
CGLOBAL div_a_bn_int
div_a_bn_int:

;LOCAL  sign:byte
; pushed 8 bytes for saved registers
%define sign   rbp - 8
;                    16 = pushed rbx + allocated local memory, 16 byte aligned

     FRAME rbx
        sub     rsp, 8                  ; room for local variable "sign"
        mov     r8, rdi                 ; save for return value, r
        sub     r10, r10                ; clear upper r10
        mov     r10d, esi

        sub     rax, rax                ; clear uppper rax
        mov     eax, [bnlength]
        mov     rbx, rdi
        add     rbx, rax
        mov     al, byte [rbx-1]
        and     al, 80h                 ; check the sign bit
        mov     byte [sign], al
        jz      .already_pos

;        invoke  neg_a_bn, r
        call    neg_a_bn                ; uses rdi & r8
        mov     r8, rax

.already_pos:

        sub     rcx, rcx                ; clear upper rcx
        mov     ecx, [bnlength]         ; set outer loop counter
        mov     rdi, r8
        ; past most significant portion of the number
        add     rdi, rcx

        ; no need to clear r here, values get mov'ed, not add'ed
        shr     ecx, 3                  ; byte = 1/8 qword

        ; need to start with most significant portion of the number
        sub     rdi, 8                  ; most sig qword

        sub     rdx, rdx                ; clear rdx register
                                        ; for first time through loop
.top_loop_64:
        mov     rax, [rdi]              ; load next qword from r
        div     r10                     ; r10 = u
        mov     [rdi], rax              ; store low qword
                                        ; leave remainder in rdx

        sub     rdi, 8                  ; next qword in r
;        loop    top_loop_64
        sub     ecx, 1
        cmp     ecx, 0
        jg      .top_loop_64

        cmp     byte [sign], 0          ; is result + or - ?
        je      .pos_answer             ; yes

;        invoke  neg_a_bn, r
        mov     rdi, r8
        call    neg_a_bn                ; uses rdi & r8

.pos_answer:

        mov     rax, r8                 ; return r in ax
     UNFRAME rbx
        ret

;; div_a_bn_int   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; bf_t routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = 0    (just like clear_bn() but loads bflength+2 instead of bnlength)
; clear_bf   PROC USES di, r:bf_t
; r is passed in rdi
CGLOBAL clear_bf
clear_bf:

        sub     rcx, rcx                ; clear upper rcx
        mov     ecx, [bflength]
        mov     r8, rdi                 ; save for return value, r

        sub     rax, rax                ; clear rax
        shr     ecx, 3                  ; 1 byte = 1/8 qword
        rep     stosq                   ; clear r, qword at a time
        stosw                           ; plus the exponent

        mov     rax, r8                 ; return r in rax
        ret

;; clear_bf   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; r = n
; copy_bf   PROC USES di si, r:bf_t, n:bf_t
; r is passed in rdi
; n is passed in rsi
CGLOBAL copy_bf
copy_bf:

        sub     rcx, rcx                ; clear upper rcx
        mov     ecx, [bflength]
        mov     r8, rdi                 ; save for return value, r
        add     ecx, 2

        shr     ecx, 3                  ; 1 byte = 1/8 qword
        rep     movsq                   ; copy qword at a time
        movsw                           ; plus the exponent

        mov     rax, r8                 ; return r in rax
        ret

;; copy_bf   ENDP

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LDBL bftofloat(bf_t n);
; converts a bf number to a 10 byte real
;
; bftofloat   PROC USES di si, n:bf_t
; n is passed in rdi
CGLOBAL bftofloat
bftofloat:

;   LOCAL value[11]:BYTE   ; 11=10+1
; pushed 8 bytes for saved registers
%define value(x)   [rbp - 24 + x]
;                         24 = pushed + allocated local memory

     FRAME rbx
      sub      rsp, 16                  ; room for local variable "value"

      mov      r8, rdi                  ; save n
      sub      rcx, rcx                 ; clear upper rcx
      sub      rbx, rbx                 ; clear upper rbx

      mov      ecx, 9                   ; need up to 9 bytes
      cmp      dword [bflength], 10     ; but no more than bflength-1
      jae      .movebytes_set
      mov      ecx, [bflength]          ; bflength is less than 10
      sub      ecx, 1                   ; ecx=movebytes=bflength-1, 1 byte padding
.movebytes_set:

      ; clear value
      sub      rax, rax
      mov      qword value(0), rax      ; clear first 8 bytes
      mov      word value(8), ax        ; clear next 2 bytes
      mov      byte value(10), al       ; clear last byte

      ; copy bytes from n to value
      lea      rdi, value(9)
      sub      rdi, rcx                 ; rcx holds movebytes
      mov      ebx, [bflength]
      sub      rbx, 1
      sub      rbx, rcx                 ; rcx holds movebytes
      mov      rsi, r8
      add      rsi, rbx                 ; n+bflength-1-movebytes
      rep movsb
      mov      bl, byte [rsi]           ; save sign byte, rsi now points to it
      add      rsi, 1                   ; point to exponent
      mov      dx, word [rsi]           ; use dx as exponent
      shl      dx, 3                    ; 256^n = 2^(8n)

      ; adjust for negative values
      and      bl, 10000000b            ; determine sign
      jz       .not_neg_64
      neg      qword value(0)           ; take the negative of the 9 byte number
      cmc                               ; toggle carry flag
      not      byte value(8)            ; notice this last one is byte ptr
      adc      byte value(8), 0
.not_neg_64:

      cmp      byte value(8), 0         ; test for 0
      jnz      .top_shift_64
      fldz
      jmp      .return

      ; Shift until most signifcant bit is set.
.top_shift_64:
      test     byte value(8), 10000000b ; test msb
      jnz      .bottom_shift_64
      sub      dx, 1                    ; decrement exponent
      shl      qword value(0), 1        ; shift left the 9 byte number
      rcl      byte value(8), 1         ; notice this last one is byte ptr
      jmp      .top_shift_64
.bottom_shift_64:

      ; round last byte
      cmp      byte value(0), 80h
      jb       .bottom                  ; no rounding necessary
      add      qword value(1), 1
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
     UNFRAME rbx
      ret

;; bftofloat   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LDBL floattobf(bf_t n, LDBL f);
; converts a 10 byte real to a bf number
;
; floattobf   PROC USES di si, n:bf_t, f:REAL10
; n is passed in rdi
; f is passed on the stack, pointed to by flt(x)
CGLOBAL floattobf
floattobf:

%define flt(x)    [rbp + PTRSZ + PTRSZ + x]
;                        16 = saved rbp + return address
;   LOCAL value[9]:BYTE   ; 9=8+1
%define value(x)   [rbp - 16 + x]
;                         16 = allocated local memory

      push     rbp                      ; if FRAME not used, do this
      mov      rbp, rsp
      sub      rsp, 16                  ; room for local variable "value"

      sub      rcx, rcx                 ; clear upper rcx
;      invoke   clear_bf, n
;      sub      rsp, 8                   ; 16 byte align
      call clear_bf                     ; n is in rdi
;      add      rsp, 8
      mov      r8, rax

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
      mov      rax, qword flt(0)
      mov      qword value(1), rax

      ; get exponent in dx
      mov      dx, word flt(8)          ; location of exponent
      and      dx, 7FFFh                ; remove sign bit
      sub      dx, 3FFFh+7              ; biased -> unbiased, + adjust

      ; Shift down until exponent is a mult of 8 (2^8n=256n)
.top_shift_64:
      test     dx, 111b                 ; expon mod 8
      jz       .bottom
      add      dx, 1                    ; increment exponent
;      shr      dword value(5), 1        ; shift right the 9 byte number
;      rcr      dword value(1), 1
      shr      qword value(1), 1        ; shift right the 9 byte number
      rcr      byte value(0), 1         ; notice this last one is byte ptr
      jmp      .top_shift_64

.bottom:

      ; Don't bother rounding last byte as it would only make a difference
      ; when bflength < 9, and then only on the last bit.

      ; move data into place, from value to n
      lea      rsi, value(9)
      sub      rsi, rcx                 ; rcx holds movebytes
      mov      rdi, r8
      sub      rax, rax                 ; clear upper rax
      mov      eax, [bflength]
      add      rdi, rax
      sub      rdi, 1
      sub      rdi, rcx                 ; rcx holds movebytes
      rep movsb
      add      rdi, 1
      sar      dx, 3                    ; divide expon by 8, 256^n=2^8n
      mov      word [rdi], dx           ; store exponent

      ; get sign
      test     byte flt(9), 10000000b   ; test sign bit
      jz       .not_negative
      mov      rdi, r8
;      invoke   neg_a_bf, n
;      push     r8                       ; 16 byte align
      call neg_a_bf
;      pop      r8

.not_negative:
.return:

      mov      rax, r8                  ; return r in rax
      mov      rsp, rbp                 ; if UNFRAME not used, do this
      pop      rbp
      ret

;; floattobf   endp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LDBL bntofloat(bf_t n);
; converts a bn number to a 10 byte real
; (the most speed critical of these to/from float routines)
; bntofloat   PROC USES di si, n:bn_t
; n is passed in rdi
CGLOBAL bntofloat
bntofloat:

;   LOCAL value[11]:BYTE   ; 11=10+1
%define value(x)   [rbp - 24 + x]
%define value9     rbp - 24 + 9
;                        24 = pushed rbx + allocated local memory

      FRAME rbx
      sub      rsp, 16                  ; room for local variable "value"

      mov      r8, rdi                  ; save n
      sub      rcx, rcx                 ; clear upper rcx
      sub      rbx, rbx                 ; clear upper rbx

      ; determine the most significant byte, not 0 or FF
      mov      rsi, r8
      sub      rsi, 1
      mov      ecx, [bnlength]         ; initialize ecx with full bnlength
      add      rsi, rcx                ; n+bnlength-1
      mov      bl, byte [rsi]          ; top byte
      cmp      bl, 0                   ; test top byte against 0
      je       .determine_sig_bytes
      cmp      bl, 0FFh                ; test top byte against -1
      jne      .sig_bytes_determined

.determine_sig_bytes:
      sub      ecx, 1                  ; now bnlength-1
.top_sig_byte:
      sub      rsi, 1                  ; previous byte
      cmp      byte [rsi], bl          ; does it have the right stuff?
      jne      .sig_bytes_determined   ; (ie: does it match top byte?)
;      loop     top_sig_byte            ; decrement ecx and repeat
      sub      ecx, 1
      cmp      ecx, 0
      jg       .top_sig_byte

; At this point, it must be 0 with no sig figs at all
; or -1/(256^bnlength), one bit away from being zero.
      cmp      bl, 0                   ; was it zero?
      jnz      .not_zero               ; no, it was a very small negative
                                       ; yes
      fldz                             ; return zero
      jmp      .return

.not_zero:
      mov      eax, [intlength]
      sub      eax, [bnlength]         ; should end up < 1 word in size
      shl      eax, 3                  ; 256^n=2^8n, now more like movebits
      add      ax, 3FFFh+0             ; bias, no adjustment necessary
      or       ah, 10000000b           ; turn on sign flag
      mov      word value(9), ax       ; store exponent
      mov      word value(7), 8000h    ; store mantissa of 1 in most sig bit
      ; clear rest of value that is actually used
      mov      dword value(1), 0
      mov      word value(5), 0

      fld      tword value(1)
      jmp      .return

.sig_bytes_determined:
      mov      edx, ecx             ; save in edx for later
      cmp      ecx, 9-1             ; no more than ecx bytes
      jb       .set_movebytes
      mov      ecx, 9-1             ; up to 8 bytes
.set_movebytes:                     ; ecx now holds movebytes
                                    ; rsi still points to most non-0 sig byte
      sub      rsi, rcx             ; rsi now points to first byte to be moved
      add      ecx, 1               ; can be up to 9

      ; clear value
      mov      qword value(0), 0
      mov      word value(8),  0
      mov      byte value(10), 0

      ; copy bytes from n to value  ; rsi still holds first move byte of n
;      lea      rdi, value+9  ; ??????
      lea      rdi, [value9]
      sub      rdi, rcx             ; rcx holds movebytes
      ; value[9] is in rdi, first move byte of n is now in rsi
      rep movsb

      ; adjust for negative values
      xor      rax, rax                ; use rax as a flag
      ; get sign flag                  ; top byte is still in bl
      and      bl, 10000000b           ; determine sign
      jz       .not_neg_64
      neg      qword value(0)          ; take the negative of the 9 byte number
      cmc                              ; toggle carry flag
      not      byte value(8)           ; notice this last one is byte ptr
      adc      byte value(8), 0
      jnc      .not_neg_64              ; normal
      mov      byte value(8), 10000000b ;n was FFFF...0000...
      add      rax, 1                  ; set rax to 1 to flag this special case

.not_neg_64:
      sub      edx, [bnlength]         ; adjust exponent
      add      edx, [intlength]        ; adjust exponent
      shl      edx, 3                  ; 256^n=2^8n
      add      edx, eax                ; see special case above
      ; Shift until most signifcant bit is set.
.top_shift_64:
      test     byte value(8), 10000000b  ; test msb
      jnz      .bottom
      sub      edx, 1                  ; decrement exponent
      shl      qword value(0), 1       ; shift left the 9 byte number
      rcl      byte value(8), 1        ; notice this last one is byte ptr
      jmp      .top_shift_64

; don't bother rounding, not really needed while speed is.

.bottom:

      ; adjust exponent
      add      dx, 3FFFh+7-8           ; unbiased -> biased, + adjusted
      or       dh, bl                  ; set sign bit if set
      mov      word value(9), dx

      ; unlike float and double, long double is returned on fpu stack
      fld      tword value(1)          ; load return value

.return:
      UNFRAME rbx
      ret

;; bntofloat   endp

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
; f is passed on the stack
; exp_ptr  is passed in rdi
CGLOBAL extract_256
extract_256:

%define f [rbp + PTRSZ + PTRSZ]
;                16 = return address + saved rbp

; local  expon:sword, exf:real10, tmp_word:word
; location = pushed rbx + room for local variables = 40
%define expon  [rbp - 40]
%define exf    [rbp - 40 + 8]
%define tmp_word [rbp - 40 + 24]

      FRAME rbx
      sub      rsp, 32                  ; room for local variables

      fld     tword f                   ; f
      ftst                              ; test for zero
      fstsw   word tmp_word
      fwait
      mov     ax, word tmp_word
      sahf
      jnz     .not_zero                 ; proceed

      mov     rbx, [rdi]                ; exp_ptr
      mov     dword [rbx], 0            ; save = in *exp_ptr
      jmp     .bottom                   ; f, which is zero, is already on stack

.not_zero:

                                        ; f is already on stack
      fxtract                           ; mant exp, where f=mant*2^exp
      fxch                              ; exp mant
      fistp   word expon                ; mant
      fwait
      mov     ax, word expon
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
      mov     rbx, [rdi]                ; exp_ptr
      mov     word [rbx], dx           ; save in *exp_ptr

      fwait

.bottom:
        ; unlike float and double, long double is returned on fpu stack
       
      UNFRAME rbx
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
; f is passed on the stack
; n  is passed in edi
CGLOBAL scale_256
scale_256:

%define f [rbp + PTRSZ + PTRSZ]
;                16 = return address + pushed rbp
; local variable, n
%define n   [rbp - 16]
;                  16 = allocated local memory

      push     rbp                      ; if FRAME not used, do this
      mov      rbp, rsp
      sub      rsp, 16                  ; room for local variable "n"

      cmp     edi, 0
      jne     .non_zero
      fld     tword f
      jmp     .bottom                   ; don't bother with scales of zero

.non_zero:
      shl     edi, 3                    ; 8n
      mov     dword n, edi 
      fild    dword n                   ; 8n
      fld     tword f                   ; f 8n
; the fscale range limits for 8087/287 processors won't be a problem here
      fscale                            ; new_f=f*2^(8n)=f*256^n  8n
      fstp    st1                       ; new_f

.bottom:
      ; unlike float and double, long double is returned on fpu stack
      mov      rsp, rbp                 ; if UNFRAME not used, do this
      pop      rbp
      ret

;; scale_256   ENDP

