;TITLE fpu087.asm (C) 1989, Mark C. Peterson, CompuServe [70441,3353]
;SUBTTL All rights reserved.
;
;  Code may be used in any program provided the author is credited
;    either during program execution or in the documentation.  Source
;    code may be distributed only in combination with public domain or
;    shareware source code.  Source code may be modified provided the
;    copyright notice and this message is left unchanged and all
;    modifications are clearly documented.
;
;    I would appreciate a copy of any work which incorporates this code.
;
;    Mark C. Peterson
;    405-C Queen St., Suite #181
;    Southington, CT 06489
;    (203) 276-9721
;
;  References:
;     The VNR Concise Encyclopedia of Mathematics
;        by W. Gellert, H. Hustner, M. Hellwich, and H. Kastner
;        Published by Van Nostrand Reinhold Comp, 1975
;
;     80386/80286 Assembly Language Programming
;        by William H. Murray, III and Chris H. Pappas
;        Published by Osborne McGraw-Hill, 1986
;
;  History since Fractint 16.3:
;     CJLT changed e2x and Log086 algorithms for more speed
;     CJLT corrected SinCos086 for -ve angles in 2nd and 4th quadrants
;     CJLT speeded up SinCos086 for angles >45 degrees in any quadrant
;     (See comments containing the string `CJLT')
; 14 Aug 91 CJLT removed r16Mul - not called from anywhere
; 21 Aug 91 CJLT corrected Table[1] from 6 to b
;                improved bx factors in Log086 for more accuracy
;                corrected Exp086 overflow detection to 15 from 16 bits.
; 07 Sep 92 MP   corrected problem in FPUcplxlog
; 07 Sep 92 MP   added argument test for FPUcplxdiv
; 06 Nov 92 CAE  made some varibles public for PARSERA.ASM
; 07 Dec 92 CAE  sped up FPUsinhcosh function
;
; CJLT=      Chris J Lusby Taylor
;            32 Turnpike Road
;            Newbury, England (where's that?)
;        Contactable via Compuserve user Stan Chelchowski [100016,351]
;                     or Tel 011 44 635 33270 (home)
;
; CAE=   Chuck Ebbert  CompuServe [76306,1226]
;
;
;PROCs in this module:
;FPUcplxmul     x:qword, y:qword, z:qword
;FPUcplxdiv     x:qword, y:qword, z:qword
;FPUcplxexp     x:qword, z:qword
;FPUcplxlog     x:qword, z:qword
;FPUsinhcosh    x:qword, sinh:qword, cosh:qword
;FPUsincos      x:qword, sinx:qword, cosx:qword
;FPUaptan387    x:qword, y:qword, z:qword

DEFAULT REL

;%use smartalign
%include "xfract_a-64.inc"

; external functions
; none

; external data
CEXTERN overflow   ;:DWORD  4 - int
CEXTERN infinity   ;:TWORD  12 - LDBL
CEXTERN PointFive  ;:QWORD  8 - double
CEXTERN _2_        ;:QWORD  8 - double

section .bss
alignb  16

sign:                      ; sign flag goes here
        resb   INTSZ
temp:
        resb   DBLSZ
Status:
        resb   2
Control:
        resb   2

section .text

CGLOBAL FPUcplxmul  ;   x:qword, y:qword, z:qword
; Pointers              in RDI,  in RSI,  in RDX
FPUcplxmul:
align 16

;   mov   bx, x
   fld   QWORD [rdi]       ; x.x
   fld   QWORD [rdi+DBLSZ] ; x.y, x.x
;   mov   bx, y
   fld   QWORD [rsi]       ; y.x, x.y, x.x
   fld   QWORD [rsi+DBLSZ] ; y.y, y.x, x.y, x.x
;   mov   bx, z
   fld   st0               ; y.y, y.y, y.x, x.y, x.x
   fmul  st0, st3          ; y.y*x.y, y.y. y.x, x.y, x.x
   fld   st2               ; y.x, y.y*x.y, y.y, y.x, x.y, x.x
   fmul  st0, st5          ; y.x*x.x, y.y*x.y, y.y, y.x, x.y, x.x
   fsubr                   ; y.x*x.x - y.y*x.y, y.y, y.x, x.y, x.x
   fstp  QWORD [rdx]       ; y.y, y.x, x.y, x.x
   fmulp st3, st0          ; y.x, x.y, x.x*y.y
   fmul                    ; y.x*x.y, x.x*y.y
   fadd                    ; y.x*x.y + x.x*y.y
   fstp  QWORD [rdx+DBLSZ]
   ret
;FPUcplxmul     ENDP


CGLOBAL FPUcplxdiv  ;   x:qword, y:qword, z:qword
; Pointers              in RDI,  in RSI,  in RDX
FPUcplxdiv:
align 16

;   mov   bx, x
   fld   QWORD [rdi]       ; x.x
   fld   QWORD [rdi+DBLSZ] ; x.y, x.x
;   mov   bx, y
   fld   QWORD [rsi]       ; y.x, x.y, x.x
   fld   QWORD [rsi+DBLSZ] ; y.y, y.x, x.y, x.x
   fld   st0               ; y.y, y.y, y.x, x.y, x.x
   fmul  st0, st0          ; y.y*y.y, y.y, y.x, x.y, x.x
   fld   st2               ; y.x, y.y*y.y, y.y, y.x, x.y, x.x
   fmul  st0, st0          ; y.x*y.x, y.y*y.y, y.y, y.x, x.y, x.x
   fadd                    ; mod, y.y, y.x, x.y, x.x

   ftst                    ; test whether mod is (0,0)
   fstsw word [Status]
   mov   ax, word [Status]
   and   ah, 01000101b
   cmp   ah, 01000000b
   jne   NotZero

   fstp  st0
   fstp  st0
   fstp  st0
   fstp  st0
   fstp  st0

   fld   tword [infinity]
   fld   st0
;   mov   bx, z
   fstp  QWORD [rdx]
   fstp  QWORD [rdx+DBLSZ]
;   mov   ax,save_release
;   cmp   ax,1920
;   jle   ExitDiv       ; before 19.20 overflow wasn't set
   mov   dword [overflow], 1
   jmp   ExitDiv

NotZero:
   fdiv  st1, st0             ; mod, y.y=y.y/mod, y.x, x.y, x.x
   fdivp st2, st0             ; y.y, y.x=y.x/mod, x.y, x.x
;   mov   bx, z
   fld   st0                  ; y.y, y.y, y.x, x.y, x.x
   fmul  st0, st3             ; y.y*x.y, y.y. y.x, x.y, x.x
   fld   st2                  ; y.x, y.y*x.y, y.y, y.x, x.y, x.x
   fmul  st0, st5             ; y.x*x.x, y.y*x.y, y.y, y.x, x.y, x.x
   fadd                       ; y.x*x.x - y.y*x.y, y.y, y.x, x.y, x.x
   fstp  QWORD [rdx]          ; y.y, y.x, x.y, x.x
   fmulp st3, st0             ; y.x, x.y, x.x*y.y
   fmul                       ; y.x*x.y, x.x*y.y
   fsubr                      ; y.x*x.y + x.x*y.y
   fstp  QWORD [rdx+DBLSZ]

ExitDiv:
   ret
;FPUcplxdiv     ENDP


CGLOBAL FPUcplxexp     ;   x:qword, z:qword
; Pointers                 in RDI,  in RSI
FPUcplxexp:
align 16

;   mov   bx, x
   fld   QWORD [rdi+DBLSZ] ; x.y
   fsincos                 ; cos, sin
   fldln2                  ; ln2, cos, sin
   fdivr QWORD [rdi]       ; x.x/ln2, cos, sin
   fld1                    ; 1, x.x/ln2, cos, sin
   fld   st1               ; x.x/ln2, 1, x.x/ln2, cos, sin
   fprem                   ; prem, 1, x.x/ln2, cos, sin
   f2xm1                   ; e**prem-1, 1, x.x/ln2, cos, sin
   fadd                    ; e**prem, x.x/ln2, cos, sin
   fscale                  ; e**x.x, x.x/ln2, cos, sin
   fstp  st1               ; e**x.x, cos, sin
   fmul  st2, st0          ; e**x.x, cos, z.y
   fmul                    ; z.x, z.y
;   mov   bx, z
   fstp  QWORD [rsi]       ; z.y
   fstp  QWORD [rsi+DBLSZ] ; < empty>
   ret
;FPUcplxexp  ENDP


CGLOBAL FPUcplxlog  ;   x:qword, z:qword
; Pointers              in RDI,  in RSI
FPUcplxlog:
align 16

;   mov   bx, x

   mov   ax, WORD [rdi+DBLSZ+6]
;   mov   ImagZero, ax
   or    ax, WORD [rdi+6]
   jnz   NotBothZero

   fldz
   fldz
   jmp   StoreZX

NotBothZero:
   fld   QWORD [rdi+DBLSZ]       ; x.y
   fld   QWORD [rdi]         ; x.x, x.y
;   mov   bx, z
   fldln2                        ; ln2, x.x, x.y
   fdiv  QWORD [_2_]             ; ln2/2, x.x, x.y
   fld   st2                     ; x.y, ln2/2, x.x, x.y
   fmul  st0, st0                ; sqr(x.y), ln2/2, x.x, x.y
   fld   st2                     ; x.x, sqr(x.y), ln2/2, x.x, x.y
   fmul  st0, st0                ; sqr(x.x), sqr(x.y), ln2/2, x.x, x.y
   fadd                          ; mod, ln2/2, x.x, x.y
   fyl2x                         ; z.x, x.x, x.y
   fxch  st2                     ; x.y, x.x, z.x
   fxch                          ; x.x, x.y, z.x
   fpatan                        ; z.y, z.x

StoreZX:
;   mov   bx, z
   fstp  QWORD [rsi+DBLSZ]   ; z.x
   fstp  QWORD [rsi]         ; <empty>
   ret
;FPUcplxlog     ENDP


CGLOBAL FPUsinhcosh  ;  x:qword, sinh:qword, cosh:qword
; Pointers              in RDI,  in RSI,     in RDX
FPUsinhcosh:
align 16

   fstcw word [Control]
   push  word [Control]         ; Save control word on the stack
   or    word [Control], 0000110000000000b
   fldcw word [Control]         ; Set control to round towards zero

;   mov   [Sign], 0              ; Assume the sign is positive
;   mov   bx, x

   fldln2                     ; ln(2)
   fdivr QWORD [rdi]      ; x/ln(2)

   cmp   BYTE [rdi+7], 0
   jns   DuplicateX

   fchs                       ; x = |x|

DuplicateX:
   fld   st0                  ; x/ln(2), x/ln(2)
   frndint                    ; int = integer(|x|/ln(2)), x/ln(2)
   fxch                       ; x/ln(2), int
   fsub  st0, st1             ; rem < 1.0, int
   fmul  qword [PointFive]    ; rem/2 < 0.5, int
      ; CAE 7Dec92 changed above from divide by 2 to multiply by .5
   f2xm1                      ; (2**rem/2)-1, int
;   fadd  _1_                  ; 2**rem/2, int
   fld1                       ; avoid memory access, do this instead
   fadd
   fmul  st0, st0             ; 2**rem, int
   fscale                     ; e**|x|, int
   fstp  st1                  ; e**|x|

   cmp   BYTE [rdi+7], 0
   jns   ExitFexp

;   fdivr _1_                  ; e**x
   fld1                       ; avoid memory access, do this instead
   fdivr

ExitFexp:
   fld   st0                  ; e**x, e**x
   fdivr QWORD [PointFive]    ; e**-x/2, e**x
   fld   st0                  ; e**-x/2, e**-x/2, e**x
   fxch  st2                  ; e**x, e**-x/2, e**-x/2
   fmul  QWORD [PointFive]    ; e**x/2,  e**-x/2, e**-x/2
      ; CAE 7Dec92 changed above from divide by 2 to multiply by .5
   fadd  st2, st0             ; e**x/2,  e**-x/2, cosh(x)
   fsubr                      ; sinh(x), cosh(x)

;   mov   bx, sinh             ; sinh, cosh
   fstp  QWORD [rsi]          ; cosh
;   mov   bx, cosh
   fstp  QWORD [rdx]          ; <empty>

   pop   word [Control]
   fldcw word [Control]       ; Restore control word
   ret
;FPUsinhcosh    ENDP


CGLOBAL FPUsincos  ;    x:qword, sinx:qword, cosx:qword
; Pointers              in RDI,  in RSI,     in RDX
FPUsincos:
align 16

;   mov   bx, x
   fld   QWORD [rdi]       ; x
   fsincos                 ; cos(x), sin(x)
;   mov   bx, cosx
   fstp  QWORD [rdx]       ; sin(x)
;   mov   bx, sinx
   fstp  QWORD [rsi]       ; <empty>
   ret
;FPUsincos   ENDP


CGLOBAL FPUaptan387  ;  x:qword, y:qword, z:qword
; Pointers              in RDI,  in RSI,  in RDX
FPUaptan387:
align 16

;   mov   bx, y
   fld   QWORD [rsi]   ; y
;   mov   bx, x
   fld   QWORD [rdi]   ; x, y
   fpatan                  ; ArtTan
;   mov   bx, z
   fstp  QWORD [rdx]   ; <empty>
   ret
;FPUaptan387    ENDP



