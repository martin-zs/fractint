;       Generic assembler routines that have very little at all
;       to do with fractals.
;

; ---- 32-bit Multiply/Divide Routines (includes 16-bit emulation)
;
;       multiply()
;       divide()
;

DEFAULT REL

;%use smartalign
%include "xfract_a-64.inc"

; external functions
;
; external data
CEXTERN overflow   ;:DWORD  4 - int

section .bss
alignb  16

sign:                      ; sign flag goes here
        resb   INTSZ

section .text

; =======================================================
;
;       32-bit integer multiply routine with an 'n'-bit shift.
;       Overflow condition returns 0x7fffffffh with overflow = 1;
;
;       long x, y, z, multiply();
;       int n;
;
;       z = multiply(x,y,n)
;

CGLOBAL    multiply ;    x:qword, y:qword, n:dword
;                        in RDI,  in RSI,  in EDX
multiply:
align 16

        mov     ecx, edx                ; set up the shift
        mov     rax, rdi                ; load X into EAX
        imul    rsi                     ; do the multiply with Y
        cmp     ecx, 32                 ; ugly klooge:  check for 32-bit shift
        jb      short fastm1            ;  < 32 bits:  no problem
        sar     rax, 32                 ;  >= 32 bits:  manual shift
        sub     ecx, 32                 ;  ...
fastm1: sar    rax, cl                  ; shift down 'n' bits
        js      fastm3
        mov     rdx, rax
        sar     rdx, 32                 ; remove eax bits first
        sar     rdx, cl
        jne     overmf
        ret
fastm3: mov     rdx, rax
        sar     rdx, 32                 ; remove eax bits first
        sar     rdx, cl
        inc     edx
        jne     overmf
        ret
overmf:
        mov     rax,07fffffffh          ; overflow value
        mov     dword [overflow],1      ; flag overflow
        ret

;multiply        endp


; =======================================================
;
;       32-bit integer divide routine with an 'n'-bit shift.
;       Overflow condition returns 0x7fffffffh with overflow = 1;
;
;       long x, y, z, divide();
;       int n;
;
;       z = divide(x,y,n);      /* z = x / y; */
;

CGLOBAL    divide   ;    x:qword, y:qword, n:dword
;                        in RDI,  in RSI,  in EDX
divide:
align 16

        mov     cx, 32                  ; set up the shift for later
        sub     cx, dx                  ; (for large shift counts - faster)

        mov     rdx, rdi                ; load X into RDX (shifts to EDX:EAX)
;        mov     rbx, rsi                ; leave Y in RSI

        mov     dword [sign], 0         ; clear out the sign flag
        cmp     rdx, 0                  ; is X negative?
        jge     short divides1          ;  nope
        not     dword [sign]            ;  yup.  flip signs
        neg     rdx                     ;   ...
divides1:
        cmp     rsi, 0                  ; is Y negative?
        jg      short divides2          ;  nope
        jl      short setsign1
        jmp     overd1                  ; don't divide by zero, set overflow
setsign1:
        not     dword [sign]            ;  yup.  flip signs
        neg     rsi                     ;   ...
divides2:
        mov     eax, 0                  ; clear out the low-order bits
fastd1: cmp     cx, 0                   ; done shifting?
        je      fastd2                  ; yup.
        shr     edx, 1                  ; shift one bit
        rcr     eax, 1                  ;  ...
        loop    fastd1                  ; and try again
fastd2:
        cmp     edx, esi                ; umm, will the divide blow out?
        jae     overd1                  ;  yup.  better skip it.
        div     esi                     ; do the divide
        cmp     eax, 0                  ; did the sign flip?
        jl      overd1                  ;  then we overflowed
        cmp     dword [sign], 0         ; is the sign reversed?
        je      short divides3          ;  nope
        neg     rax                     ; flip the sign
divides3:
;        shl     rax, 16
;        shl     rax, 16
;        shld    rdx, rax, 16
;        shld    rdx, rax, 16
;        mov     rax, rdx
        jmp     dividereturn            ; back to common code

overd1:
        mov     rax, 07fffffffh         ; overflow value
        mov     dword [overflow], 1     ; flag overflow

dividereturn:                           ; that's all, folks!
        ret
;divide          endp


CGLOBAL    divide_test   ;    x:qword, y:qword, n:dword
;                        in RDI,  in RSI,  in EDX
divide_test:
align 16
        FRAME rbx

        mov     cx, 32
        sub     cx, dx
        mov     rax, rdi                ; load X into RAX
;        mov     rbx, rsi                ; leave Y in RSI
        cmp     rsi, 0
        je      overd1a

        mov     rdx, 0                  ; zero out RDX
        cmp     rax,0                   ; is X negative?
        jge     short divides1a          ;  nope
        neg     rdx
divides1a:

        shrd    rdx, rax, cl
        cmp     rax, rsi                ; umm, will the divide blow out?
        jae     overd1a                  ;  yup.  better skip it.

        idiv    rsi                     ; signed divide

        jmp     dividereturna            ; back to common code

overd1a:
        mov     rax,07fffffffh          ; overflow value
        mov     dword [overflow],1      ; flag overflow

dividereturna:                           ; that's all, folks!
        UNFRAME rbx
        ret
;divide          endp


