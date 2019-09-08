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
%include "xfract_a.inc"

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

CGLOBAL    multiply ;    x:dword, y:dword, n:dword
;                       [ebp+8], [ebp+12], [ebp+16]
multiply:
align 4
        FRAME   ebx
        mov     eax, dword [ebp + 0x8]         ; load X into EAX
        mov     ebx, dword [ebp + 0xC]         ; load Y into EBX
        mov     ecx, dword [ebp + 0x10]        ; set up the shift
;hlt
        imul    ebx                     ; do the multiply with Y
        cmp     cl, 32                  ; ugly klooge:  check for 32-bit shift
        jb      short fastm1            ;  < 32 bits:  no problem
        mov     edx, eax                ;  >= 32 bits:  manual shift
        sub     ecx, 32                 ;  ...
fastm1: shrd    eax, edx, cl            ; shift down 'n' bits
        js      fastm3
        sar     edx, cl
        jne     overmf
        shld    edx, eax, 16
        UNFRAME ebx
        ret
fastm3: sar     edx, cl
        inc     edx
        jne     overmf
        shld    edx, eax, 16
        UNFRAME ebx
        ret
overmf:
        mov     eax,07fffffffh          ; overflow value
        mov     dword [overflow],1      ; flag overflow
        UNFRAME ebx
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

CGLOBAL    divide   ;    x:dword, y:dword, n:dword
;                       [ebp+8], [ebp+12], [ebp+16]
divide:
align 4
        FRAME   ebx
        mov     ecx, 32                 ; set up the shift for later
        sub     ecx, dword [ebp + 0x10] ; (for large shift counts - faster)

        mov     edx, dword [ebp + 0x8]  ; load X into EDX (shifts to EDX:EAX)
        mov     ebx, dword [ebp + 0xC]  ; load Y into EBX

        mov     dword [sign], 0         ; clear out the sign flag
        cmp     edx, 0                  ; is X negative?
        jge     short divides1          ;  nope
        not     dword [sign]            ;  yup.  flip signs
        neg     edx                     ;   ...
divides1:
        cmp     ebx, 0                  ; is Y negative?
        jg      short divides2          ;  nope
        jl      short setsign1
        jmp     overd1                  ; don't divide by zero, set overflow
setsign1:
        not     dword [sign]            ;  yup.  flip signs
        neg     ebx                     ;   ...
divides2:
        mov     eax, 0                  ; clear out the low-order bits
fastd1: cmp     cx, 0                   ; done shifting?
        je      fastd2                  ; yup.
        shr     edx, 1                  ; shift one bit
        rcr     eax, 1                  ;  ...
        loop    fastd1                  ; and try again
fastd2:
        cmp     edx, ebx                ; umm, will the divide blow out?
        jae     overd1                  ;  yup.  better skip it.
        div     ebx                     ; do the divide
        cmp     eax, 0                  ; did the sign flip?
        jl      overd1                  ;  then we overflowed
        cmp     dword [sign], 0         ; is the sign reversed?
        je      short divides3          ;  nope
        neg     eax                     ; flip the sign
divides3:
;        shl     eax, 16
;        shl     eax, 16
;        shld    edx, eax, 16
;        shld    edx, eax, 16
;        mov     eax, edx
        jmp     dividereturn            ; back to common code

overd1:
        mov     eax, 07fffffffh         ; overflow value
        mov     dword [overflow], 1     ; flag overflow

dividereturn:                           ; that's all, folks!
        UNFRAME ebx
        ret
;divide          endp


