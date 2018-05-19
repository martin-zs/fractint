;  This file includes macros and equates that will be needed in any
;   assembly language files used by Xfractint.  1/27/2002  JCO

PTRSZ    equ    8
INTSZ    equ    4
LNGSZ    equ    8
DBLSZ    equ    8
LDBLSZ   equ   16

; ---------------------------------------------------------------------------
;  The following two macros take care of the case where gcc uses
;   underscores for assembly identifiers.
;   Define the variable U_SCORE in the Make file if underscores are required.

%macro CGLOBAL 1
%ifdef   U_SCORE
    global  _%1
%define %1 _%1
%else
    global  %1
%endif
%endmacro

%macro CEXTERN 1
%ifdef   U_SCORE
    extern  _%1
%define %1 _%1
%else
    extern  %1
%endif
%endmacro

; ---------------------------------------------------------------------------
;  The following two macros frame and unframe the operations in a routine
;  Note that the order of the passed parameters is the same in both.
%macro FRAME 1-*          ; build a stack frame
      push         rbp
      mov          rbp, rsp
;      push         rbx
;      push         rsi
;      push         rdi
%rep %0
      push %1
%rotate 1
%endrep
%endmacro

%macro UNFRAME 1-*          ; unframe before return
%rep %0
%rotate -1
          pop %1
%endrep
;      pop         rdi
;      pop         rsi
;      pop         rbx
      mov         rsp, rbp
      pop         rbp
%endmacro

; ---------------------------------------------------------------------------
;  The following defines change the format of the fpu instructions from
;  the nasm style to the Intel style
%ifdef NEED_PARENS
  %define   st0  st(0)
  %define   st1  st(1)
  %define   st2  st(2)
  %define   st3  st(3)
  %define   st4  st(4)
  %define   st5  st(5)
  %define   st6  st(6)
  %define   st7  st(7)
%endif
