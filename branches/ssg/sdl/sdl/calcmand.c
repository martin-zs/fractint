/* calcmand.c
 * This file contains routines to replace calcmand.asm.
 *
 * This file Copyright 1991 Ken Shirriff.  It may be used according to the
 * fractint license conditions, blah blah blah.
 */

#include "port.h"
#include "prototyp.h"
#include "fractype.h"

unsigned long savedmask;
long linitx, linity;

long calcmandasm(void)
{
#if 0
  lnew.x  = ltempsqrx - ltempsqry + longparm->x;
  lnew.y = multiply(lold.x, lold.y, bitshiftless1) + longparm->y;
  return(longbailout());
#else
  printf("Warning: called calcmandasm\n");
  return(0);
#endif
}

#if 0    /* not used */
code16bit() {}
checkperiod() {}
code32bit() {}
#endif
