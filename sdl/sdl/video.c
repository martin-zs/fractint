#include <string.h>
#include <assert.h>
#include <stdio.h>
#include "port.h"
#include "prototyp.h"
/*
 * This file contains Unix versions of the routines in video.asm
 * Copyright 1992 Ken Shirriff
 */
/* The intent is to have non-system specific video routines here */

extern int startdisk (void);
extern int waitkeypressed (int);

extern int COLS;
extern int LINES;

extern BYTE pixel[48];

int fake_lut = 0;
int istruecolor = 0;
int daclearn = 0;
int dacnorm = 0;
int daccount = 0;
int ShadowColors;
void (*dotwrite) (int, int, U32);  /* write-a-dot routine */
BYTE (*dotread) (int, int);         /* read-a-dot routine */
void (*linewrite) (int, int, int, BYTE *); /* write-a-line routine */
void (*lineread) (int, int, int, BYTE *);  /* read-a-line routine */
int andcolor = 0;          /* "and" value used for color selection */
int diskflag = 0;          /* disk video active flag */

void (*swapsetup) (void) = NULL; /* setfortext/graphics setup routine */
int color_dark = 0;        /* darkest color in palette */
int color_bright = 0;      /* brightest color in palette */
int color_medium = 0;      /* nearest to medbright grey in palette
                              Zoom-Box values (2K x 2K screens max) */
int boxcolor = 0;          /* Zoom-Box color */
int gotrealdac = 0;        /* 1 if loaddac has a dacbox */
int rowcount = 0;          /* row-counter for decoder and out_line */
// int textaddr = 0xb800;  /* b800 for mode 3, b000 for mode 7 */
// NOTE (jonathan#1#): Next can be removed eventually since only one type is recognized.
int text_type = 0; /* always 0 */
int textrow = 0;   /* for putstring(-1,...) */
int textcol = 0;   /* for putstring(..,-1,...) */
int textrbase = 0; /* textrow is relative to this */
int textcbase = 0; /* textcol is relative to this */

void setfortext (void);
void setforgraphics (void);
void setvideomode (int);
void putstring (int, int, int, CHAR *);
void normaline (int, int, int, BYTE *);
void normalineread (int, int, int, BYTE *);

void nullwrite (int a, int b, U32 c)
{
}

BYTE nullread (int a, int b)
{
  return 0;
}

void setnullvideo (void)
{
  dotwrite = nullwrite;
  dotread = nullread;
}

/*
; ********************** Function setvideotext() ************************

;       Sets video to text mode, using setvideomode to do the work.
*/
void setvideotext (void)
{
  dotmode = 1;
  setvideomode (dotmode);
}

void loaddac (void)
{
  readvideopalette ();
}

/* Set video mode */
/*
; **************** Function setvideomode(dotmode) ****************
;       This function sets the (alphanumeric or graphic) video mode
;       of the monitor.   Called with the proper value of dotmode.
;       No returned values, as there is no particular standard to
;       adhere to in this case.  Dotmode is 1 for text and 2 for graphics.

*/
void setvideomode (int dotmode)
{
  if (diskflag)
    {
      enddisk ();
    }
  switch (dotmode)
    {
    case 1:   /* text */
// FIXME (jonathan#1#): Add code to setup text screen.
      starttext();
      setfortext();
      break;
    case 2:   /* video window */
      dotwrite = writevideo;
      dotread = readvideo;
      lineread = readvideoline;
      linewrite = writevideoline;
      startvideo ();
      setforgraphics ();
      break;
    default:
      printf ("Bad video mode %d\n", dotmode);
      exit (-1);
    }

  loaddac ();
  andcolor = colors - 1;
  boxcount = 0;
}


/*
; **************** Function getcolor(xdot, ydot) *******************

;       Return the color on the screen at the (xdot,ydot) point
*/
int getcolor (int xdot, int ydot)
{
  int x1, y1;
  x1 = xdot + sxoffs;
  y1 = ydot + syoffs;
  if (x1 < 0 || y1 < 0 || x1 >= sxdots || y1 >= sydots)
    return 0;
  return dotread (x1, y1);
}

/*
; ************** Function putcolor_a(xdot, ydot, color) *******************

;       write the color on the screen at the (xdot,ydot) point
*/
void putcolor_a (int xdot, int ydot, int color)
{
  if (istruecolor)
    dotwrite (xdot + sxoffs, ydot + syoffs, map_to_pixel((BYTE) color));
  else
    dotwrite (xdot + sxoffs, ydot + syoffs, (U32)color /* & andcolor */);
  /* assume andcolor is taken care of prior to this point */
}

/*
; **************** Function movecursor(row, col)  **********************

;       Move the cursor (called before printfs)
*/
void movecursor (int row, int col)
{
  if (row == -1)
    {
      row = textrow;
    }
  else
    {
      textrow = row;
    }
  if (col == -1)
    {
      col = textcol;
    }
  else
    {
      textcol = col;
    }
}

/*
; **************** Function keycursor(row, col)  **********************

;       Subroutine to wait cx ticks, or till keystroke pending
*/
int keycursor (int row, int col)
{
  movecursor (row, col);
  waitkeypressed (0);
  return getakey ();
}

/*
; **************** Function home()  ********************************

;       Home the cursor (called before printfs)
*/
void home (void)
{
  textrow = 0;
  textcol = 0;
}

/*
; ************* Function scrollup(toprow, botrow) ******************

;       Scroll the screen up (from toprow to botrow)
*/
void scrollup (int top, int bot)
{
// FIXME (jonathan#1#): Move the contents of text_screen and text_attr up 1 line, then blit all to screen.


}

/*
; *************** Function spindac(direction, rstep) ********************

;       Rotate the MCGA/VGA DAC in the (plus or minus) "direction"
;       in "rstep" increments - or, if "direction" is 0, just replace it.
*/
void spindac (int dir, int inc)
{
  int i, top;
  unsigned char tmp[3];
  unsigned char *dacbot;
  int len;

  if (colors < 16)
    return;
  if (!(gotrealdac || fake_lut))
    return;
  if (istruecolor)
    return;
  if (dir != 0 && rotate_lo < colors && rotate_lo < rotate_hi)
    {
      top = rotate_hi > colors ? colors - 1 : rotate_hi;
      dacbot = (unsigned char *) dacbox + 3 * rotate_lo;
      len = (top - rotate_lo) * 3 * sizeof (unsigned char);
      if (dir > 0)
        {
          for (i = 0; i < inc; i++)
            {
              bcopy (dacbot, tmp, 3 * sizeof (unsigned char));
              bcopy (dacbot + 3 * sizeof (unsigned char), dacbot, len);
              bcopy (tmp, dacbot + len, 3 * sizeof (unsigned char));
            }
        }
      else
        {
          for (i = 0; i < inc; i++)
            {
              bcopy (dacbot + len, tmp, 3 * sizeof (unsigned char));
              bcopy (dacbot, dacbot + 3 * sizeof (unsigned char), len);
              bcopy (tmp, dacbot, 3 * sizeof (unsigned char));
            }
        }
    }
  writevideopalette ();
  delay (colors - daccount - 1);
}

/*
; ---- Help (Video) Support
; ********* Functions setfortext() and setforgraphics() ************

;       setfortext() resets the video for text mode and saves graphics data
;       setforgraphics() restores the graphics mode and data
;       setclear() clears the screen after setfortext()
*/
void setfortext (void)
{
  setclear();
}

void setforgraphics (void)
{
  startvideo ();
  spindac (0, 1);
}

void dac_to_rgb(BYTE color, BYTE *red, BYTE *green, BYTE *blue)
{
  /* returns the rgb values corresponding to the color entry in dacbox */

  switch (truemode)
    {
    default:
    case 0:
      *red   = dacbox[color][0] << 2; /* red */
      *green = dacbox[color][1] << 2; /* green */
      *blue  = dacbox[color][2] << 2; /* blue */
      break;
    case 1:
      *red   = (realcoloriter >> 16) & 0xff; /* red */
      *green = (realcoloriter >> 8) & 0xff; /* green */
      *blue  = realcoloriter & 0xff; /* blue */
      break;
    case 2:
      *red   = (coloriter >> 16) & 0xff; /* red */
      *green = (coloriter >> 8) & 0xff; /* green */
      *blue  = coloriter & 0xff; /* blue */
      break;
    case 3:
      { /* experimental */
      U32 temp = (U32)((realcoloriter << 16) / maxit);
      *red   = (temp >> 16) & 0xff; /* red */
      *green = (temp >> 8) & 0xff; /* green */
      *blue  = temp & 0xff; /* blue */
      break;
      }
    }
}

/************** Function findfont(n) **************************/
/*    findfont(0) returns pointer to 8x8 font table if it can */
/*                find it, NULL otherwise; */
/*                nonzero parameter reserved for future use */
// NOTE (jonathan#1#): Need to determine how to implement this.
BYTE *findfont(int fontparm)
{
  return(NULL);
}

/*
; ******************** Zoombox functions **************************
*/

/*
 * The IBM method is that boxx[],boxy[] is a set of locations, and boxvalues
 * is the values in these locations.
 * Instead of using this box save/restore technique, we'll put the corners
 * in boxx[0],boxy[0],1,2,3 and then use xor.
 */
#if 0
void dispbox (void)
{
  if (boxcount)
    {
      drawline (boxx[0], boxy[0], boxx[1], boxy[1]);
      drawline (boxx[1], boxy[1], boxx[2], boxy[2]);
      drawline (boxx[2], boxy[2], boxx[3], boxy[3]);
      drawline (boxx[3], boxy[3], boxx[0], boxy[0]);
    }
}

void clearbox (void)
{
  dispbox ();
}

// NOTE next four routines are probably not needed
int CheckForTPlus (void)
{
  return 0;
}

/*
; Passing this routine 0 turns off shadow, nonzero turns it on.
*/
int ShadowVideo (int on)
{
  return 0;
}

int SetupShadowVideo (void)
{
  return 0;
}
#endif
/*
; **************** internal Read/Write-a-line routines *********************
;
;       These routines are called by out_line(), put_line() and get_line().
*/

void normaline (int y, int x, int lastx, BYTE *pixels)
{
  int i, width;
  width = lastx - x + 1;
  for (i = 0; i < width; i++)
    {
      dotwrite (x + i, y, (U32)pixels[i]);
    }
}

void normalineread (int y, int x, int lastx, BYTE *pixels)
{
  int i, width;
  width = lastx - x + 1;
  for (i = 0; i < width; i++)
    {
      pixels[i] = dotread (x + i, y);
    }
}

/*
; *************** Function find_special_colors ********************

;       Find the darkest and brightest colors in palette, and a medium
;       color which is reasonably bright and reasonably grey.
*/
void find_special_colors (void)
{
  int maxb = 0;
  int minb = 9999;
  int med = 0;
  int maxgun, mingun;
  int brt;
  int i;

  color_dark = 0;
  color_medium = 7;
  color_bright = 15;

  if (colors == 2)
    {
      color_medium = 1;
      color_bright = 1;
      return;
    }

  if (!(gotrealdac || fake_lut))
    return;

  for (i = 0; i < colors; i++)
    {
      brt = (int) dacbox[i][0] + (int) dacbox[i][1] + (int) dacbox[i][2];
      if (brt > maxb)
        {
          maxb = brt;
          color_bright = i;
        }
      if (brt < minb)
        {
          minb = brt;
          color_dark = i;
        }
      if (brt < 150 && brt > 80)
        {
          maxgun = mingun = (int) dacbox[i][0];
          if ((int) dacbox[i][1] > (int) dacbox[i][0])
            {
              maxgun = (int) dacbox[i][1];
            }
          else
            {
              mingun = (int) dacbox[i][1];
            }
          if ((int) dacbox[i][2] > maxgun)
            {
              maxgun = (int) dacbox[i][2];
            }
          if ((int) dacbox[i][2] < mingun)
            {
              mingun = (int) dacbox[i][2];
            }
          if (brt - (maxgun - mingun) / 2 > med)
            {
              color_medium = i;
              med = brt - (maxgun - mingun) / 2;
            }
        }
    }
}

/*
; *************** Functions get_a_char, put_a_char ********************

;       Get and put character and attribute at cursor
;       Hi nybble=character, low nybble attribute. Text mode only
*/
char get_a_char (void)
{
  return (char) getakey ();
}

void put_a_char (int ch)
{
// NOTE (jonathan#1#): Apparently this isn't used
}

/*
; ***Function get_line(int row,int startcol,int stopcol, unsigned char *pixels)

;       This routine is a 'line' analog of 'getcolor()', and gets a segment
;       of a line from the screen and stores it in pixels[] at one byte per
;       pixel
;       Called by the GIF decoder
*/

void get_line (int row, int startcol, int stopcol, BYTE *pixels)
{
  if (startcol + sxoffs >= sxdots || row + syoffs >= sydots)
    return;
  lineread (row + syoffs, startcol + sxoffs, stopcol + sxoffs, pixels);
}

/*
; ***Function put_line(int row,int startcol,int stopcol, unsigned char *pixels)

;       This routine is a 'line' analog of 'putcolor()', and puts a segment
;       of a line from the screen and stores it in pixels[] at one byte per
;       pixel
;       Called by the GIF decoder
*/

void put_line (int row, int startcol, int stopcol, BYTE *pixels)
{
  if (startcol + sxoffs >= sxdots || row + syoffs > sydots)
    return;
  linewrite (row + syoffs, startcol + sxoffs, stopcol + sxoffs, pixels);
}

/*
; ***************Function out_line(pixels,linelen) *********************

;       This routine is a 'line' analog of 'putcolor()', and sends an
;       entire line of pixels to the screen (0 <= xdot < xdots) at a clip
;       Called by the GIF decoder
*/
int out_line (BYTE *pixels, int linelen)
{
  if (rowcount + syoffs >= sydots)
    return 0;
  linewrite (rowcount + syoffs, sxoffs, linelen + sxoffs - 1, pixels);
  rowcount++;
  return 0;
}

/*
; far move routine for savegraphics/restoregraphics
*/
void movewords (int len, BYTE *fromptr, BYTE *toptr)
{
  bcopy (fromptr, toptr, len);
}

void swapnormread (void)
{
}
void swapnormwrite (void)
{
}

