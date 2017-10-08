#include <string.h>
#include <assert.h>
#include <stdio.h>
#include "port.h"
#include "prototyp.h"
#include "helpdefs.h"
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
extern int text_attr[TEXT_HEIGHT][TEXT_WIDTH];
extern char text_screen[TEXT_HEIGHT][TEXT_WIDTH];

int fake_lut = 1;
int istruecolor = 0;
int daclearn = 0;
int dacnorm = 0;
int daccount = 128;
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
// NOTE (jonathan#1#): Next can be removed eventually since only one type is recognized.
int text_type = 2; /* always 2 */
int textrow = 0;   /* for putstring(-1,...) */
int textcol = 0;   /* for putstring(..,-1,...) */
int textrbase = 0; /* textrow is relative to this */
int textcbase = 0; /* textcol is relative to this */

struct videoinfo videoentry;

void setfortext (void);
void setforgraphics (void);
int  setvideomode (int);
void putstring (int, int, int, CHAR *);
void normaline (int, int, int, BYTE *);
void normalineread (int, int, int, BYTE *);

/*
    name of mode              | Comments
   key | mode | x | y | clr
*/

struct videoinfo videotable[MAXVIDEOTABLE] = {
  {"16 color mode            ", "                         ",
   F2, 4, 640, 350, 16},
  {"256 color mode           ", "                         ",
   F3, 8, 320, 200, 256},
  {"16 color mode            ", "                         ",
   F4, 4, 640, 480, 16},
  {"4 color mode             ", "                         ",
   F5, 2, 320, 200, 4},
  {"2 color mode             ", "                         ",
   F6, 1, 640, 200, 2},
  {"2 color mode             ", "                         ",
   F7, 1, 640, 350, 2},
  {"2 color mode             ", "                         ",
   F8, 1, 640, 480, 2},
  {"16 color mode            ", "                         ",
   F9, 4, 320, 200, 16},
  {"256 color mode           ", "                         ",
   F10, 8, 320, 400, 256},
  {"256 color mode           ", "                         ",
   SF1, 8, 360, 480, 256},
  {"16 color mode            ", "                         ",
   SF2, 4, 800, 600, 16},
  {"16 color mode            ", "                         ",
   SF3, 4, 1024, 768, 16},
  {"256 color mode           ", "                         ",
   SF4, 8, 640, 400, 256},
  {"256 color mode           ", "                         ",
   SF5, 8, 640, 480, 256},
  {"256 color mode           ", "                         ",
   SF6, 8, 800, 600, 256},
  {"256 color mode           ", "                         ",
   SF7, 8, 1024, 768, 256},
  {"256 color mode           ", "                         ",
   SF8, 8, 1280, 1024, 256},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
  {"unused  mode             ", "                         ",
   0, 0, 0, 0, 0},
};

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
  setvideomode (3);  /* dotmode = 3 for text, but don't change screen */
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
;       Returns 1 on success, 0 on fail (disk video only)
;       Dotmode is 3 for text and 0,8,16,24,32 for graphics.

*/
int setvideomode (int dotmode)
{
  if (diskflag && dotmode != 11)
    {
      enddisk ();
    }
  switch (dotmode)
    {
    case 3:   /* text */
      ResizeScreen(2);
      starttext();
      setfortext();
      break;
    case 0:   /* video window */
    case 8:
    case 15:   /* shouldn't be used */
    case 16:
    case 24:
    case 32:
      dotwrite = writevideo;
      dotread = readvideo;
      lineread = readvideoline;
      linewrite = writevideoline;
      ResizeScreen(1);
      setforgraphics ();
      break;
    case 11:
      starttext();
      setfortext();
      if (startdisk ()) /* 0 on success, 1 on failure */
        return(0);
      dotwrite = writedisk;
      dotread = readdisk;
      lineread = normalineread;
      linewrite = normaline;
      break;
    default:
      printf ("Bad video mode %d\n", dotmode);
      exit (-1);
    }

  loaddac ();
  andcolor = videoentry.colors - 1;
  boxcount = 0;
  return (1);
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
    dotwrite (xdot + sxoffs, ydot + syoffs, (U32)color);
}

/*
; **************** Function movecursor(row, col)  **********************

;       Move the cursor
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
  if (textrow == 25 && textcol == 80)
  {
      text_attr[textrow][textcol] = 0;
      text_screen[textrow][textcol] = ' ';
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
  if (istruecolor && truemode)
    return;
  if (dotmode == 11)
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
  if (dir != 0)
    delay ((colors - daccount) * 8);
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
  stackscreen();
  setclear();
}

void setforgraphics (void)
{
  unstackscreen();
  startvideo ();
  spindac (0, 1);
}

void dac_to_rgb(BYTE color, BYTE *red, BYTE *green, BYTE *blue)
{
  /* truemode=0 returns the rgb values corresponding to the color entry in dacbox */

  switch (truemode)
    {
    default:
    case 0:
#if BYTE_ORDER == BIG_ENDIAN
      *red   = dacbox[color][2] << 2; /* red */
      *green = dacbox[color][1] << 2; /* green */
      *blue  = dacbox[color][0] << 2; /* blue */
#else
      *red   = dacbox[color][0] << 2; /* red */
      *green = dacbox[color][1] << 2; /* green */
      *blue  = dacbox[color][2] << 2; /* blue */
#endif
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
    {
      /* experimental */
      U32 temp = (U32)((realcoloriter << 16) / maxit);
      *red   = (temp >> 16) & 0xff; /* red */
      *green = (temp >> 8) & 0xff; /* green */
      *blue  = temp & 0xff; /* blue */
      break;
    }
    }
}

static unsigned char *fontPtr = NULL;

/************** Function findfont(n) **************************/
/*    findfont(0) returns pointer to 8x8 font table */
/*                nonzero parameter reserved for future use */
BYTE *findfont(int fontparm)
{
static unsigned char values[128*8] = {
0x00, 0x22, 0x00, 0x22, 0x00, 0x22, 0x00, 0x2a,
0x00, 0x08, 0x1c, 0x3e, 0x1c, 0x08, 0x00, 0x00,
0x15, 0x2a, 0x15, 0x2a, 0x15, 0x2a, 0x15, 0x2a,
0x28, 0x38, 0x28, 0x28, 0x0e, 0x04, 0x04, 0x04,
0x20, 0x30, 0x20, 0x2e, 0x08, 0x0c, 0x08, 0x08,
0x20, 0x20, 0x18, 0x0c, 0x0a, 0x0c, 0x0a, 0x0a,
0x20, 0x20, 0x38, 0x0e, 0x08, 0x0c, 0x08, 0x08,
0x12, 0x12, 0x0c, 0x00, 0x00, 0x00, 0x00, 0x00,
0x08, 0x08, 0x3e, 0x08, 0x08, 0x00, 0x3e, 0x00,
0x34, 0x2c, 0x24, 0x08, 0x08, 0x08, 0x08, 0x0e,
0x28, 0x28, 0x10, 0x10, 0x0e, 0x04, 0x04, 0x04,
0x08, 0x08, 0x08, 0x38, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x38, 0x08, 0x08, 0x08, 0x08,
0x00, 0x00, 0x00, 0x0f, 0x08, 0x08, 0x08, 0x08,
0x08, 0x08, 0x08, 0x0f, 0x00, 0x00, 0x00, 0x00,
0x08, 0x08, 0x08, 0x3f, 0x08, 0x08, 0x08, 0x08,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x3f, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x3f, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3f, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x08, 0x08, 0x08, 0x0f, 0x08, 0x08, 0x08, 0x08,
0x08, 0x08, 0x08, 0x38, 0x08, 0x08, 0x08, 0x08,
0x08, 0x08, 0x08, 0x3f, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x3f, 0x08, 0x08, 0x08, 0x08,
0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
0x06, 0x18, 0x20, 0x18, 0x06, 0x00, 0x3e, 0x00,
0x30, 0x0c, 0x02, 0x0c, 0x30, 0x00, 0x3e, 0x00,
0x00, 0x00, 0x3e, 0x14, 0x14, 0x14, 0x14, 0x14,
0x00, 0x00, 0x02, 0x3e, 0x08, 0x3e, 0x20, 0x00,
0x12, 0x10, 0x10, 0x38, 0x10, 0x10, 0x12, 0x2c,
0x00, 0x00, 0x00, 0x0c, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x00, 0x08,
0x14, 0x14, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x14, 0x14, 0x3e, 0x14, 0x3e, 0x14, 0x14, 0x00,
0x1e, 0x28, 0x28, 0x1c, 0x0a, 0x0a, 0x3c, 0x08,
0x2a, 0x14, 0x04, 0x08, 0x10, 0x14, 0x2a, 0x24,
0x10, 0x28, 0x28, 0x10, 0x28, 0x26, 0x24, 0x1a,
0x08, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x08, 0x10, 0x10, 0x10, 0x10, 0x10, 0x08, 0x08,
0x08, 0x04, 0x04, 0x04, 0x04, 0x04, 0x08, 0x08,
0x2a, 0x1c, 0x2a, 0x08, 0x00, 0x00, 0x00, 0x00,
0x00, 0x08, 0x08, 0x3e, 0x08, 0x08, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0c, 0x08,
0x00, 0x00, 0x00, 0x3e, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x1c,
0x02, 0x04, 0x04, 0x08, 0x10, 0x10, 0x20, 0x20,
0x14, 0x22, 0x22, 0x22, 0x22, 0x22, 0x14, 0x08,
0x18, 0x28, 0x08, 0x08, 0x08, 0x08, 0x08, 0x3e,
0x22, 0x22, 0x02, 0x04, 0x08, 0x10, 0x20, 0x3e,
0x02, 0x04, 0x08, 0x1c, 0x02, 0x02, 0x22, 0x1c,
0x04, 0x0c, 0x14, 0x14, 0x24, 0x3e, 0x04, 0x04,
0x20, 0x20, 0x2c, 0x32, 0x02, 0x02, 0x22, 0x1c,
0x22, 0x20, 0x20, 0x3c, 0x22, 0x22, 0x22, 0x1c,
0x02, 0x04, 0x04, 0x08, 0x08, 0x10, 0x10, 0x10,
0x22, 0x22, 0x22, 0x1c, 0x22, 0x22, 0x22, 0x1c,
0x22, 0x22, 0x22, 0x1e, 0x02, 0x02, 0x22, 0x1c,
0x00, 0x08, 0x1c, 0x08, 0x00, 0x00, 0x08, 0x1c,
0x00, 0x08, 0x1c, 0x08, 0x00, 0x00, 0x0c, 0x08,
0x04, 0x08, 0x10, 0x20, 0x10, 0x08, 0x04, 0x02,
0x00, 0x00, 0x3e, 0x00, 0x00, 0x3e, 0x00, 0x00,
0x10, 0x08, 0x04, 0x02, 0x04, 0x08, 0x10, 0x20,
0x22, 0x22, 0x02, 0x04, 0x08, 0x08, 0x00, 0x08,
0x22, 0x22, 0x26, 0x2a, 0x2a, 0x2c, 0x20, 0x1e,
0x14, 0x22, 0x22, 0x22, 0x3e, 0x22, 0x22, 0x22,
0x12, 0x12, 0x12, 0x1c, 0x12, 0x12, 0x12, 0x3c,
0x22, 0x20, 0x20, 0x20, 0x20, 0x20, 0x22, 0x1c,
0x12, 0x12, 0x12, 0x12, 0x12, 0x12, 0x12, 0x3c,
0x20, 0x20, 0x20, 0x3c, 0x20, 0x20, 0x20, 0x3e,
0x20, 0x20, 0x20, 0x3c, 0x20, 0x20, 0x20, 0x20,
0x22, 0x20, 0x20, 0x20, 0x26, 0x22, 0x22, 0x1c,
0x22, 0x22, 0x22, 0x3e, 0x22, 0x22, 0x22, 0x22,
0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x1c,
0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x24, 0x18,
0x22, 0x24, 0x28, 0x30, 0x28, 0x24, 0x22, 0x22,
0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x20, 0x3e,
0x22, 0x36, 0x2a, 0x2a, 0x22, 0x22, 0x22, 0x22,
0x32, 0x32, 0x2a, 0x2a, 0x26, 0x26, 0x22, 0x22,
0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x1c,
0x22, 0x22, 0x22, 0x3c, 0x20, 0x20, 0x20, 0x20,
0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x2a, 0x1c,
0x22, 0x22, 0x22, 0x3c, 0x28, 0x24, 0x22, 0x22,
0x22, 0x20, 0x20, 0x1c, 0x02, 0x02, 0x22, 0x1c,
0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x22, 0x1c,
0x22, 0x22, 0x22, 0x14, 0x14, 0x14, 0x08, 0x08,
0x22, 0x22, 0x22, 0x2a, 0x2a, 0x2a, 0x2a, 0x14,
0x22, 0x14, 0x14, 0x08, 0x14, 0x14, 0x22, 0x22,
0x22, 0x14, 0x14, 0x08, 0x08, 0x08, 0x08, 0x08,
0x02, 0x04, 0x04, 0x08, 0x10, 0x10, 0x20, 0x3e,
0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x10,
0x20, 0x10, 0x10, 0x08, 0x04, 0x04, 0x02, 0x02,
0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04,
0x14, 0x22, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x1c, 0x02, 0x1e, 0x22, 0x26, 0x1a,
0x20, 0x20, 0x3c, 0x22, 0x22, 0x22, 0x22, 0x3c,
0x00, 0x00, 0x1c, 0x22, 0x20, 0x20, 0x22, 0x1c,
0x02, 0x02, 0x1e, 0x22, 0x22, 0x22, 0x22, 0x1e,
0x00, 0x00, 0x1c, 0x22, 0x3e, 0x20, 0x22, 0x1c,
0x12, 0x10, 0x10, 0x3c, 0x10, 0x10, 0x10, 0x10,
0x00, 0x00, 0x1c, 0x22, 0x22, 0x22, 0x1e, 0x02,
0x20, 0x20, 0x2c, 0x32, 0x22, 0x22, 0x22, 0x22,
0x08, 0x00, 0x18, 0x08, 0x08, 0x08, 0x08, 0x1c,
0x04, 0x00, 0x0c, 0x04, 0x04, 0x04, 0x04, 0x24,
0x20, 0x20, 0x24, 0x28, 0x30, 0x28, 0x24, 0x22,
0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x1c,
0x00, 0x00, 0x34, 0x2a, 0x2a, 0x2a, 0x2a, 0x22,
0x00, 0x00, 0x2c, 0x32, 0x22, 0x22, 0x22, 0x22,
0x00, 0x00, 0x1c, 0x22, 0x22, 0x22, 0x22, 0x1c,
0x00, 0x00, 0x3c, 0x22, 0x22, 0x22, 0x3c, 0x20,
0x00, 0x00, 0x1e, 0x22, 0x22, 0x22, 0x1e, 0x02,
0x00, 0x00, 0x2c, 0x32, 0x20, 0x20, 0x20, 0x20,
0x00, 0x00, 0x1c, 0x22, 0x18, 0x04, 0x22, 0x1c,
0x10, 0x10, 0x3c, 0x10, 0x10, 0x10, 0x12, 0x0c,
0x00, 0x00, 0x22, 0x22, 0x22, 0x22, 0x26, 0x1a,
0x00, 0x00, 0x22, 0x22, 0x22, 0x14, 0x14, 0x08,
0x00, 0x00, 0x22, 0x22, 0x2a, 0x2a, 0x2a, 0x14,
0x00, 0x00, 0x22, 0x14, 0x08, 0x08, 0x14, 0x22,
0x00, 0x00, 0x22, 0x22, 0x22, 0x26, 0x1a, 0x02,
0x00, 0x00, 0x3e, 0x04, 0x08, 0x10, 0x20, 0x3e,
0x08, 0x08, 0x08, 0x30, 0x08, 0x08, 0x08, 0x08,
0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08, 0x08,
0x08, 0x08, 0x08, 0x06, 0x08, 0x08, 0x08, 0x08,
0x2a, 0x24, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x22, 0x00, 0x22, 0x00, 0x22, 0x00, 0x2a};
  fontPtr = values;
  return (fontPtr);
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

void put_a_char (char ch)
{
// NOTE (jonathan#1#): Used in slideshw.c!!!
  int attr = text_attr[textrow][textcol];

  putstring (-1, -1, attr, &ch);
}

void blink_cursor (void)
{
  int attr;

  attr = text_attr[textrow][textcol];
  if (attr & BLINK)
    attr -= BLINK;
  else
    attr += BLINK;
  text_attr[textrow][textcol] = attr;
  outtext(textrow, textcol, textcol+1);
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
    loadfdos.c - subroutine of loadfile.c (read_overlay) which sets
                 up video (mode, screen size).
    This module is linked as an overlay, should only be called from loadfile.c

    This code was split to a separate module to isolate the DOS only aspects
    of loading an image.  get_video_mode should return with:
      return code 0 for ok, -1 for error or cancelled by user
      video parameters setup for the mainline, in the dos case this means
        setting initmode to video mode, based on this fractint.c will set up
        for and call setvideomode
      set viewwindow on if file going to be loaded into a view smaller than
        physical screen, in this case also set viewreduction, viewxdots,
        viewydots, and finalaspectratio
      set skipxdots and skipydots, to 0 if all pixels are to be loaded,
        to 1 for every 2nd pixel, 2 for every 3rd, etc

    In WinFract, at least initially, get_video_mode can do just the
    following:
      set overall image x & y dimensions (sxdots and sydots) to filexdots
        and fileydots (note that filecolors is the number of colors in the
        gif, not sure if that is of any use...)
      if current window smaller than new sxdots and sydots, use scroll bars,
        if larger perhaps reduce the window size? whatever
      set viewwindow to 0 (no need? it always is for now in windows vsn?)
      set finalaspectratio to .75 (ditto?)
      set skipxdots and skipydots to 0
      return 0

*/

/* routines in this module      */

static int    vidcompare(VOIDCONSTPTR ,VOIDCONSTPTR );
static void   format_item(int,char *);
static int    check_modekey(int,int);
static void   format_vid_inf(int i,char *err,char *buf);
static double vid_aspect(int tryxdots,int tryydots);

struct vidinf {
   int entnum;     /* videoentry subscript */
   unsigned flags; /* flags for sort's compare, defined below */
   };
/* defines for flags; done this way instead of bit union to ensure ordering;
   these bits represent the sort sequence for video mode list */
#define VI_EXACT 0x8000 /* unless the one and only exact match */
#define VI_NOKEY   512  /* if no function key assigned */
#define VI_DISK1   256  /* disk video and size not exact */
#define VI_SSMALL  128  /* screen smaller than file's screen */
#define VI_SBIG     64  /* screen bigger than file's screen */
#define VI_VSMALL   32  /* screen smaller than file's view */
#define VI_VBIG     16  /* screen bigger than file's view */
#define VI_CSMALL    8  /* mode has too few colors */
#define VI_CBIG      4  /* mode has excess colors */
#define VI_DISK2     2  /* disk video */
#define VI_ASPECT    1  /* aspect ratio bad */

static int vidcompare(VOIDCONSTPTR p1,VOIDCONSTPTR p2)
{
   struct vidinf CONST *ptr1,*ptr2;
   ptr1 = (struct vidinf CONST *)p1;
   ptr2 = (struct vidinf CONST *)p2;
   if (ptr1->flags < ptr2->flags) return(-1);
   if (ptr1->flags > ptr2->flags) return(1);
   if (vidtbl[ptr1->entnum].keynum < vidtbl[ptr2->entnum].keynum) return(-1);
   if (vidtbl[ptr1->entnum].keynum > vidtbl[ptr2->entnum].keynum) return(1);
   if (ptr1->entnum < ptr2->entnum) return(-1);
   return(1);
}

static void format_vid_inf(int i,char *err,char *buf)
{
   char kname[5];
   memcpy((char *)&videoentry,(char *)&vidtbl[i],
              sizeof(videoentry));
   vidmode_keyname(videoentry.keynum,kname);
   sprintf(buf,"%-5s %-25s %-4s %5d %5d %3d %-25s",  /* 78 chars */
           kname, videoentry.name, err,
           videoentry.xdots, videoentry.ydots,
           videoentry.colors, videoentry.comment);
   videoentry.xdots = 0; /* so tab_display knows to display nothing */
}

static double vid_aspect(int tryxdots,int tryydots)
{  /* calc resulting aspect ratio for specified dots in current mode */
   return (double)tryydots / (double)tryxdots
        * (double)videoentry.xdots / (double)videoentry.ydots
        * screenaspect;
   }

static struct vidinf *vidptr;

int get_video_mode(struct fractal_info *info,struct ext_blk_3 *blk_3_info)
{
   static FCODE o_hdg2[]={"key...name......................err...xdot..ydot.clr.comment.................."};
   static FCODE o_warning[]={"\nWARNING: non-standard aspect ratio; loading will change your <v>iew settings"};
   static FCODE o_select_msg[]={"\
Select a video mode.  Use the cursor keypad to move the pointer.\n\
Press ENTER for selected mode, or use a video mode function key.\n\
Press F1 for help, "};
   char *hdg2, *warning, *select_msg, *ptr;
   struct vidinf vid[MAXVIDEOMODES];
   int i,j;
   int gotrealmode;
   double ftemp,ftemp2;
   unsigned tmpflags;

   int tmpxdots,tmpydots;
   float tmpreduce;
   char *nameptr;
   int  *attributes;
   int oldhelpmode;
   VIDEOINFO *vident;
   char buff[2000];

   ptr = buff;
   hdg2 = ptr;
   ptr += sizeof(o_hdg2);
   warning = ptr;
   ptr += sizeof(o_warning);
   select_msg = ptr;
   strcpy(hdg2,o_hdg2);
   strcpy(warning,o_warning);
   strcpy(select_msg,o_select_msg);

   initmode = -1;
   load_fractint_cfg(0); /* get fractint.cfg into *vidtbl (== extraseg) */

#if 0
   /* try to change any VESA entries to fit the loaded image size */
   if (virtual && video_vram && initmode == -1) {
      unsigned long vram = (unsigned long)video_vram << 16,
                    need = (unsigned long)info->xdots * info->ydots;
      if (need <= vram) {
         char over[25]; /* overwrite comments with original resolutions */
         int bppx;      /* bytesperpixel multiplier */
         for (i = 0; i < vidtbllen; ++i) {
            vident = &vidtbl[i];
            if (vident->dotmode%100 == 28 && vident->colors >= 256
               && (info->xdots > vident->xdots || info->ydots > vident->ydots)
               && vram >= (unsigned long)
                  (info->xdots < vident->xdots ? vident->xdots : info->xdots)
                  * (info->ydots < vident->ydots ? vident->ydots : info->ydots)
                  * ((bppx = vident->dotmode/1000) < 2 ? ++bppx : bppx)) {

               sprintf(over,"<-VIRTUAL! at %4u x %4u",vident->xdots,vident->ydots);
               far_strcpy((char far *)vident->comment,(char far *)over);

               if (info->xdots > vident->xdots)
                  vident->xdots = info->xdots;
               if (info->ydots > vident->ydots)
                  vident->ydots = info->ydots;
             }  /* change entry to force VESA virtual scanline setup */
         }
      }
   }
#endif

   /* try to find exact match for vid mode */
   for (i = 0; i < vidtbllen; ++i) {
      vident = &vidtbl[i];
      if (info->xdots == vident->xdots && info->ydots == vident->ydots
        && filecolors == vident->colors
        && info->dotmode%100 == vident->dotmode%100) {
         initmode = i;
         break;
         }
      }

   /* exit in makepar mode if no exact match of video mode in file */
   if(*s_makepar == '\0' && initmode == -1) {
      return(0);
      }

   if (initmode == -1) /* try to find very good match for vid mode */
      for (i = 0; i < vidtbllen; ++i) {
         vident = &vidtbl[i];
         if (info->xdots == vident->xdots && info->ydots == vident->ydots
           && filecolors == vident->colors) {
            initmode = i;
            break;
            }
         }

   /* setup table entry for each vid mode, flagged for how well it matches */
   for (i = 0; i < vidtbllen; ++i) {
      memcpy((char *)&videoentry,(char *)&vidtbl[i],
                 sizeof(videoentry));
      tmpflags = VI_EXACT;
      if (videoentry.keynum == 0)
         tmpflags |= VI_NOKEY;
      if (info->xdots > videoentry.xdots || info->ydots > videoentry.ydots)
         tmpflags |= VI_SSMALL;
      else if (info->xdots < videoentry.xdots || info->ydots < videoentry.ydots)
         tmpflags |= VI_SBIG;
      if (filexdots > videoentry.xdots || fileydots > videoentry.ydots)
         tmpflags |= VI_VSMALL;
      else if (filexdots < videoentry.xdots || fileydots < videoentry.ydots)
         tmpflags |= VI_VBIG;
      if (filecolors > videoentry.colors)
         tmpflags |= VI_CSMALL;
      if (filecolors < videoentry.colors)
         tmpflags |= VI_CBIG;
      if (i == initmode)
         tmpflags -= VI_EXACT;
      if (videoentry.dotmode%100 == 11) {
         tmpflags |= VI_DISK2;
         if ((tmpflags & (VI_SBIG+VI_SSMALL+VI_VBIG+VI_VSMALL)) != 0)
            tmpflags |= VI_DISK1;
         }
      if (fileaspectratio != 0 && videoentry.dotmode%100 != 11
        && (tmpflags & VI_VSMALL) == 0) {
         ftemp = vid_aspect(filexdots,fileydots);
         if ( ftemp < fileaspectratio * 0.98
           || ftemp > fileaspectratio * 1.02)
            tmpflags |= VI_ASPECT;
         }
      vid[i].entnum = i;
      vid[i].flags  = tmpflags;
      }

if (fastrestore  && !askvideo)
   initmode = adapter;

#if 1
   gotrealmode = 0;
   if ((initmode < 0 || (askvideo && !initbatch)) && *s_makepar != '\0') {
      /* no exact match or (askvideo=yes and batch=no), and not
        in makepar mode, talk to user */

      qsort(vid,vidtbllen,sizeof(vid[0]),vidcompare); /* sort modes */

      attributes = (int *)&dstack[1000];
      for (i = 0; i < vidtbllen; ++i)
         attributes[i] = 1;
      vidptr = &vid[0]; /* for format_item */

      /* format heading */
      if (info->info_id[0] == 'G')
         strcpy(temp1,"      Non-fractal GIF");
      else {
         nameptr = curfractalspecific->name;
         if (*nameptr == '*') ++nameptr;
          if (display3d) nameptr = "3D Transform";
         if ((!strcmp(nameptr,"formula")) ||
             (!strcmp(nameptr,"lsystem")) ||
             (!strncmp(nameptr,"ifs",3))) /* for ifs and ifs3d */
               sprintf(temp1,"Type: %s -> %s",nameptr,blk_3_info->form_name);
         else
           sprintf(temp1,"Type: %s",nameptr);
         }
      sprintf((char *)dstack,"File: %-44s  %d x %d x %d\n%-52s",
             readname,filexdots,fileydots,filecolors,temp1);
      if (info->info_id[0] != 'G') {
         sprintf(temp1,"v%d.%01d",save_release/100,(save_release%100)/10);
         if (save_release%100) {
            i = strlen(temp1);
            temp1[i] = (char)((save_release%10) + '0');
            temp1[i+1] = 0;
            }
         if (save_system == 0 && save_release <= 1410)
            strcat(temp1," or earlier");
         strcat((char *)dstack,temp1);
         }
      strcat((char *)dstack,"\n");
      if (info->info_id[0] != 'G' && save_system == 0) {
         if (initmode < 0)
            strcat((char *)dstack,"Saved in unknown video mode.");
         else {
            format_vid_inf(initmode,"",temp1);
            strcat((char *)dstack,temp1);
            }
        }
      if (fileaspectratio != 0 && fileaspectratio != screenaspect)
         strcat((char *)dstack,warning);
      strcat((char *)dstack,"\n");
      /* set up instructions */
      strcpy(temp1,select_msg);
      if (info->info_id[0] != 'G')
         strcat(temp1,"TAB for fractal information, ");
      strcat(temp1,"ESCAPE to back out.");

      oldhelpmode = helpmode;
      helpmode = HELPLOADFILE;
      i = fullscreen_choice(0,(char *)dstack,hdg2,temp1,vidtbllen,NULL,attributes,
                             1,13,78,0,format_item,NULL,NULL,check_modekey);
      helpmode = oldhelpmode;
      if (i == -1) {
         return(-1);
         }
      if (i < 0) { /* returned -100 - videotable entry number */
         initmode = -100 - i;
         gotrealmode = 1;
         }
      else
         initmode = vid[i].entnum;
      }
#else
      initmode = 0;
      j = vidtbl[0].keynum;
      gotrealmode = 0;
#endif

   if (gotrealmode == 0) { /* translate from temp table to permanent */
      if ((j = vidtbl[i=initmode].keynum) != 0) {
         for (initmode = 0; initmode < MAXVIDEOTABLE-1; ++initmode)
            if (videotable[initmode].keynum == j) break;
         if (initmode >= MAXVIDEOTABLE-1) j = 0;
         }
      if (j == 0) /* mode has no key, add to reserved slot at end */
         memcpy((char *)&videotable[initmode=MAXVIDEOTABLE-1],
                    (char *)&vidtbl[i],sizeof(*vidtbl));
      }

   /* ok, we're going to return with a video mode */

   memcpy((char *)&videoentry,(char *)&videotable[initmode],
              sizeof(videoentry));


   if (viewwindow &&
      filexdots == videoentry.xdots && fileydots == videoentry.ydots) {
      /* pull image into a view window */
      if (calc_status != 4) /* if not complete */
          calc_status = 0;  /* can't resume anyway */
      if (viewxdots) {
         viewreduction = videoentry.xdots / viewxdots;
         viewxdots = viewydots = 0; /* easier to use auto reduction */
      }
      viewreduction = (float)((int)(viewreduction + 0.5)); /* need integer value */
      skipxdots = skipydots = (short)(viewreduction - 1);
      return(0);
   }

   skipxdots = skipydots = 0; /* set for no reduction */
   if (videoentry.xdots < filexdots || videoentry.ydots < fileydots) {
      /* set up to load only every nth pixel to make image fit */
      if (calc_status != 4) /* if not complete */
          calc_status = 0;  /* can't resume anyway */
      skipxdots = skipydots = 1;
      while (skipxdots * videoentry.xdots < filexdots) ++skipxdots;
      while (skipydots * videoentry.ydots < fileydots) ++skipydots;
      i = j = 0;
      for(;;) {
         tmpxdots = (filexdots + skipxdots - 1) / skipxdots;
         tmpydots = (fileydots + skipydots - 1) / skipydots;
         if (fileaspectratio == 0 || videoentry.dotmode%100 == 11)
            break;
         /* reduce further if that improves aspect */
         if ((ftemp = vid_aspect(tmpxdots,tmpydots)) > fileaspectratio) {
            if (j) break; /* already reduced x, don't reduce y */
            ftemp2 = vid_aspect(tmpxdots,(fileydots+skipydots)/(skipydots+1));
            if (ftemp2 < fileaspectratio
              && ftemp/fileaspectratio *0.9 <= fileaspectratio/ftemp2)
               break; /* further y reduction is worse */
            ++skipydots;
            ++i;
            }
         else {
            if (i) break; /* already reduced y, don't reduce x */
            ftemp2 = vid_aspect((filexdots+skipxdots)/(skipxdots+1),tmpydots);
            if (ftemp2 > fileaspectratio
              && fileaspectratio/ftemp *0.9 <= ftemp2/fileaspectratio)
               break; /* further x reduction is worse */
            ++skipxdots;
            ++j;
            }
         }
      filexdots = tmpxdots;
      fileydots = tmpydots;
      --skipxdots;
      --skipydots;
      }

   if ((finalaspectratio = fileaspectratio) == 0) /* assume display correct */
      finalaspectratio = (float)vid_aspect(filexdots,fileydots);
   if (finalaspectratio >= screenaspect-0.02
     && finalaspectratio <= screenaspect+0.02)
      finalaspectratio = screenaspect;
   i = (int)(finalaspectratio * 1000.0 + 0.5);
   finalaspectratio = (float)(i/1000.0); /* chop precision to 3 decimals */

   /* setup view window stuff */
   viewwindow = viewxdots = viewydots = 0;
   if (filexdots != videoentry.xdots || fileydots != videoentry.ydots) {
      /* image not exactly same size as screen */
      viewwindow = 1;
      ftemp = finalaspectratio
            * (double)videoentry.ydots / (double)videoentry.xdots
            / screenaspect;
      if (finalaspectratio <= screenaspect) {
         i = (int)((double)videoentry.xdots / (double)filexdots * 20.0 + 0.5);
         tmpreduce = (float)(i/20.0); /* chop precision to nearest .05 */
         i = (int)((double)videoentry.xdots / tmpreduce + 0.5);
         j = (int)((double)i * ftemp + 0.5);
         }
      else {
         i = (int)((double)videoentry.ydots / (double)fileydots * 20.0 + 0.5);
         tmpreduce = (float)(i/20.0); /* chop precision to nearest .05 */
         j = (int)((double)videoentry.ydots / tmpreduce + 0.5);
         i = (int)((double)j / ftemp + 0.5);
         }
      if (i != filexdots || j != fileydots) { /* too bad, must be explicit */
         viewxdots = filexdots;
         viewydots = fileydots;
         }
      else
         viewreduction = tmpreduce; /* ok, this works */
      }
   if (*s_makepar && !fastrestore && !initbatch &&
        (fabs(finalaspectratio - screenaspect) > .00001 || viewxdots != 0)) {
      static FCODE msg[] = {"\
Warning: <V>iew parameters are being set to non-standard values.\n\
Remember to reset them when finished with this image!"};
      stopmsg(4,msg);
      }
   return(0);
}

#if 1
static void format_item(int choice,char *buf)
{
   char errbuf[10];
   unsigned tmpflags;
   errbuf[0] = 0;
   tmpflags = vidptr[choice].flags;
   if (tmpflags & (VI_VSMALL+VI_CSMALL+VI_ASPECT)) strcat(errbuf,"*");
   if (tmpflags & VI_VSMALL) strcat(errbuf,"R");
   if (tmpflags & VI_CSMALL) strcat(errbuf,"C");
   if (tmpflags & VI_ASPECT) strcat(errbuf,"A");
   if (tmpflags & VI_VBIG)   strcat(errbuf,"v");
   if (tmpflags & VI_CBIG)   strcat(errbuf,"c");
   format_vid_inf(vidptr[choice].entnum,errbuf,buf);
}

static int check_modekey(int curkey,int choice)
{
   int i;
   i=choice; /* avoid warning */
   return (((i = check_vidmode_key(0,curkey)) >= 0) ? -100-i : 0);
}
#endif

