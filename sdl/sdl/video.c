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

extern U32 pixel[48];

int fake_lut = 0;
int istruecolor = 0;
int daclearn = 0;
int dacnorm = 0;
int daccount = 0;
int ShadowColors;
int goodmode = 0;   /* if non-zero, OK to read/write pixels */
void (*dotwrite) (int, int, U32);  /* write-a-dot routine */
U32 (*dotread) (int, int);         /* read-a-dot routine */
void (*linewrite) (int, int, int, U32 *); /* write-a-line routine */
void (*lineread) (int, int, int, U32 *);  /* read-a-line routine */
int andcolor = 0;          /* "and" value used for color selection */
int diskflag = 0;          /* disk video active flag */

int videoflag = 0;         /* special "your-own-video" flag */

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

enum {
  TEXT_WIDTH = 80,
  TEXT_HEIGHT = 25,
  MOUSE_SCALE = 1
};

int txt_ht;  /* text letter height = 2^txt_ht pixels */
int txt_wt;  /* text letter width = 2^txt_wt pixels */

char text_screen[TEXT_HEIGHT][TEXT_WIDTH];
int  text_attr[TEXT_HEIGHT][TEXT_WIDTH];
char stack_text_screen[TEXT_HEIGHT][TEXT_WIDTH];
int  stack_text_attr[TEXT_HEIGHT][TEXT_WIDTH];


void setforgraphics (void);
void setvideomode (int);
void putstring (int, int, int, CHAR *);
void normaline (int, int, int, U32 *);
void normalineread (int, int, int, U32 *);


void nullwrite (int a, int b, U32 c)
{
}

U32 nullread (int a, int b)
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
  if (videoflag)
    {
      videoflag = 0;
    }
  goodmode = 1;
  switch (dotmode)
    {
    case 1:   /* text */
// FIXME (jonathan#1#): Add code to setup text screen.
      starttext();
      break;
    case 2:   /* video window */
      dotwrite = writevideo;
      dotread = readvideo;
      lineread = readvideoline;
      linewrite = writevideoline;
      videoflag = 1;
      startvideo ();
      setforgraphics ();
      break;
    default:
      printf ("Bad video mode %d\n", dotmode);
      exit (-1);
    }
// need to set istruecolor, colors, ... based on properties of screen

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
  dotwrite (xdot + sxoffs, ydot + syoffs, color /* & andcolor */);
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
; PUTSTR.asm puts a string directly to video display memory. Called from C by:
;    putstring(row, col, attr, string) where
;         row, col = row and column to start printing.
;         attr = color attribute.
;         string = far pointer to the null terminated string to print.
;    Written for the A86 assembler (which has much less 'red tape' than MASM)
;    by Bob Montgomery, Orlando, Fla.             7-11-88
;    Adapted for MASM 5.1 by Tim Wegner          12-11-89
;    Furthur mucked up to handle graphics
;       video modes by Bert Tyler                 1-07-90
;    Reworked for:  row,col update/inherit;
;       620x200x2 inverse video;  far ptr to string;
;       fix to avoid scrolling when last posn chgd;
;       divider removed;  newline ctl chars;  PB  9-25-90
*/
void putstring (int row, int col, int attr, CHAR *msg)
{
  int r, c, i, k, s_r, s_c;
  int foregnd = attr & 15;
  int backgnd = (attr >> 4) & 15;
  int tmp_attr;
  int max_c = 0;

  if (row != -1)
    textrow = row;
  if (col != -1)
    textcol = col;

  if (attr & BRIGHT && !(attr & INVERSE))   /* bright */
    {
      foregnd += 8;
    }
  if (attr & INVERSE)   /* inverse video */
{
// FIXME (jonathan#1#): How do we implement next????
//    text_mode(palette_color[foregnd]);
      tmp_attr = backgnd;
    }
  else
    {
//    text_mode(palette_color[backgnd]);
      tmp_attr = foregnd;
    }

  s_r = r = textrow + textrbase;
  s_c = c = textcol + textcbase;

  while (*msg)
    {
      if (*msg == '\n')
        {
          textrow++;
          r++;
          if (c > max_c)
            max_c = c;
          textcol = 0;
          c = textcbase;
        }
      else
        {
#if DEBUG
          if (c >= TEXT_WIDTH) c = TEXT_WIDTH - 1; /* keep going, but truncate */
          if (r >= TEXT_HEIGHT) r = TEXT_HEIGHT - 1;
#endif
          assert(r < TEXT_HEIGHT);
          assert(c < TEXT_WIDTH);
          text_screen[r][c] = *msg;
          text_attr[r][c] = attr;
          textcol++;
          c++;
        }
      msg++;
    }

  if (c > max_c)
    max_c = c;

  i = s_r<<txt_ht; /* reuse i for blit */
  k = s_c<<txt_wt;
  if (r == 0)
     r = 1;
  if (max_c > TEXT_WIDTH)
    max_c = TEXT_WIDTH;
  c = max_c - s_c;     /* reuse c for blit, now it's max width of msg */
  if (r > TEXT_HEIGHT - s_r)
    r = TEXT_HEIGHT - s_r;

// FIXME (jonathan#1#): blit to screen here
// blit(txt,screen,k,i,k,i,c<<txt_wt,r<<txt_ht);
}

/*
; setattr(row, col, attr, count) where
;         row, col = row and column to start printing.
;         attr = color attribute.
;         count = number of characters to set
;         This routine works only in real color text mode.
*/
void setattr (int row, int col, int attr, int count)
{
  int i = col;
  int k;
  int r, c, s_r, s_c;
  int s_count = count;
  int foregnd = attr & 15;
  int backgnd = (attr >> 4) & 15;
  int tmp_attr;

  if (attr & BRIGHT && !(attr & INVERSE)) { /* bright */
    foregnd += 8;
  }
  if (attr & INVERSE) { /* inverse video */
// FIXME (jonathan#1#): How do we implement next????
//    text_mode(palette_color[foregnd]);
    tmp_attr = backgnd;
  }
  else {
// FIXME (jonathan#1#): How do we implement next????
//    text_mode(palette_color[backgnd]);
    tmp_attr = foregnd;
  }

  if (row != -1)
    textrow = row;
  if (col != -1)
    textcol = col;
  s_r = r = textrow + textrbase;
  s_c = c = textcol + textcbase;

  assert(count <= TEXT_WIDTH * TEXT_HEIGHT);
  while (count) {
    assert(r < TEXT_HEIGHT);
    assert(i < TEXT_WIDTH);
    text_attr[r][i] = attr;
    if (++i == TEXT_WIDTH) {
      i = 0;
      r++;
    }
    count--;
  }
  /* refresh text */
  if (r == 0)
    r = 1;
  if (r > TEXT_HEIGHT - s_r)
    r = TEXT_HEIGHT - s_r;
  if (s_count > TEXT_WIDTH - s_c)
    s_count = TEXT_WIDTH - s_c;
  i = s_r<<txt_ht; /* reuse i for blit, above i is col, now it's row */
  k = s_c<<txt_wt;

// FIXME (jonathan#1#): blit to screen here
// blit(txt,screen,k,i,k,i,s_count<<txt_wt,r<<txt_ht);
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
}

void setforgraphics (void)
{
  startvideo ();
  spindac (0, 1);
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

/*
; **************** internal Read/Write-a-line routines *********************
;
;       These routines are called by out_line(), put_line() and get_line().
*/

void normaline (int y, int x, int lastx, U32 *pixels)
{
  int i, width;
  width = lastx - x + 1;
  for (i = 0; i < width; i++)
    {
      dotwrite (x + i, y, pixels[i]);
    }
}

void normalineread (int y, int x, int lastx, U32 *pixels)
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

void get_line (int row, int startcol, int stopcol, U32 *pixels)
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

void put_line (int row, int startcol, int stopcol, U32 *pixels)
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
int out_line (U32 *pixels, int linelen)
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

/*
 * The stackscreen()/unstackscreen() functions were originally
 * ported to Xfractint.
 * These functions are useful for switching between different text screens.
 * For example, looking at a parameter entry using F2.
 */

int screenctr = 0;
// NOTE (jonathan#1#): Don't need next.  Never checked.
#define MAXSCREENS 3
// May need something if two text screens isn't enough
//static BYTE *savescreen[MAXSCREENS];
//static int saverc[MAXSCREENS+1];

void stackscreen(void)
{
  int r, c;
#if DEBUG
  fprintf(stderr, "stack_screen, %i screens stacked\n", screenctr+1);
#endif
/* since we double buffer,  */
/* no need to clear the screen, the text routines do it */
  if (screenctr > 0) {
    for (r = 0; r < TEXT_HEIGHT; r++)
      for (c = 0; c < TEXT_WIDTH; c++) {
        stack_text_screen[r][c] = text_screen[r][c];
        stack_text_attr[r][c] = text_attr[r][c];
      }
// FIXME (jonathan#1#): Put text in textmsg then blit to screen
//    blit(txt,stack_txt,0,0,0,0,TEXT_WIDTH<<txt_wt,TEXT_HEIGHT<<txt_ht);
  }
  screenctr++;
}

void unstackscreen(void)
{
  int r, c;
#if DEBUG
  fprintf(stderr, "unstack_screen, %i screens stacked\n", screenctr);
#endif
  if (screenctr > 1) {
// FIXME (jonathan#1#): blit textbkgd to screen
//    set_palette(default_palette);
    for (r = 0; r < TEXT_HEIGHT; r++)
      for (c = 0; c < TEXT_WIDTH; c++) {
        text_screen[r][c] = stack_text_screen[r][c];
        text_attr[r][c] = stack_text_attr[r][c];
      }
// FIXME (jonathan#1#): Put text in textmsg then blit to screen
//    blit(txt,screen,0,0,0,0,TEXT_WIDTH<<txt_wt,TEXT_HEIGHT<<txt_ht);
  }
  else {
// FIXME (jonathan#1#): blit backscrn to screen  }
  }
  screenctr--;
}

void discardscreen(void)
{
  screenctr = 0;   /* unstack all */
}
