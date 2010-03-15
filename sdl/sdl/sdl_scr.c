
#include <stdlib.h>
#include <SDL.h>
#include <SDL_image.h>

#include "port.h"
#include "prototyp.h"


/* SDL global variables */
extern SDL_Surface *screen;

struct _win_st
  {
    int   _cur_y, _cur_x;
    int   _car_y, _car_x;
    int   _num_y, _num_x;
    int   _cur_attr;
    char  *_text;
    short *_attr;
  };

#define WINDOW struct _win_st

WINDOW *curwin;

int fake_lut = 0;
int istruecolor = 0;
int color_dark = 0;   /* darkest color in palette */
int color_bright = 0; /* brightest color in palette */
int color_medium = 0; /* nearest to medbright grey in palette */

int andcolor = 0;    /* "and" value used for color selection */
int videoflag = 0;   /* special "your-own-video" flag */
void (*dotwrite) (int, int, Uint32); /* write-a-dot routine */
Uint32 (*dotread) (int, int);        /* read-a-dot routine */
void (*linewrite) ();                /* write-a-line routine */
void (*lineread) ();                 /* read-a-line routine */


// examples (bad) for locking the screen surface
void Slock(SDL_Surface *screen)
{
  if ( SDL_MUSTLOCK(screen) )
    {
      if ( SDL_LockSurface(screen) < 0 )
        {
          return;
        }
    }
}

void Sulock(SDL_Surface *screen)
{
  if ( SDL_MUSTLOCK(screen) )
    {
      SDL_UnlockSurface(screen);
    }
}


/*
 * Return the pixel value at (x, y)
 * NOTE: The surface must be locked before calling this!
 */
Uint32 readvideo(int x, int y)
{
  int bpp = screen->format->BytesPerPixel;
  /* Here p is the address to the pixel we want to retrieve */
  Uint8 *p = (Uint8 *)screen->pixels + y * screen->pitch + x * bpp;

  switch (bpp)
    {
    case 1:
      return *p;

    case 2:
      return *(Uint16 *)p;

    case 3:
      if (SDL_BYTEORDER == SDL_BIG_ENDIAN)
        return p[0] << 16 | p[1] << 8 | p[2];
      else
        return p[0] | p[1] << 8 | p[2] << 16;

    case 4:
      return *(Uint32 *)p;

    default:
      return 0;       /* shouldn't happen, but avoids warnings */
    }
}

/*
 * Set the pixel at (x, y) to the given value
 * NOTE: The surface must be locked before calling this!
 */
void writevideo(int x, int y, Uint32 pixel)
{
  int bpp = screen->format->BytesPerPixel;
  /* Here p is the address to the pixel we want to set */
  Uint8 *p = (Uint8 *)screen->pixels + y * screen->pitch + x * bpp;

  switch (bpp)
    {
    case 1:
      *p = pixel;
      break;

    case 2:
      *(Uint16 *)p = pixel;
      break;

    case 3:
      if (SDL_BYTEORDER == SDL_BIG_ENDIAN)
        {
          p[0] = (pixel >> 16) & 0xff;
          p[1] = (pixel >> 8) & 0xff;
          p[2] = pixel & 0xff;
        }
      else
        {
          p[0] = pixel & 0xff;
          p[1] = (pixel >> 8) & 0xff;
          p[2] = (pixel >> 16) & 0xff;
        }
      break;

    case 4:
      *(Uint32 *)p = pixel;
      break;
    }
}

void apply_surface( int x, int y, SDL_Surface* source)
{
  SDL_Rect offset;

  offset.x = x;
  offset.y = y;
  SDL_BlitSurface( source, NULL, screen, &offset );
}

/*
; **************** Function getcolor(xdot, ydot) *******************

;       Return the color on the screen at the (xdot,ydot) point
*/
int
getcolor (int xdot, int ydot)
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
void
putcolor_a (int xdot, int ydot, int color)
{
  dotwrite (xdot + sxoffs, ydot + syoffs, color & andcolor);
}


/*
 *----------------------------------------------------------------------
 *
 * writevideoline --
 *
 *  Write a line of pixels to the screen.
 *
 * Results:
 *  None.
 *
 * Side effects:
 *  Draws pixels.
 *
 *----------------------------------------------------------------------
 */
void writevideoline(int y, int x, int lastx, Uint32 *pixels)
{
  int width;
  int i;

  width = lastx-x+1;
  for (i=0;i<width;i++)
    {
      writevideo(x+i,y,pixels[i]);
    }
}
/*
 *----------------------------------------------------------------------
 *
 * readvideoline --
 *
 *  Reads a line of pixels from the screen.
 *
 * Results:
 *  None.
 *
 * Side effects:
 *  Gets pixels
 *
 *----------------------------------------------------------------------
 */
void readvideoline(int y, int x, int lastx, Uint32 *pixels)
{
  int i,width;
  width = lastx-x+1;
  for (i=0;i<width;i++)
    {
      pixels[i] = readvideo(x+i,y);
    }
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
  wmove (curwin, row, col);
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
  int so = 0;
  if (row != -1)
    textrow = row;
  if (col != -1)
    textcol = col;

#ifndef NCURSES
  curwin->_cur_attr = attr;
#endif

  if (attr & INVERSE || attr & BRIGHT)
    {
      wstandout (curwin);
      so = 1;
    }

  wmove (curwin, textrow + textrbase, textcol + textcbase);
  while (1)
    {
      if (*msg == '\0')
        break;
      if (*msg == '\n')
        {
          textcol = 0;
          textrow++;
          wmove (curwin, textrow + textrbase, textcol + textcbase);
        }
      else
        {
          char *ptr;
          ptr = strchr (msg, '\n');
          if (ptr == NULL)
            {
              waddstr (curwin, msg);
              break;
            }
          else
            {
              waddch (curwin, *msg);
            }
        }
      msg++;
    }
  if (so)
    {
      wstandend (curwin);
    }
  /*
  #ifdef NCURSES
    wrefresh (curwin);
    fflush (stdout);
  #else
    XFlush(Xdp);
  #endif
  */
  getyx (curwin, textrow, textcol);
  textrow -= textrbase;
  textcol -= textcbase;
}

/* Set video mode */
/*
; **************** Function setvideomode(dotmode) ****************
;       This function sets the (alphanumeric or graphic) video mode
;       of the monitor.   Called with the proper value of dotmode.
;       No returned values, as there is no particular standard to
;       adhere to in this case.  Dotmode is 0 for text and 1 for graphics.

*/
void
setvideomode (int dotmode)
{
  if (diskflag)
    {
      enddisk ();
    }
  if (videoflag)
    {
      endvideo ();
      videoflag = 0;
    }
  goodmode = 1;
  switch (dotmode)
    {
    case 0:   /* text */
      clear ();
// FIXME (jonathan#1#): Add code to setup text screen.
      break;
    case 1:   /* video window */
      putprompt ();
      dotwrite = writevideo;
      dotread = readvideo;
      lineread = readvideoline;
      linewrite = writevideoline;
      videoflag = 1;
      startvideo ();
      setforgraphics ();
      break;
    default:
      printf ("Bad mode %d\n", dotmode);
      exit (-1);
    }
  if (dotmode != 0)
    {
      loaddac ();
      andcolor = colors - 1;
      boxcount = 0;
    }
}

/*
; ********************** Function setvideotext() ************************

;       Sets video to text mode, using setvideomode to do the work.
*/
void
setvideotext (void)
{
  dotmode = 0;
  setvideomode (dotmode);
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
; *************** Function spindac(direction, rstep) ********************

;       Rotate the MCGA/VGA DAC in the (plus or minus) "direction"
;       in "rstep" increments - or, if "direction" is 0, just replace it.
*/
void
spindac (dir, inc)
int dir, inc;
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


/* the following should return a value for success or failure */
void ShowBMP(char *filename)
{
  SDL_Surface* loadedImage = NULL;
  SDL_Surface* optimizedImage = NULL;

  loadedImage = SDL_LoadBMP( filename );
  if ( loadedImage != NULL )
    {
      optimizedImage = SDL_DisplayFormat( loadedImage );
      SDL_FreeSurface( loadedImage );
    }
  if ( optimizedImage != NULL )
    {
      apply_surface( 0, 0, optimizedImage);
      SDL_Flip( screen );
      SDL_FreeSurface( optimizedImage );
    }
}


void ShowGIF(char *filename)
{

  SDL_Surface* loadedImage = NULL;
  SDL_Surface* optimizedImage = NULL;
  SDL_RWops *rwop;

  rwop=SDL_RWFromFile(filename, "rb");
  loadedImage=IMG_LoadGIF_RW(rwop);
  if ( loadedImage != NULL )
    {
      optimizedImage = SDL_DisplayFormat( loadedImage );
      SDL_FreeSurface( loadedImage );
    }
  if ( optimizedImage != NULL )
    {
      apply_surface( 0, 0, optimizedImage);
      SDL_Flip( screen );
      SDL_FreeSurface( optimizedImage );
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
 * The stackscreen()/unstackscreen() functions were originally
 * ported to Xfractint.
 * These functions are useful for switching between different text screens.
 * For example, looking at a parameter entry using F2.
 */

int screenctr = 0;

#define MAXSCREENS 3

static BYTE far *savescreen[MAXSCREENS];
static int saverc[MAXSCREENS+1];

void stackscreen()
{
  int i;
  BYTE far *ptr;
  saverc[screenctr+1] = textrow*80 + textcol;
  if (++screenctr)   /* already have some stacked */
    {
      static char far msg[]={"stackscreen overflow"};
      if ((i = screenctr - 1) >= MAXSCREENS)   /* bug, missing unstack? */
        {
          stopmsg(1,msg);
          exit(1);
        }
      if ((ptr = (savescreen[i] = (BYTE far *)farmemalloc(sizeof(int *)))))
        savecurses((WINDOW **)ptr);
      else
        {
          stopmsg(1,msg);
          exit(1);
        }
      setclear();
    }
  else
    setfortext();
}

void unstackscreen()
{
  BYTE far *ptr;
  textrow = saverc[screenctr] / 80;
  textcol = saverc[screenctr] % 80;
  if (--screenctr >= 0)   /* unstack */
    {
      ptr = savescreen[screenctr];
      restorecurses((WINDOW **)ptr);
      farmemfree(ptr);
    }
  else
    setforgraphics();
  movecursor(-1,-1);
}

void discardscreen()
{
  if (--screenctr >= 0)   /* unstack */
    {
      if (savescreen[screenctr])
        {
          farmemfree(savescreen[screenctr]);
        }
    }
  else
    discardgraphics();
}
