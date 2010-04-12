/* Put all of the routines that call SDL functions in this module */

#include <stdlib.h>
#include <SDL.h>
#include <SDL_image.h>
#include <SDL_ttf.h>

#include "port.h"
#include "prototyp.h"

/* SDL global variables */
SDL_Surface *screen = NULL;
SDL_Surface *backscrn = NULL;
SDL_Surface *textbkgd = NULL;
SDL_Surface *textmsg = NULL;
TTF_Font *font = NULL;
// NOTE (jonathan#1#): Does next need to be global to catch events????
//SDL_Event event;

void apply_surface( int, int, SDL_Surface*);


void Slock(void)
{
  if ( SDL_MUSTLOCK(screen) )
    {
      if ( SDL_LockSurface(screen) < 0 )
        {
          return;
        }
    }
}

void Sulock(void)
{
  if ( SDL_MUSTLOCK(screen) )
    {
      SDL_UnlockSurface(screen);
    }
}

void CleanupSDL(void)
{
  /*
   * Quit SDL so we can release the fullscreen
   * mode and restore the previous video settings,
   * etc.  Called by goodbye() routine.
   */
  SDL_FreeSurface(backscrn);
  SDL_FreeSurface(textbkgd);
  SDL_FreeSurface(textmsg);
// NOTE (jonathan#1#): May not need this once png support is added.
  IMG_Quit();

  TTF_CloseFont(font);
  font = NULL;
  TTF_Quit();

  SDL_Quit( );

}

void SetupSDL(void)
{
  /* called by main() routine */
  if ( SDL_Init(SDL_INIT_AUDIO|SDL_INIT_VIDEO|SDL_INIT_TIMER) < 0 )
    {
      fprintf(stderr, "Unable to init SDL: %s\n", SDL_GetError());
      exit(1);
    }

// NOTE (jonathan#1#): May not need this once png support is added.
  if ( IMG_Init(IMG_INIT_PNG) < 0 )
    {
      fprintf(stderr, "Unable to init IMG: %s\n", SDL_GetError());
      exit(1);
    }

  if (TTF_Init() < 0)
    {
      fprintf(stderr, "Unable to init TTF: %s\n", SDL_GetError());
      exit(1);
    }

  /*
   * Initialize the display in a 800x600 best available bit mode,
   * requesting a hardware surface and double buffering.
   * Failing to initialize that then falls back to
   * Initialize the display in a 800x600 16-bit mode,
   * requesting a software surface
   */
  xdots = 800;
  ydots = 600;
// FIXME (jonathan#1#): Need to work out changing window size.
//  screen = SDL_SetVideoMode(xdots, ydots, 0, SDL_HWSURFACE|SDL_DOUBLEBUF);
  screen = SDL_SetVideoMode(xdots, ydots, 8, SDL_HWSURFACE|SDL_DOUBLEBUF);
  if (screen == NULL )
    {
      screen = SDL_SetVideoMode(xdots, ydots, 16, SDL_SWSURFACE|SDL_ANYFORMAT);
      /* need flags for no double buffering */
    }
  if ( screen == NULL )
    {
      fprintf(stderr, "Couldn't set %dx%dx16 video mode: %s\n", xdots, ydots,
              SDL_GetError());
      exit(1);
    }
#if DEBUG
  printf("Set %dx%d at %d bits-per-pixel mode\n", xdots, ydots,
         screen->format->BitsPerPixel);
#endif
  backscrn = SDL_SetVideoMode(xdots, ydots, 8, SDL_SWSURFACE);
  textmsg = SDL_SetVideoMode(xdots, ydots, 8, SDL_SWSURFACE);
  textbkgd = SDL_SetVideoMode(xdots, ydots, 8, SDL_SWSURFACE);

  font = TTF_OpenFont("crystal.ttf", 20);
  if ( font == NULL )
    {
      fprintf(stderr, "Couldn't set font: %s\n", SDL_GetError());
      exit(1);
    }

  SDL_WM_SetCaption( "Fractint", NULL );
  SDL_WM_SetIcon(SDL_LoadBMP("fractint.ico"), NULL);
  SDL_EnableKeyRepeat(250,30);

}

void startvideo(void)
{
/* initialize screen and backscrn surfaces to black */
  SDL_FillRect(screen, NULL, 0);
  SDL_FillRect(backscrn, NULL, 0);
}

void setclear (void)
{
  apply_surface(xdots, ydots, textbkgd);
}

void starttext(void)
{
/* initialize textbkgd and textmsg surfaces to black */
  SDL_FillRect(textbkgd, NULL, 0);
  SDL_FillRect(textmsg, NULL, 0);
}

/*
 * Return the pixel value at (x, y)
 * NOTE: The surface must be locked before calling this!
 * Try reading from backscrn, which should match screen
 */
BYTE readvideo(int x, int y)
{
  int bpp = backscrn->format->BytesPerPixel;
  /* Here p is the address to the pixel we want to retrieve */
  Uint8 *p = (Uint8 *)backscrn->pixels + y * backscrn->pitch + x * bpp;

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
      return *(U32 *)p;

    default:
      return 0;       /* shouldn't happen, but avoids warnings */
    }
}

void gettruecolor(SDL_Surface *screen, int x, int y, Uint8 red, Uint8 green, Uint8 blue)
{
  /* Extracting color components from a 32-bit color value */
  SDL_PixelFormat *fmt;
  Uint32 temp, pixel;
/* Uint8 alpha; if needed later */

  fmt = screen->format;
  SDL_LockSurface(screen);
  pixel = *((Uint32*)screen->pixels);
  SDL_UnlockSurface(screen);

  /* Get Red component */
  temp = pixel&fmt->Rmask; /* Isolate red component */
  temp = temp>>fmt->Rshift;/* Shift it down to 8-bit */
  temp = temp<<fmt->Rloss; /* Expand to a full 8-bit number */
  red = (Uint8)temp;

  /* Get Green component */
  temp = pixel&fmt->Gmask; /* Isolate green component */
  temp = temp>>fmt->Gshift;/* Shift it down to 8-bit */
  temp = temp<<fmt->Gloss; /* Expand to a full 8-bit number */
  green = (Uint8)temp;

  /* Get Blue component */
  temp = pixel&fmt->Bmask; /* Isolate blue component */
  temp = temp>>fmt->Bshift;/* Shift it down to 8-bit */
  temp = temp<<fmt->Bloss; /* Expand to a full 8-bit number */
  blue = (Uint8)temp;

#if 0
  /* Get Alpha component */
  temp = pixel&fmt->Amask; /* Isolate alpha component */
  temp = temp>>fmt->Ashift;/* Shift it down to 8-bit */
  temp = temp<<fmt->Aloss; /* Expand to a full 8-bit number */
  alpha = (Uint8)temp;
#endif
}

/*
 * Set the pixel at (x, y) to the given value
 * NOTE: The surface must be locked before calling this!
 * Try writing to backscrn first, then blit to screen
 */
void writevideo(int x, int y, BYTE pixel)
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
      *(U32 *)p = pixel;
      break;
    }
}

void puttruecolor(SDL_Surface *screen, int x, int y, Uint8 R, Uint8 G, Uint8 B)
{
  Uint32 color = SDL_MapRGB(screen->format, R, G, B);
  switch (screen->format->BytesPerPixel)
    {
    case 1: // Assuming 8-bpp
    {
      Uint8 *bufp;
      bufp = (Uint8 *)screen->pixels + y*screen->pitch + x;
      *bufp = color;
    }
    break;
    case 2: // Probably 15-bpp or 16-bpp
    {
      Uint16 *bufp;
      bufp = (Uint16 *)screen->pixels + y*screen->pitch/2 + x;
      *bufp = color;
    }
    break;
    case 3: // Slow 24-bpp mode, usually not used
    {
      Uint8 *bufp;
      bufp = (Uint8 *)screen->pixels + y*screen->pitch + x * 3;
      if (SDL_BYTEORDER == SDL_LIL_ENDIAN)
        {
          bufp[0] = color;
          bufp[1] = color >> 8;
          bufp[2] = color >> 16;
        }
      else
        {
          bufp[2] = color;
          bufp[1] = color >> 8;
          bufp[0] = color >> 16;
        }
    }
    break;
    case 4: // Probably 32-bpp
    {
      Uint32 *bufp;
      bufp = (Uint32 *)screen->pixels + y*screen->pitch/4 + x;
      *bufp = color;
    }
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
void writevideoline(int y, int x, int lastx, BYTE *pixels)
{
  int width;
  int i;

  width = lastx-x+1;
  for (i=0;i<width;i++)
    {
      writevideo(x+i, y, pixels[i]);
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
void readvideoline(int y, int x, int lastx, BYTE *pixels)
{
  int i,width;
  width = lastx-x+1;
  for (i=0;i<width;i++)
    {
      pixels[i] = (BYTE)readvideo(x+i,y);
    }
}

SDL_Color cols[256];

/*
 *----------------------------------------------------------------------
 *
 * readvideopalette --
 *  Reads the current video palette into dacbox.
 *
 *
 * Results:
 *  None.
 *
 * Side effects:
 *  Fills in dacbox.
 *
 *----------------------------------------------------------------------
 */
void readvideopalette(void)
{
  int i;
  for (i=0;i<colors;i++)
    {
// NOTE (jonathan#1#): We may not need to divide by 1024
      dacbox[i][0] = cols[i].r/1024;
      dacbox[i][1] = cols[i].g/1024;
      dacbox[i][2] = cols[i].b/1024;
    }
}

/*
 *----------------------------------------------------------------------
 *
 * writevideopalette --
 *  Writes dacbox into the video palette.
 *
 *
 * Results:
 *  None.
 *
 * Side effects:
 *  Changes the displayed colors.
 *
 *----------------------------------------------------------------------
 */
int writevideopalette(void)
{
  int i;

  for (i = 0; i < colors; i++)
    {
// NOTE (jonathan#1#): We may not need to multiply by 1024
      cols[i].r = dacbox[i][0]*1024;
      cols[i].g = dacbox[i][1]*1024;
      cols[i].b = dacbox[i][2]*1024;
    }
  /* Set palette */
  return (SDL_SetColors(screen, cols, 0, 256));
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

int get_key_event(int block)
{
  SDL_Event event;
  int keypressed = 0;

  do
    {
      /* look for an event */
      if ( SDL_PollEvent ( &event ) )
        {
          /* an event was found */
          switch (event.type)
            {
            case SDL_KEYDOWN:
              keypressed = event.key.keysym.sym;
              break;
            default:
              break;
            }
        }
    }
  while (block && !keypressed);
  return (keypressed);
}

/*
; ***************** Function delay(int delaytime) ************************
;
;       performs a delay of 'delaytime' milliseconds
*/
void delay(int delaytime)
{
  SDL_Delay(delaytime);
}

/*
 *----------------------------------------------------------------------
 *
 * clock_ticks --
 *
 *      Return time in CLK_TCK ticks.
 *
 * Results:
 *      Time.
 *
 * Side effects:
 *      None.
 *
 *----------------------------------------------------------------------
 */
long clock_ticks(void)
{
  return(SDL_GetTicks());
}
