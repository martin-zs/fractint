
#include <stdlib.h>
#include <SDL.h>
#include <SDL_image.h>

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

/* Set video mode */


/*
; **************** Function setvideomode(dotmode) ****************
;       This function sets the (alphanumeric or graphic) video mode
;       of the monitor.   Called with the proper value of dotmode.
;       No returned values, as there is no particular standard to
;       adhere to in this case.  Dotmode is 0 for test and 1 for graphics.

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

// NOTE (jonathan#1#): Need to decide if passing screen around is appropriate.  Don't really need dotread and dotwrite since not using disk video.



/*
 * Return the pixel value at (x, y)
 * NOTE: The surface must be locked before calling this!
 */
Uint32 getpixel(SDL_Surface *surface, int x, int y)
{
  int bpp = surface->format->BytesPerPixel;
  /* Here p is the address to the pixel we want to retrieve */
  Uint8 *p = (Uint8 *)surface->pixels + y * surface->pitch + x * bpp;

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
void putpixel(SDL_Surface *surface, int x, int y, Uint32 pixel)
{
  int bpp = surface->format->BytesPerPixel;
  /* Here p is the address to the pixel we want to set */
  Uint8 *p = (Uint8 *)surface->pixels + y * surface->pitch + x * bpp;

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


void apply_surface( int x, int y, SDL_Surface* source, SDL_Surface* destination )
{
  SDL_Rect offset;

  offset.x = x;
  offset.y = y;
  SDL_BlitSurface( source, NULL, destination, &offset );
}

/* the following should return a value for success or failure */
void ShowBMP(char *filename, SDL_Surface* destination)
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
      apply_surface( 0, 0, optimizedImage, destination );
      SDL_Flip( destination );
      SDL_FreeSurface( optimizedImage );
    }
}


void ShowGIF(char *filename, SDL_Surface* destination)
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
      apply_surface( 0, 0, optimizedImage, destination );
      SDL_Flip( destination );
      SDL_FreeSurface( optimizedImage );
    }
}

