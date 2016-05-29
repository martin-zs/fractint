/* Put all of the routines that call SDL functions in this module */

#include <stdlib.h>
#include <assert.h>
#include <SDL.h>
//#include <SDL_image.h>
#include <SDL_ttf.h>

#include "port.h"
#include "prototyp.h"

/* SDL global variables */
SDL_Surface *mainscrn = NULL;
SDL_Surface *backscrn = NULL;
SDL_Surface *backtext = NULL;
SDL_Window *sdlWindow = NULL;
SDL_Renderer *sdlRenderer = NULL;
SDL_Texture *sdlTexture = NULL;

Uint32 sdlPixelfmt;
int rowbytes;
SDL_PixelFormat *sdlPixelFormat;

TTF_Font *font = NULL;
SDL_Color cols[256];
int SDL_video_flags = SDL_WINDOW_RESIZABLE;
SDL_Cursor *mousecurser = NULL;

SDL_Color XlateText[] =
{
#if BYTE_ORDER == BIG_ENDIAN
/* R, G, B, A */
  {  0,  0,  0,255}, /* black */
  {  0,  0,205,255}, /* Blue */
  {  0,205,  0,255}, /* Green */
  {  0,205,205,255}, /* Cyan */
  {205,  0,  0,255}, /* Red */
  {205,  0,205,255}, /* Magenta */
  {205,205,  0,255}, /* Brown */
  {205,205,205,255}, /* White */
  {100,100,100,255}, /* Gray */
  {  0,  0,255,255}, /* L_Blue */
  {  0,255,  0,255}, /* L_Green */
  {  0,255,255,255}, /* L_Cyan */
  {255,  0,  0,255}, /* L_Red */
  {255,  0,255,255}, /* L_Magenta */
  {255,255,  0,255}, /* Yellow */
  {255,255,255,255}  /* L_White */
#else
/* B, G, R, A */
  {  0,  0,  0,255}, /* black */
  {205,  0,  0,255}, /* Blue */
  {  0,205,  0,255}, /* Green */
  {205,205,  0,255}, /* Cyan */
  {  0,  0,205,255}, /* Red */
  {205,  0,205,255}, /* Magenta */
  {  0,205,205,255}, /* Brown */
  {205,205,205,255}, /* White */
  {100,100,100,255}, /* Gray */
  {255,  0,  0,255}, /* L_Blue */
  {  0,255,  0,255}, /* L_Green */
  {255,255,  0,255}, /* L_Cyan */
  {255,  0,  0,255}, /* L_Red */
  {255,  0,255,255}, /* L_Magenta */
  {  0,255,255,255}, /* Yellow */
  {255,255,255,255}  /* L_White */
#endif
};

static int mousefkey[4][4] /* [button][dir] */ =
{
  {RIGHT_ARROW,LEFT_ARROW,DOWN_ARROW,UP_ARROW},
  {0,0,PAGE_DOWN,PAGE_UP},
  {CTL_PLUS,CTL_MINUS,CTL_DEL,CTL_INSERT},
  {CTL_END,CTL_HOME,CTL_PAGE_DOWN,CTL_PAGE_UP}
};

int resize_flag = 0;
int window_is_fullscreen = 0;
int updatewindow = 1;
int screenctr = 0;
int txt_ht;  /* text letter height = 2^txt_ht pixels */
int txt_wt;  /* text letter width = 2^txt_wt pixels */

char text_screen[TEXT_HEIGHT][TEXT_WIDTH];
int  text_attr[TEXT_HEIGHT][TEXT_WIDTH];
char stack_text_screen[TEXT_HEIGHT][TEXT_WIDTH];
int  stack_text_attr[TEXT_HEIGHT][TEXT_WIDTH];

void puttruecolor_SDL(SDL_Surface*, int, int, Uint8, Uint8, Uint8);
void outtext(int, int, int);


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

void CleanupSDL(void)
{
  /*
   * Quit SDL so we can release the fullscreen
   * mode and restore the previous video settings,
   * etc.  Called by goodbye() routine.
   */

  SDL_DestroyRenderer(sdlRenderer);
  SDL_DestroyTexture(sdlTexture);
  SDL_FreeSurface(mainscrn);
  SDL_FreeSurface(backscrn);
  SDL_FreeSurface(backtext);
  SDL_FreeCursor(mousecurser);
// NOTE (jonathan#1#): May not need this once png support is added.
//  IMG_Quit();

  TTF_CloseFont(font);
  font = NULL;
  TTF_Quit();

  SDL_Quit();
  delay(250);
}

void ResizeScreen(int mode)
{
  /* mode = 0 for initial startup */
  /* mode = 1 to resize the graphics window */
  /* mode = 2 to resize the text window */ /* NOT USED */

  Uint32 rmask, gmask, bmask, amask;
  char msg[40];
  int bpp; /* bits per pixel for graphics mode */
  int sxdots, sydots, dotmode;
  int fontsize;
  SDL_Event resize_event;

  /*
   * Initialize the display in a 800x600 best available bit mode,
   * requesting a hardware surface and double buffering.
   * Failing to initialize that then falls back to
   * Initialize the display in a 800x600 16-bit mode,
   * requesting a software surface
   */
  if (mode == 0)
    {
      if (initmode <= 0)   /* make sure we have something reasonable */
        {
          memcpy((char *)&videoentry,(char *)&videotable[0],
                 sizeof(videoentry));  /* SF6 800x600 now in videoentry */
        }
      else
        {
          adapter = initmode;
          memcpy((char *)&videoentry,(char *)&videotable[adapter],
                 sizeof(videoentry));  /* the selected entry now in videoentry */
        }
      if ( sdlWindow == NULL ) /* Don't create one if we already have one */
         sdlWindow = SDL_CreateWindow(0, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, 1024, 768, SDL_video_flags);
      if ( sdlWindow == NULL ) /* Oops it didn't work, try something different */
          sdlWindow = SDL_CreateWindow(0, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 0, 0, SDL_video_flags);

      if ( sdlWindow == NULL ) /* No luck, bail out */
        {
          SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "Window creation fail : %s\n",SDL_GetError());
          exit(1);
        }

    SDL_SetWindowIcon( sdlWindow, SDL_LoadBMP("Fractint.bmp") );
    }
  else if (mode == 1)  /*  graphics window  */
    {
    /* initialize screen to black */
      SDL_FillRect(mainscrn, NULL, map_to_pixel(0));
      SDL_SetRenderDrawColor(sdlRenderer, 0, 0, 0, 255);
      SDL_RenderClear(sdlRenderer);
      SDL_RenderPresent(sdlRenderer);
      if (resize_flag == 0)  /* not called from event Queue */
        {
#if 0
          bpp = SDL_VideoModeOK(videotable[adapter].xdots,
                                videotable[adapter].ydots,
                                videotable[adapter].dotmode, SDL_video_flags);
          if (bpp != videotable[adapter].dotmode)
            {
              videotable[adapter].dotmode = bpp;
            }
          resize_flag = 1;
#endif
          SDL_SetWindowSize(sdlWindow, videotable[adapter].xdots, videotable[adapter].ydots);
          SDL_SetWindowPosition(sdlWindow, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED);
          memcpy((char *)&videoentry,(char *)&videotable[adapter],
                 sizeof(videoentry));  /* the selected entry now in videoentry */
        }
      else
        memcpy((char *)&videotable[adapter],(char *)&videoentry,
               sizeof(videoentry));
    /* need to free the screens & texture here */
      SDL_DestroyRenderer(sdlRenderer);
      sdlRenderer = NULL;
      SDL_DestroyTexture(sdlTexture);
      sdlTexture = NULL;
      SDL_FreeSurface(mainscrn);
      mainscrn = NULL;
      SDL_FreeSurface(backscrn);
      backscrn = NULL;
      SDL_FreeSurface(backtext);
      backtext = NULL;
    }
  else  /*  mode == 2  text window  */
    {
      sxdots = 800;
      sydots = 600;
      dotmode = 0;
      resize_event.type = SDL_WINDOWEVENT;
      resize_event.window.event = SDL_WINDOWEVENT_RESIZED;
      resize_event.window.data1 = sxdots;
      resize_event.window.data2 = sydots;
      SDL_PushEvent(&resize_event);
      SDL_PumpEvents();
      SDL_PeepEvents(&resize_event,1,SDL_GETEVENT,SDL_FIRSTEVENT,SDL_LASTEVENT);
      TTF_CloseFont(font);
    }

  sxdots = videoentry.xdots;
  sydots = videoentry.ydots;
  dotmode = videoentry.dotmode;
  colors = videoentry.colors;
#if 0
  resize_event.type = SDL_WINDOWEVENT;
  resize_event.window.event = SDL_WINDOWEVENT_RESIZED;
  resize_event.window.data1 = sxdots;
  resize_event.window.data2 = sydots;
  SDL_PushEvent(&resize_event);
  SDL_PumpEvents();
  SDL_PeepEvents(&resize_event,1,SDL_GETEVENT,SDL_FIRSTEVENT,SDL_LASTEVENT);
#endif
#if 0
  if (sdlWindow && window_is_fullscreen)
    {
      SDL_DisplayMode target, closest;
      target.w = sxdots;
      target.h = sydots;
      target.format = 0;
      target.refresh_rate = 0;
      target.driverdata = 0;
      SDL_GetClosestDisplayMode(0, &target, &closest);
      SDL_SetWindowDisplayMode (sdlWindow, (const)closest);
    }
#endif

  if ( sdlRenderer != NULL ) /* Don't create two with same name */
      SDL_DestroyRenderer(sdlRenderer);
  sdlRenderer = SDL_CreateRenderer(sdlWindow, -1, 0);
  if ( sdlRenderer == NULL )
    {
      SDL_LogError(SDL_LOG_CATEGORY_APPLICATION, "Render creation for surface fail : %s\n",SDL_GetError());
      exit(1);
    }

  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "linear");  /* make the scaled rendering look smoother */
  SDL_RenderSetLogicalSize(sdlRenderer, sxdots, sydots);

  /* initialize screen to black */
  SDL_SetRenderDrawColor(sdlRenderer, 0, 0, 0, 255);
  SDL_RenderClear(sdlRenderer);
  SDL_RenderPresent(sdlRenderer);

  sdlPixelfmt = SDL_GetWindowPixelFormat(sdlWindow);
  bpp = SDL_BYTESPERPIXEL(sdlPixelfmt) * 8;
  rowbytes = SDL_BYTESPERPIXEL(sdlPixelfmt) * sxdots;
  if (dotmode == 0)
    videoentry.dotmode = bpp;

#if DEBUG
  sprintf(msg, "%s at %dx%dx%d", Fractint, sxdots, sydots, bpp);
#else
  sprintf(msg, Fractint);
#endif /* 1 */

  SDL_SetWindowTitle(sdlWindow, msg);

  if ( sdlTexture != NULL ) /* Don't create two with same name */
      SDL_DestroyTexture(sdlTexture);
  sdlTexture = SDL_CreateTexture(sdlRenderer, sdlPixelfmt, SDL_TEXTUREACCESS_STREAMING, sxdots, sydots);

#if DEBUG
  fprintf(stderr, "Set %dx%d at %d bits-per-pixel mode\n", sxdots, sydots, bpp);
  if (sdlTexture == NULL )
      SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,
                         "Creation Error",
                         "No sdlTexture.",
                         NULL);
#endif

#if BYTE_ORDER == BIG_ENDIAN
    rmask = 0xff000000;
    gmask = 0x00ff0000;
    bmask = 0x0000ff00;
    amask = 0x000000ff;
#else
    rmask = 0x000000ff;
    gmask = 0x0000ff00;
    bmask = 0x00ff0000;
    amask = 0xff000000;
#endif

  mainscrn = SDL_CreateRGBSurface(0, sxdots, sydots, bpp, rmask, gmask, bmask, amask);
  backscrn = SDL_CreateRGBSurface(0, sxdots, sydots, bpp, rmask, gmask, bmask, amask);
  backtext = SDL_CreateRGBSurface(0, sxdots, sydots, bpp, rmask, gmask, bmask, amask);

#if DEBUG
  if (mainscrn == NULL )
      SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,
                         "Creation Error",
                         "No mainscrn.",
                         NULL);
  if (backscrn == NULL )
      SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,
                         "Creation Error",
                         "No backscrn.",
                         NULL);
  if (backtext == NULL )
      SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,
                         "Creation Error",
                         "No backtext.",
                         NULL);
#endif

  SDL_FillRect(mainscrn, NULL, 0);
  SDL_FillRect(backscrn, NULL, 0);
  SDL_FillRect(backtext, NULL, 0);

  sdlPixelFormat = mainscrn->format;
  if (mousecurser == NULL)
    {
      mousecurser = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_ARROW);
      SDL_SetCursor(mousecurser);
    }

  if (font != NULL)
    {
      TTF_CloseFont(font);
      font = NULL;
    }
  fontsize = (int)(sydots / 34) + 1; /* arbitrary font size, not too big */
  font = TTF_OpenFont("crystal.ttf", fontsize);
  if ( font == NULL )
    {
      SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,
                         "Missing file",
                         "Couldn't set font.",
                         NULL);
      exit(1);
    }

  /* get text height and width in pixels */
  TTF_SizeText(font,"H",&txt_wt,&txt_ht);

  return;
}

void SetupSDL(void)
{
  /* called by main() routine */

  if ( SDL_Init(SDL_INIT_AUDIO|SDL_INIT_VIDEO|SDL_INIT_TIMER) < 0 )
    {
      SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,
                         "Initiation Error",
                         "Unable to init SDL.",
                         NULL);
      exit(1);
    }

  if (TTF_Init() < 0)
    {
      SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,
                         "Initiation Error",
                         "Unable to init TTF.",
                         NULL);
      exit(1);
    }

  ResizeScreen(0);

// NOTE (jonathan#1#): May not need this once png support is added.
//  if ( IMG_Init(IMG_INIT_PNG) < 0 )
//    {
//      fprintf(stderr, "Unable to init IMG: %s\n", SDL_GetError());
//      exit(1);
//    }

//  SDL_EnableKeyRepeat(500,30);

}

/*
; adapter_detect:
;       This routine gets the video modes supported by SDL
;       and puts the compatible ones in vidtbl.
;       It sets variable dotmode.
*/
int done_detect = 0;

void adapter_detect(void)
{
  int wdth;
  int hgth;
  int desktop_w, desktop_h;
  int i, j, display_mode_count;
  static int display_in_use = 0;
  SDL_DisplayMode mode;

  if (done_detect)
    return;

  SDL_GetDesktopDisplayMode(display_in_use, &mode);
  desktop_w = mode.w;
  desktop_h = mode.h;

  display_mode_count = SDL_GetNumDisplayModes(display_in_use);
  if (display_mode_count > MAXVIDEOTABLE)
    display_mode_count = MAXVIDEOTABLE;

  memset((char *)vidtbl,0,sizeof(*vidtbl)*display_mode_count);

  i = 0;
  j = 0;
  do {
      if (SDL_GetDisplayMode(display_in_use, i, &mode) != 0)
          SDL_Log("SDL_GetDisplayMode failed: %s", SDL_GetError());
      if (mode.refresh_rate == 60) {
        if (mode.w == desktop_w && mode.h == desktop_h)
           strncpy(vidtbl[j].comment, "Full Desktop Mode", 25);
        else
           strncpy(vidtbl[j].comment, SDL_GetPixelFormatName(mode.format), 25);
        vidtbl[j].dotmode = SDL_BITSPERPIXEL(mode.format);
        sprintf(vidtbl[j].name, "%i-bit True-Color", vidtbl[j].dotmode);
        vidtbl[j].xdots = mode.w;
        vidtbl[j].ydots = mode.h;
        vidtbl[j].colors = 256; /* this will need to be fixed */
        switch (mode.w)
        {
          case 640:
            if (mode.h == 480)
               vidtbl[j].keynum = check_vidmode_keyname("SF5");
            break;
          case 800:
            if (mode.h == 600)
              vidtbl[j].keynum = check_vidmode_keyname("SF6");
            break;
          case 1024:
            if (mode.h == 768)
              vidtbl[j].keynum = check_vidmode_keyname("SF7");
            break;
          case 1280:
            if (mode.h == 1024)
              vidtbl[j].keynum = check_vidmode_keyname("SF9");
            break;
          default:
            break;
        }
        j++;
      }
      i++;
  } while (i < display_mode_count);
  vidtbllen = j;

  SDL_GetWindowSize(sdlWindow, &wdth, &hgth);
  xdots = wdth;
  ydots = hgth;
  sxdots  = xdots;
  sydots  = ydots;
  done_detect = 1;
  dotmode = SDL_BYTESPERPIXEL(sdlPixelfmt) * 8;
}

void startvideo(void)
{
  int Bpp = SDL_BYTESPERPIXEL(sdlPixelfmt);

  if (Bpp == 1) /* 8-bits per pixel => uses a palette */
    {
      gotrealdac = 1;
      istruecolor = 0;
      colors = videoentry.colors;
    }
  else /* truecolor modes */
    {
      gotrealdac = 0;
      istruecolor = 1;
// FIXME (jonathan#1#): Need to have more colors for truecolor modes
      colors = 256;
    }

/* initialize mainscrn, backscrn, and backtext surfaces to inside color */
  SDL_FillRect(mainscrn, NULL, map_to_pixel(inside));
  SDL_FillRect(backscrn, NULL, map_to_pixel(inside));
  SDL_FillRect(backtext, NULL, map_to_pixel(inside));

  /* initialize window to black */
  SDL_UpdateTexture( sdlTexture, NULL, mainscrn->pixels, rowbytes );
  SDL_RenderCopy(sdlRenderer, sdlTexture, NULL, NULL);
  SDL_SetRenderDrawBlendMode(sdlRenderer, SDL_BLENDMODE_NONE);
  SDL_SetRenderDrawColor(sdlRenderer, 0, 0, 0, 255);
  SDL_RenderClear(sdlRenderer);
  SDL_RenderPresent(sdlRenderer);
}

U32 map_to_pixel(BYTE color)
{
  /* returns the pixel value corresponding to the truemode selection */
  BYTE red, green, blue;

  dac_to_rgb(color, &red, &green, &blue);
  return((U32)SDL_MapRGB(sdlPixelFormat, red, green, blue));
}

/*
 * Return the pixel value at (x, y)
 */
BYTE readvideo(int x, int y)
{
int Bpp = SDL_BYTESPERPIXEL(sdlPixelfmt);
  /* Here p is the address to the pixel we want to retrieve */
  Uint8 *p = (Uint8 *)mainscrn->pixels + y * mainscrn->pitch + x * Bpp;
  Slock(mainscrn);
  switch (Bpp)
    {
    case 1:
      return *p;

    case 2:
      return *(Uint16 *)p;

    case 3:
      if (BYTE_ORDER == BIG_ENDIAN)
        return p[0] << 16 | p[1] << 8 | p[2];
      else
        return p[0] | p[1] << 8 | p[2] << 16;

    case 4:
      return (BYTE)SDL_MapRGB(sdlPixelFormat, p[0], p[1], p[2]);

    default:
      return 0;       /* shouldn't happen, but avoids warnings */
    }
  Sulock(mainscrn);
}

void gettruecolor_SDL(SDL_Surface *screen, int x, int y, Uint8 *red, Uint8 *green, Uint8 *blue)
{
  /* Extracting color components from a 32-bit color value */
  Uint32 *pixel;

  Slock(screen);
  pixel = (Uint32 *)screen->pixels + y*screen->pitch/4 + x;
  Sulock(screen);

  SDL_GetRGB(*pixel, sdlPixelFormat, (Uint8 *)red, (Uint8 *)green, (Uint8 *)blue);
}

void gettruecolor(int x, int y, BYTE *R, BYTE *G, BYTE *B)
{
  gettruecolor_SDL(mainscrn, x, y, (Uint8 *)R, (Uint8 *)G, (Uint8 *)B);
}

void puttruecolor_SDL(SDL_Surface *screen, int x, int y, Uint8 R, Uint8 G, Uint8 B)
{
  /* Currently, SDL gets this backwards */
  Uint32 color = SDL_MapRGB(screen->format, B, G, R);

  Slock(screen);
  switch (screen->format->BytesPerPixel)
    {
    case 1: /* Assuming 8-bpp */
    {
      Uint8 *bufp;
      bufp = (Uint8 *)screen->pixels + y*screen->pitch + x;
      *bufp = color;
    }
    break;
    case 2: /* Probably 15-bpp or 16-bpp */
    {
      Uint16 *bufp;
      bufp = (Uint16 *)screen->pixels + y*screen->pitch*2 + x;
      *bufp = color;
    }
    break;
    case 3: /* Slow 24-bpp mode, usually not used !!! Probably doesn't work !!! look at pitch */
    {
      Uint8 *bufp;
      bufp = (Uint8 *)screen->pixels + y*screen->pitch + x * 3;
      if (SDL_BYTEORDER == SDL_BIG_ENDIAN)
        {
          bufp[2] = color;
          bufp[1] = color >> 8;
          bufp[0] = color >> 16;
        }
      else
        {
          bufp[0] = color;
          bufp[1] = color >> 8;
          bufp[2] = color >> 16;
        }
    }
    break;
    default:
    case 4: /* Probably 32-bpp */
    {
      Uint32 *bufp;
      bufp = (Uint32 *)screen->pixels + y*screen->pitch/4 + x;
      *bufp = color;
    }
    break;
    }
  Sulock(screen);
  if (show_orbit) /* Do it slow, one pixel at a time */
  {
     SDL_UpdateTexture( sdlTexture, NULL, mainscrn->pixels, rowbytes );
     SDL_RenderCopy(sdlRenderer, sdlTexture, NULL, NULL);
     SDL_RenderPresent(sdlRenderer);
  }
}

void puttruecolor(int x, int y, BYTE R, BYTE G, BYTE B)
{
  puttruecolor_SDL(mainscrn, x, y, (Uint8)R, (Uint8)G, (Uint8)B);
}

/*
 * Set the pixel at (x, y) to the given value
 */
void writevideo(int x, int y, U32 pixel)
{
  BYTE red, green, blue;

  dac_to_rgb((BYTE)(pixel & andcolor), &red, &green, &blue);
  puttruecolor(x, y, red, green, blue);
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
  for (i=0; i<width; i++)
    {
      writevideo(x+i, y, (U32)pixels[i]);
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
  for (i=0; i<width; i++)
    {
      pixels[i] = (BYTE)readvideo(x+i,y);
    }
}

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
#if 0
  int i;
  for (i=0; i<colors; i++)
    {
      dacbox[i][0] = cols[i].r >> 2;
      dacbox[i][1] = cols[i].g >> 2;
      dacbox[i][2] = cols[i].b >> 2;
    }
#endif
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
void writevideopalette(void)
{
  int i;

  for (i = 0; i < colors; i++)
    {
      if (SDL_BYTEORDER == SDL_BIG_ENDIAN)
        {
          cols[i].r = dacbox[i][0] << 2;
          cols[i].g = dacbox[i][1] << 2;
          cols[i].b = dacbox[i][2] << 2;
        }
      else
        {
          cols[i].r = dacbox[i][2] << 2;
          cols[i].g = dacbox[i][1] << 2;
          cols[i].b = dacbox[i][0] << 2;
        }
    }
  /* Set palette */
  SDL_SetPaletteColors(mainscrn->format->palette, cols, 0, 256);
  SDL_SetPaletteColors(backscrn->format->palette, cols, 0, 256);
}

void savevideopalette(void)
{
  memcpy(olddacbox, dacbox, 256*3);
}

void restorevideopalette(void)
{
  memcpy(dacbox, olddacbox, 256*3);
}

/* start of text processing routines */

void setclear (void)
{
  /* clears the screen to the text background */
  int r, c;

  /*  Fill the screen with black  */
  SDL_FillRect( mainscrn, NULL, SDL_MapRGB( mainscrn->format, 0, 0, 0 ) );

  for (r = 0; r < TEXT_HEIGHT; r++)
    {
      for (c = 0; c < TEXT_WIDTH; c++)
        {
          text_attr[r][c] = 0;
          text_screen[r][c] = ' ';
        }
      outtext(r, 0, c); /* clear a row at a time */
    }
}

void starttext(void)
{
  /* Setup text palette */
  int i;
// FIXME (jonathan#1#): This still doesn't work
  if (mainscrn->format->BitsPerPixel == 8)
    {
      savevideopalette();
      for (i = 0; i < 16; i++)
        {
        if (SDL_BYTEORDER == SDL_BIG_ENDIAN)
          {
            dacbox[i][2] = XlateText[i].r;
            dacbox[i][1] = XlateText[i].g;
            dacbox[i][0] = XlateText[i].b;
          }
        else
          {
            dacbox[i][0] = XlateText[i].r;
            dacbox[i][1] = XlateText[i].g;
            dacbox[i][2] = XlateText[i].b;
          }
        }
      spindac(0,1);
      SDL_SetPaletteColors(mainscrn->format->palette, XlateText, 0, 16);
      SDL_SetPaletteColors(backtext->format->palette, XlateText, 0, 16);
    }
}

void outtext(int row, int col, int max_c)
{
  /* Takes the text in text_screen[row][] with */
  /*     attributes in text_attr[row][] */
  /* and puts it on the screen */

  SDL_Color color, bgcolor;
  SDL_Surface *text_surface = NULL;
  SDL_Rect text_rect;

  int i, j;
  int attr    = text_attr[row][col];
  int foregnd = attr & 15;
  int backgnd = (attr >> 4) & 15;
  char buf[TEXT_WIDTH+1]; /* room for text and null terminator */
#if 0
  fprintf(stderr, "outtext, %d, %d, %d\n", row, col, max_c);
  fprintf(stderr, "  attributes, %d, %d, %d\n", attr, foregnd, backgnd);
{
      char errortxt[40];
      sprintf(errortxt, "outtext, %d, %d, %d\nattributes, %d, %d, %d", row, col, max_c, attr, foregnd, backgnd);
      SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,
                         "OutText",
                         errortxt,
                         NULL);
}
#endif

  text_rect.x = col * txt_wt;   /* starting column */
  text_rect.y = row * txt_ht;   /* starting row */
  text_rect.w = max_c * txt_wt; /* output this many columns */
  text_rect.h = 1 * txt_ht;     /* output one row at a time */

  for (i = 0; i <= TEXT_WIDTH; i++) /* clear the text buffer */
    buf[i] = 0;

  /* put text into text buffer */
  j = 0;
  for (i = col; i < max_c; i++)
    {
      buf[j++] = text_screen[row][i];
    }

  color   = XlateText[foregnd];
  bgcolor = XlateText[backgnd];

  if (!(text_surface = TTF_RenderText_Shaded(font,buf,color,bgcolor)))
    {
      /* Handle error here */
#if 0
      char errortxt[40];
      sprintf(errortxt, "outtext error: %s.", SDL_GetError());
      SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,
                         "Text Error",
                         errortxt,
                         NULL);
#endif
    }
  else
    {
      SDL_BlitSurface(text_surface, NULL, mainscrn, &text_rect);
      SDL_FreeSurface(text_surface);
    }
}

void save_screen(void)
{
  SDL_BlitSurface( mainscrn, NULL, backscrn, NULL ); /* save mainscrn */
}

void restore_screen(void)
{
  SDL_BlitSurface( backscrn, NULL, mainscrn, NULL );
}

void save_text(void)
{
  SDL_BlitSurface( mainscrn, NULL, backtext, NULL ); /* save text screen */
  setclear();
}

void restore_text(void)
{
  SDL_BlitSurface( backtext, NULL, mainscrn, NULL );
}

/*
 * The stackscreen()/unstackscreen() functions were originally
 * ported to Xfractint.
 * These functions are useful for switching between different text screens.
 * For example, looking at a parameter entry using F2.
 */

// NOTE (jonathan#1#): Don't need next.  Never checked.
#define MAXSCREENS 3
// May need something if two text screens isn't enough
//static BYTE *savescreen[MAXSCREENS];
//static int saverc[MAXSCREENS+1];

void stackscreen(void)
{
  int r, c;
#if DEBUG
  fprintf(stderr, "stackscreen, %i screens stacked\n", screenctr+1);
#endif

  if (screenctr == 0)
    {
      save_screen();
    }
  if (screenctr > 0)
    {
      save_text();
      for (r = 0; r < TEXT_HEIGHT; r++)
        for (c = 0; c < TEXT_WIDTH; c++)
          {
            stack_text_screen[r][c] = text_screen[r][c];
            stack_text_attr[r][c] = text_attr[r][c];
          }
    }
  screenctr++;
}

void unstackscreen(void)
{
  int r, c;
#if DEBUG
  fprintf(stderr, "unstackscreen, %i screens stacked\n", screenctr);
#endif
  if (screenctr > 1)
    {
      restore_text();
      for (r = 0; r < TEXT_HEIGHT; r++)
        {
          for (c = 0; c < TEXT_WIDTH; c++)
            {
              text_screen[r][c] = stack_text_screen[r][c];
              text_attr[r][c] = stack_text_attr[r][c];
            }
        }
    }
  else
    {
      restore_screen(); /* restore screen */
    }
  screenctr--;
  if (screenctr < 0) /* shouldn't happen */
    screenctr = 0;
}

void discardscreen(void)
{
#if DEBUG
  fprintf(stderr, "discardscreen, %i screens stacked\n", screenctr);
#endif
  screenctr = 0;   /* unstack all */
  restore_screen(); /* restore screen */
}

void putstring (int row, int col, int attr, CHAR *msg)
{
  int r, c, s_c;
  int max_c = 0;
#if 0
  fprintf(stderr, "putstring, %s\n", msg);
/* {
      char errortxt[80];
      sprintf(errortxt, "%s", msg);
      SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,
                         "PutString",
                         errortxt,
                         NULL);
} */
#endif

  if (row != -1)
    textrow = row;
  if (col != -1)
    textcol = col;

  r = textrow + textrbase;
  s_c = c = textcol + textcbase;

  while (*msg)
    {
      if (*msg == '\n' || c == TEXT_WIDTH)
        {
          if (c > max_c)
            max_c = c;
          /* output line here, get ready for the next one */
          outtext(r, s_c, max_c);
          textrow++;
          r++;
          textcol = 0;
          c = s_c;
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

  /* output the last line here */

  outtext(r, s_c, max_c);
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
  int r, c, s_c;
#if DEBUG
  fprintf(stderr, "setattr, row= %d col=%d attr=%d count=%d\n", row, col, attr, count);
#endif

  if (row != -1)
    textrow = row;
  if (col != -1)
    textcol = col;

  r = textrow + textrbase;
  s_c = c = textcol + textcbase;

  assert(count <= TEXT_WIDTH * TEXT_HEIGHT);
  while (count)
    {
      assert(r < TEXT_HEIGHT);
      assert(i < TEXT_WIDTH);
      text_attr[r][i] = attr;
      c++;
      count--;
      if (++i == TEXT_WIDTH)
        {
          outtext(r, s_c, c);
          i = 0;
          r++;
          c = s_c;
        }
    }
  /* refresh text */
  outtext(r, s_c, c);
}

/*
; ************* Function scrollup(toprow, botrow) ******************

;       Scroll the screen up (from toprow to botrow)
*/
void scrollup (int top, int bot)
{
  int r, c;
#if DEBUG
  fprintf(stderr, "scrollup(%d, %d)\n", top, bot);
#endif

  assert(bot < TEXT_HEIGHT);
  for (r = top; r < bot; r++)
    {
      for (c = 0; c < TEXT_WIDTH; c++)
        {
          text_attr[r][c] = text_attr[r+1][c];
          text_screen[r][c] = text_screen[r+1][c];
        }
      outtext(r, 0, c);
    }
  for (c = 0; c < TEXT_WIDTH; c++) /* clear bottom line */
    {
      text_attr[bot][c] = 0;
      text_screen[bot][c] = ' ';
    }
  outtext(bot, 0, c);
  /* bottom line is added by intro() */
}

static int translate_key(SDL_KeyboardEvent *key)
{
  int tmp = key->keysym.sym & 0x7f; /* 0 - 127 */

#if 0
  fprintf(stderr, "translate_key(%i): ''%c'' with mod %i\n",
          key->keysym.sym, key->keysym.sym, key->keysym.mod);
{
      char errortxt[80];
      sprintf(errortxt, "translate_key(%i): ''%c'' with mod %i", key->keysym.sym, key->keysym.sym, key->keysym.mod);
      SDL_ShowSimpleMessageBox(SDL_MESSAGEBOX_ERROR,
                         "TranslateKey",
                         errortxt,
                         NULL);
}
#endif

  /* This is the SDL key mapping */
  if (key->keysym.mod & KMOD_CTRL) /* Control key down */
    {
      switch (key->keysym.sym)
        {
        case SDLK_F1:
          return CF1;
        case SDLK_F2:
          return CF2;
        case SDLK_F3:
          return CF3;
        case SDLK_F4:
          return CF4;
        case SDLK_F5:
          return CF5;
        case SDLK_F6:
          return CF6;
        case SDLK_F7:
          return CF7;
        case SDLK_F8:
          return CF8;
        case SDLK_F9:
          return CF9;
        case SDLK_F10:
          return CF10;
        case SDLK_TAB:
          return CTL_TAB;
        case SDLK_BACKSLASH:
          return CTL_BACKSLASH;
        case SDLK_MINUS:
          return CTL_MINUS;
        case SDLK_EQUALS: /* pretend shift is pressed */
        case SDLK_PLUS:
          return CTL_PLUS;
        case SDLK_RETURN:
          return CTL_ENTER;
        case SDLK_INSERT:
          return CTL_INSERT;
        case SDLK_DELETE:
          return CTL_DEL;
        case SDLK_HOME:
          return CTL_HOME;
        case SDLK_END:
          return CTL_END;
        case SDLK_PAGEUP:
          return CTL_PAGE_UP;
        case SDLK_PAGEDOWN:
          return CTL_PAGE_DOWN;
        case SDLK_LEFT:
          return LEFT_ARROW_2;
        case SDLK_RIGHT:
          return RIGHT_ARROW_2;
        case SDLK_UP:
          return UP_ARROW_2;
        case SDLK_DOWN:
          return DOWN_ARROW_2;
        case SDLK_KP_ENTER:
          return CTL_ENTER_2;
        default:
          if (tmp >= 'a' && tmp <= 'z')
            return (tmp - 'a' + 1);
        }
    }
  else if (key->keysym.mod & KMOD_SHIFT) /* Shift key down */
    {
      switch (key->keysym.sym)
        {
        case SDLK_BACKQUOTE:
          return '~';
        case SDLK_0:
          return SDLK_RIGHTPAREN;
        case SDLK_1:
          return SDLK_EXCLAIM;
        case SDLK_2:
          return SDLK_AT;
        case SDLK_3:
          return SDLK_HASH;
        case SDLK_4:
          return SDLK_DOLLAR;
        case SDLK_5:
          return '%';
        case SDLK_6:
          return SDLK_CARET;
        case SDLK_7:
          return SDLK_AMPERSAND;
        case SDLK_8:
          return SDLK_ASTERISK;
        case SDLK_9:
          return SDLK_LEFTPAREN;
        case SDLK_MINUS:
          return SDLK_UNDERSCORE;
        case SDLK_EQUALS:
          return SDLK_PLUS;
        case SDLK_LEFTBRACKET:
          return '{';
        case SDLK_RIGHTBRACKET:
          return '}';
        case SDLK_BACKSLASH:
          return '|';
        case SDLK_SEMICOLON:
          return SDLK_COLON;
        case SDLK_QUOTE:
          return SDLK_QUOTEDBL;
        case SDLK_COMMA:
          return SDLK_LESS;
        case SDLK_PERIOD:
          return SDLK_GREATER;
        case SDLK_SLASH:
          return SDLK_QUESTION;
        case SDLK_F1:
          return SF1;
        case SDLK_F2:
          return SF2;
        case SDLK_F3:
          return SF3;
        case SDLK_F4:
          return SF4;
        case SDLK_F5:
          return SF5;
        case SDLK_F6:
          return SF6;
        case SDLK_F7:
          return SF7;
        case SDLK_F8:
          return SF8;
        case SDLK_F9:
          return SF9;
        case SDLK_F10:
          return SF10;
        case SDLK_TAB:
          return BACK_TAB;
        default:
          if (tmp >= 'a' && tmp <= 'z')
            return (tmp - ('a' - 'A'));
        }
    }
  else if (key->keysym.mod & KMOD_ALT) /* Alt key down */
    {
      switch (key->keysym.sym)
        {
        case SDLK_F1:
          return AF1;
        case SDLK_F2:
          return AF2;
        case SDLK_F3:
          return AF3;
        case SDLK_F4:
          return AF4;
        case SDLK_F5:
          return AF5;
        case SDLK_F6:
          return AF6;
        case SDLK_F7:
          return AF7;
        case SDLK_F8:
          return AF8;
        case SDLK_F9:
          return AF9;
        case SDLK_F10:
          return AF10;
        case SDLK_TAB:
          return ALT_TAB;
        case SDLK_a:
          return ALT_A;
        case SDLK_s:
          return ALT_S;
        case SDLK_1:
          return ALT_1;
        case SDLK_2:
          return ALT_2;
        case SDLK_3:
          return ALT_3;
        case SDLK_4:
          return ALT_4;
        case SDLK_5:
          return ALT_5;
        case SDLK_6:
          return ALT_6;
        case SDLK_7:
          return ALT_7;
        default:
          break;
        }
    }
  /* No modifier key down */
  switch (key->keysym.sym)
    {
    case SDLK_F1:
      return F1;
    case SDLK_F2:
      return F2;
    case SDLK_F3:
      return F3;
    case SDLK_F4:
      return F4;
    case SDLK_F5:
      return F5;
    case SDLK_F6:
      return F6;
    case SDLK_F7:
      return F7;
    case SDLK_F8:
      return F8;
    case SDLK_F9:
      return F9;
    case SDLK_F10:
      return F10;
    case SDLK_INSERT:
      return INSERT;
    case SDLK_DELETE:
      return DELETE;
    case SDLK_HOME:
      return HOME;
    case SDLK_END:
      return END;
    case SDLK_PAGEUP:
      return PAGE_UP;
    case SDLK_PAGEDOWN:
      return PAGE_DOWN;
    case SDLK_LEFT:
      return LEFT_ARROW;
    case SDLK_RIGHT:
      return RIGHT_ARROW;
    case SDLK_UP:
      return UP_ARROW;
    case SDLK_DOWN:
      return DOWN_ARROW;
    case SDLK_ESCAPE:
      return ESC;
    case SDLK_KP_ENTER:
      return ENTER_2;
    case SDLK_RETURN:
      return ENTER;
    default:
      break;
    }

  if (key->keysym.sym <= 127)
    return tmp;
  else
    return 0;
}

int left_mouse_button_down = 0;
int right_mouse_button_down = 0;
int button_held = 0;

void check_mouse(SDL_Event mevent)
{
  static int lastx = 0;
  static int lasty = 0;
  static int dx,dy;
  int bandx0, bandy0, bandx1, bandy1;
  int done = 0;
  static int banding = 0;

  if (screenctr) /* don't do it, we're on a text screen */
    return;

  if (lookatmouse == 3 || zoomoff == 0)
    {
      lastx = mevent.button.x;
      lasty = mevent.button.y;
      return;
    }

  if (left_mouse_button_down && !button_held)
    {
      if (zoomoff == 1)  /* zooming is allowed */
        {
          if (zwidth == 0)  /* haven't started zooming yet */
            {
              /* start zoombox */
              button_held = 1;
              banding = 0;
              lastx = mevent.button.x;
              lasty = mevent.button.y;
              find_special_colors();
              boxcolor = color_bright;
            }
        }
    }

  if (left_mouse_button_down && button_held && !banding)
    {
       bandx1 = mevent.button.x;
       bandy1 = mevent.button.y;
       if (abs(bandx1 - lastx) > 2 || abs(bandy1 - lasty) > 2)
        {
          banding = 1;
        }
    }

  if(left_mouse_button_down && button_held && banding)
    {
      /* Get the mouse offset */
      dx = abs(mevent.button.x - lastx) / MOUSE_SCALE;

      /* (zbx,zby) is upper left corner of zoom box */
      /* zwidth & zdepth are deltas to lower right corner */
      zbx = (lastx - sxoffs) / dxsize;
      zby = (lasty - syoffs) / dysize;
      zwidth = dx / dxsize;
      zdepth = dx * finalaspectratio / dysize; /* maintain aspect ratio here */

      chgboxi(zbx,zby);
      drawbox(1);
    }

  if (right_mouse_button_down)
    (*plot)(mevent.button.x, mevent.button.y, 4);

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
            case SDL_WINDOWEVENT_RESIZED:
              videoentry.xdots = event.window.data1 & 0xFFFC;  /* divisible by 4 */
              /*  Maintain aspect ratio of image  */
              videoentry.ydots = finalaspectratio * videoentry.xdots;
              discardscreen(); /* dump text screen if in use */
              resize_flag = 1;
              ResizeScreen(1);
              keypressed = ENTER;
              break;
            case SDL_WINDOWEVENT_EXPOSED:
            case SDL_WINDOWEVENT_SHOWN:
              updatewindow = 1;
              break;
            case SDL_WINDOWEVENT_RESTORED:
              window_is_fullscreen = 0;
              updatewindow = 1;
              break;
            case SDL_WINDOWEVENT_MAXIMIZED:
              window_is_fullscreen = 1;
              updatewindow = 1;
              break;
            case SDL_WINDOWEVENT_HIDDEN:
            case SDL_WINDOWEVENT_MINIMIZED:
              updatewindow = 0;
              break;
            case SDL_MOUSEMOTION:
              check_mouse(event);
              break;
            case SDL_MOUSEBUTTONDOWN:
              if (event.button.button == SDL_BUTTON_LEFT && left_mouse_button_down == 1)
                button_held = 1;
              if (event.button.button == SDL_BUTTON_LEFT)
                left_mouse_button_down = 1;
              if (event.button.button == SDL_BUTTON_RIGHT)
                right_mouse_button_down = 1;
              break;
            case SDL_MOUSEBUTTONUP:
              if (event.button.button == SDL_BUTTON_LEFT) {
                if (left_mouse_button_down == 1)
                   keypressed = ENTER;
                left_mouse_button_down = 0;
                button_held = 0;
              }
              if (event.button.button == SDL_BUTTON_RIGHT)
                right_mouse_button_down = 0;
              break;
            case SDL_KEYDOWN:
              keypressed = translate_key(&event.key);
              break;
            case SDL_QUIT:
              exit(0);
              break;
            default:
              break;
            }
        }
      if (checkautosave()) keypressed = 9999; /* time to save, exit */
  if (time_to_update() && updatewindow)
    {
      SDL_UpdateTexture( sdlTexture, NULL, mainscrn->pixels, rowbytes );
      SDL_RenderCopy(sdlRenderer, sdlTexture, NULL, NULL);
      SDL_RenderPresent(sdlRenderer);
    }
   }
  while (block && !keypressed);
  if (time_to_update() && updatewindow)
    {
      SDL_UpdateTexture( sdlTexture, NULL, mainscrn->pixels, rowbytes );
      SDL_RenderCopy(sdlRenderer, sdlTexture, NULL, NULL);
      SDL_RenderPresent(sdlRenderer);
    }
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


#define TICK_INTERVAL    50
#define TICK_INTERVAL2   100

static U32 next_time = 0;

int time_to_update(void)
{
// FIXME (jonathan#1#): Need to adjust this for bf math.
  /* return a 1 every 50 milliseconds if calculating */
  /* return a 1 every 100  milliseconds if not calculating */

  U32 now;

  now = SDL_GetTicks();
  if (calc_status == 1) /* calculating */
    {
      if (next_time <= now)
        {
          next_time = SDL_GetTicks() + TICK_INTERVAL;
          return (1);
        }
      else
        return (0);
    }
  else if (button_held)
    {
      if (next_time <= now)
        {
          next_time = SDL_GetTicks() + TICK_INTERVAL2;
          return (1);
        }
      else
        return (0);
    }
  else
    {
    SDL_Delay(5);
    return (1);
    }
}
