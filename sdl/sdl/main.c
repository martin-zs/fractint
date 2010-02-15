/*
        FRACTINT - The Ultimate Fractal Generator
                        Main Routine
*/

#include <stdio.h>
#include <stdlib.h>
#include <SDL.h>
#include <string.h>
#include <signal.h>

#include <io.h> /* might not be needed */
#include <ctype.h> /* might not be needed */

#ifndef USE_VARARGS
#include <stdarg.h>
#else
#include <varargs.h>
#endif

/* #include hierarchy for fractint is a follows:
      Each module should include port.h as the first fractint specific
          include. port.h includes <stdlib.h>, <stdio.h>, <math.h>,
          <float.h>; and, ifndef XFRACT, <dos.h>.
      Most modules should include prototyp.h, which incorporates by
          direct or indirect reference the following header files:
              mpmath.h
              cmplx.h
              fractint.h
              big.h
              biginit.h
              helpcom.h
              externs.h
      Other modules may need the following, which must be included
          separately:
              fractype.h
              helpdefs.h
              lsys.y
      If included separately from prototyp.h, big.h includes cmplx.h
         and biginit.h; and mpmath.h includes cmplx.h
 */

#include "port.h"
#include "prototyp.h"
#include "fractype.h"
#include "helpdefs.h"

int     helpmode;
long    timer_start,timer_interval;     /* timer(...) start & total */
long    calctime;
char    *fract_dir1="", *fract_dir2="";

/*
   the following variables are out here only so
   that the calcfract() and assembler routines can get at them easily
*/
int     active_system = 0;      /* 0 for DOS, WINFRAC for Windows */
int     dotmode;                /* video access method      */
int     textsafe2;              /* textsafe override from videotable */
int     oktoprint;              /* 0 if printf() won't work */
int     sxdots,sydots;          /* # of dots on the physical screen    */
int     sxoffs,syoffs;          /* physical top left of logical screen */
int     xdots, ydots;           /* # of dots on the logical screen     */
double  dxsize, dysize;         /* xdots-1, ydots-1         */
int     colors = 256;           /* maximum colors available */
long    maxit;                  /* try this many iterations */
int     boxcount;               /* 0 if no zoom-box yet     */
int     zrotate;                /* zoombox rotation         */
double  zbx,zby;                /* topleft of zoombox       */
double  zwidth,zdepth,zskew;    /* zoombox size & shape     */

int     fractype;               /* if == 0, use Mandelbrot  */
char    stdcalcmode;            /* '1', '2', 'g', 'b'       */
long    creal, cimag;           /* real, imag'ry parts of C */
long    delx, dely;             /* screen pixel increments  */
long    delx2, dely2;           /* screen pixel increments  */
LDBL    delxx, delyy;           /* screen pixel increments  */
LDBL    delxx2, delyy2;         /* screen pixel increments  */
long    delmin;                 /* for calcfrac/calcmand    */
double  ddelmin;                /* same as a double         */
double  param[MAXPARAMS];       /* parameters               */
double  potparam[3];            /* three potential parameters*/
long    fudge;                  /* 2**fudgefactor           */
long    l_at_rad;               /* finite attractor radius  */
double  f_at_rad;               /* finite attractor radius  */
int     bitshift;               /* fudgefactor              */

int     badconfig = 0;          /* 'fractint.cfg' ok?       */
int     diskisactive;           /* disk-video drivers flag  */
int     diskvideo;              /* disk-video access flag   */
int     hasinverse = 0;
/* note that integer grid is set when integerfractal && !invert;    */
/* otherwise the floating point grid is set; never both at once     */
long    far *lx0, far *ly0;     /* x, y grid                */
long    far *lx1, far *ly1;     /* adjustment for rotate    */
/* note that lx1 & ly1 values can overflow into sign bit; since     */
/* they're used only to add to lx0/ly0, 2s comp straightens it out  */
double  far *dx0, far *dy0;      /* floating pt equivs */
double  far *dx1, far *dy1;
int     integerfractal;         /* TRUE if fractal uses integer math */

/* usr_xxx is what the user wants, vs what we may be forced to do */
char    usr_stdcalcmode;
int     usr_periodicitycheck;
long    usr_distest;
char    usr_floatflag;

int     viewwindow;             /* 0 for full screen, 1 for window */
float   viewreduction;          /* window auto-sizing */
int     viewcrop;               /* nonzero to crop default coords */
float   finalaspectratio;       /* for view shape and rotation */
int     viewxdots,viewydots;    /* explicit view sizing */
int     video_cutboth;          /* nonzero to keep virtual aspect */
int     zscroll;                /* screen/zoombox 0 fixed, 1 relaxed */

/*      HISTORY  far *history = NULL; */
U16     history = 0;
int     maxhistory = 10;

/* variables defined by the command line/files processor */
int     comparegif = 0;                 /* compare two gif files flag */
int     timedsave = 0;                  /* when doing a timed save */
int     resave_flag = 0;                /* tells encoder not to incr filename */
int     started_resaves = 0;            /* but incr on first resave */
int     save_system;                    /* from and for save files */
int     tabmode = 1;                    /* tab display enabled */

/* for historical reasons (before rotation):         */
/*    top    left  corner of screen is (xxmin,yymax) */
/*    bottom left  corner of screen is (xx3rd,yy3rd) */
/*    bottom right corner of screen is (xxmax,yymin) */
double  xxmin,xxmax,yymin,yymax,xx3rd,yy3rd; /* selected screen corners  */
long    xmin, xmax, ymin, ymax, x3rd, y3rd;  /* integer equivs           */
double  sxmin,sxmax,symin,symax,sx3rd,sy3rd; /* displayed screen corners */
double  plotmx1,plotmx2,plotmy1,plotmy2;     /* real->screen multipliers */

int calc_status = -1; /* -1 no fractal                   */
/*  0 parms changed, recalc reqd   */
/*  1 actively calculating         */
/*  2 interrupted, resumable       */
/*  3 interrupted, not resumable   */
/*  4 completed                    */

int     max_colors;                  /* maximum palette size */
int     zoomoff;                     /* = 0 when zoom is disabled    */
int     savedac;                     /* save-the-Video DAC flag      */
int     browsing;                    /* browse mode flag */
char    file_name_stack[16][MAX_NAME]; /* array of file names used while browsing */
int     name_stack_ptr ;
double  toosmall;
int     minbox;
int     no_sub_images;
int     autobrowse,doublecaution;
char    brwscheckparms,brwschecktype;
char    browsemask[MAX_NAME];
int     scale_map[12] = {1,2,3,4,5,6,7,8,9,10,11,12}; /*RB, array for mapping notes to a (user defined) scale */


#define RESTART           1
#define IMAGESTART        2
#define RESTORESTART      3
#define CONTINUE          4


/* Do nothing if math error */
static void my_floating_point_err(int sig)
{
  if (sig != 0)
    overflow = 1;
}

static void quit_fractint( int code )
{
  /*
   * Quit SDL so we can release the fullscreen
   * mode and restore the previous video settings,
   * etc.
   */
  SDL_Quit( );

  /* Exit program. */
  exit( code );
}


/* not used */
static void handle_key_down( SDL_keysym* keysym )
{

  /*
   * We're only interested if 'Esc' has
   * been presssed.
   *
   * EXERCISE:
   * Handle the arrow keys and have that change the
   * viewing position/angle.
   */
  switch ( keysym->sym )
    {
    case SDLK_ESCAPE:
      quit_fractint( 0 );
      break;
    case SDLK_SPACE:
//        should_rotate = !should_rotate;
      break;
    default:
      break;
    }

}


/* not used */
static void process_events( void )
{
  /* Our SDL event placeholder. */
  SDL_Event event;

  /* Grab all the events off the queue. */
  while ( SDL_PollEvent( &event ) )
    {

      switch ( event.type )
        {
        case SDL_KEYDOWN:
          /* Handle key presses. */
          handle_key_down( &event.key.keysym );
          break;
        case SDL_QUIT:
          /* Handle quit requests (like Ctrl-c). */
          quit_fractint( 0 );
          break;
        }
    }
}



int main(int argc, char *argv[])
{
  SDL_Surface *screen;
  SDL_Event event;

  int     done = 0;
  char    keypressed = 0;
  int     resumeflag;
  int     kbdchar;                     /* keyboard key-hit value       */
  int     kbdmore;                     /* continuation variable        */
  char    stacked = 0;                 /* flag to indicate screen stacked */

  /* Initialize the SDL library */
  if ( SDL_Init(SDL_INIT_AUDIO|SDL_INIT_VIDEO|SDL_INIT_TIMER) < 0 )
    {
      fprintf(stderr, "Unable to init SDL: %s\n", SDL_GetError());
      exit(1);
    }

  /*
   * Initialize the display in a 800x600 best available bit mode,
   * requesting a hardware surface and double buffering.
   * Failing to initialize that then falls back to
   * Initialize the display in a 800x600 16-bit mode,
   * requesting a software surface
   */
  screen = SDL_SetVideoMode(800, 600, 0, SDL_HWSURFACE|SDL_DOUBLEBUF);
  if (screen == NULL )
    {
      screen = SDL_SetVideoMode(800, 600, 16, SDL_SWSURFACE|SDL_ANYFORMAT);
      /* need flags for no double buffering */
    }
  if ( screen == NULL )
    {
      fprintf(stderr, "Couldn't set 800x600x16 video mode: %s\n",
              SDL_GetError());
      exit(1);
    }
  printf("Set 800x600 at %d bits-per-pixel mode\n",
         screen->format->BitsPerPixel);

  atexit(SDL_Quit);

  SDL_WM_SetCaption( "Fractint", NULL );
  SDL_WM_SetIcon(SDL_LoadBMP("fractint.ico"), NULL);

  ShowGIF ( "fract001.gif", screen );

  /* This is where the Fractint code starts */
  /* this traps non-math library floating point errors */
  signal( SIGFPE, my_floating_point_err );

  InitMemory();
  init_help();

restart:   /* insert key re-starts here */
  autobrowse     = FALSE;
  brwschecktype  = TRUE;
  brwscheckparms = TRUE;
  doublecaution  = TRUE;
  no_sub_images = FALSE;
  toosmall = 6;
  minbox   = 3;
  strcpy(browsemask,"*.gif");
  strcpy(browsename,"            ");
  name_stack_ptr= -1; /* init loaded files stack */

  evolving = FALSE;
  paramrangex = 4;
  opx = newopx = -2.0;
  paramrangey = 3;
  opy = newopy = -1.5;
  odpx = odpy = 0;
  gridsz = 9;
  fiddlefactor = 1;
  fiddle_reduction = 1.0;
  this_gen_rseed = (unsigned int)clock_ticks();
  srand(this_gen_rseed);
  initgene(); /*initialise pointers to lots of fractint variables for the evolution engine*/
  start_showorbit = 0;
  showdot = -1; /* turn off showdot if entered with <g> command */
  calc_status = -1;                    /* no active fractal image */

  fract_dir1 = getenv("FRACTDIR");
  if (fract_dir1==NULL)
    {
      fract_dir1 = ".";
    }
#ifdef SRCDIR
  fract_dir2 = SRCDIR;
#else
  fract_dir2 = ".";
#endif

  cmdfiles(argc,argv);                 /* process the command-line */

  memcpy(olddacbox,dacbox,256*3);      /* save in case colors= present */

  setvideotext();                      /* switch to text mode */

// following shows up a lot, is it really necessary??? JCO
  savedac = 0;                         /* don't save the VGA DAC */

  max_colors = 256;
  max_kbdcount = 80;   /* check the keyboard this often */

  browsing = FALSE;

  if (!functionpreloaded)
    set_if_old_bif();
  stacked = 0;

restorestart:
  if (colorpreloaded)
    memcpy(dacbox,olddacbox,256*3);   /* restore in case colors= present */

  lookatmouse = 0;                     /* ignore mouse */

  while (showfile <= 0)                /* image is to be loaded */
    {
      char *hdg;
      tabmode = 0;
      if (!browsing)     /*RB*/
        {
          if (overlay3d)
            {
              hdg = "Select File for 3D Overlay";
              helpmode = HELP3DOVLY;
            }
          else if (display3d)
            {
              hdg = "Select File for 3D Transform";
              helpmode = HELP3D;
            }
          else
            {
              hdg = "Select File to Restore";
              helpmode = HELPSAVEREST;
            }
          if (showfile < 0 && getafilename(hdg,gifmask,readname) < 0)
            {
              showfile = 1;               /* cancelled */
              initmode = -1;
              break;
            }

          name_stack_ptr = 0; /* 'r' reads first filename for browsing */
          strcpy(file_name_stack[name_stack_ptr],browsename);
        }

      evolving = viewwindow = 0;
      showfile = 0;
      helpmode = -1;
      tabmode = 1;
      if (stacked)
        {
          discardscreen();
          setvideotext();
          stacked = 0;
        }
      if (read_overlay() == 0)       /* read hdr, get video mode */
        break;                      /* got it, exit */
      if (browsing) /* break out of infinite loop, but lose your mind */
        showfile = 1;
      else
        showfile = -1;                 /* retry */
    }

  helpmode = HELPMENU;                 /* now use this help mode */
  tabmode = 1;
  lookatmouse = 0;                     /* ignore mouse */

  if (((overlay3d && !initbatch) || stacked) && initmode < 0)   /* overlay command failed */
    {
      unstackscreen();                  /* restore the graphics screen */
      stacked = 0;
      overlay3d = 0;                    /* forget overlays */
      display3d = 0;                    /* forget 3D */
      if (calc_status ==3)
        calc_status = 0;
      resumeflag = 1;
      goto resumeloop;                  /* ooh, this is ugly */
    }

  savedac = 0;                         /* don't save the VGA DAC */

imagestart:                          /* calc/display a new image */
  if (stacked)
    {
      discardscreen();
      stacked = 0;
    }

  usr_floatflag = 1;
  got_status = -1;                     /* for tab_display */

  if (showfile)
    if (calc_status > 0)              /* goto imagestart implies re-calc */
      calc_status = 0;

  if (initbatch == 0)
    lookatmouse = -PAGE_UP;           /* just mouse left button, == pgup */

  cyclelimit = initcyclelimit;         /* default cycle limit   */
  initmode = -1;                       /* (once)                   */

  zoomoff = 1;                 /* zooming is enabled */
  helpmode = HELPMAIN;         /* now use this help mode */
  resumeflag = 0;  /* allows taking goto inside big_while_loop() */

resumeloop:
  param_history(0); /* save old history */




  while (!done)
    {
      /* look for an event */
      if ( SDL_PollEvent ( &event ) )
        {
          /* an event was found */
          switch (event.type)
            {
            case SDL_QUIT:
              done = 1;
              break;
            case SDL_KEYDOWN:
              keypressed = event.key.keysym.sym;
              break;
            default:
              break;
            }
        }

      /* if keypressed go do something */

      switch (keypressed)
        {
        case SDLK_b:
          ShowBMP ( "fracttst.bmp", screen );
          break;
        case SDLK_c:
          quit_fractint( 0 );
          break;
        case SDLK_g:
          ShowGIF ( "fract001.gif", screen );
          break;
        default:
          break;
        }
      keypressed = 0;


      /* calculate fractal */
      /* display fractal */




      /* this switch processes gotos that are now inside function */
      switch (big_while_loop(&kbdmore,&stacked,resumeflag))
        {
        case RESTART:
          goto restart;
        case IMAGESTART:
          goto imagestart;
        case RESTORESTART:
          goto restorestart;
        default:
          break;
        }



    }/* end of while (!done) */

  return ( 0 );

}
