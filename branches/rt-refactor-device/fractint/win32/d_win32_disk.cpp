/* d_win32_disk.c
 *
 * Routines for a Win32 disk video mode driver for fractint.
 */
#include <assert.h>
#include <stdio.h>

#define WIN32_LEAN_AND_MEAN
#define STRICT
#include <windows.h>

#include "port.h"
#include "prototyp.h"
#include "fractype.h"
#include "helpdefs.h"
#include "drivers.h"
#include "fihelp.h"

#include "WinText.h"
#include "frame.h"
#include "d_win32.h"
#include "ods.h"

#define DRAW_INTERVAL 6
#define TIMER_ID 1

/* read/write-a-dot/line routines */
typedef void t_dotwriter(int, int, int);
typedef int  t_dotreader(int, int);
typedef void t_linewriter(int y, int x, int lastx, BYTE *pixels);
typedef void t_linereader(int y, int x, int lastx, BYTE *pixels);

extern HINSTANCE g_instance;

extern void set_normal_line(void);
extern void set_disk_dot(void);

class Win32DiskDriver : public Win32BaseDriver
{
public:
	Win32DiskDriver(const char *name, const char *description);

	/* initialize the driver */			virtual int initialize(int *argc, char **argv);
	/* pause this driver */				virtual void pause();
	/* resume this driver */			virtual void resume();

	/* validate a fractint.cfg mode */	virtual int validate_mode(const VIDEOINFO &mode);
										virtual void set_video_mode(const VIDEOINFO &mode);
	/* find max screen extents */		virtual void get_max_screen(int &x_max, int &y_max) const;

	/* creates a window */				virtual void window();
	/* handles window resize.  */		virtual int resize();
	/* redraws the screen */			virtual void redraw();

	/* read palette into g_dac_box */	virtual int read_palette();
	/* write g_dac_box into palette */	virtual int write_palette();

	/* reads a single pixel */			virtual int read_pixel(int x, int y);
	/* writes a single pixel */			virtual void write_pixel(int x, int y, int color);
	/* reads a span of pixel */			virtual void read_span(int y, int x, int lastx, BYTE *pixels);
	/* writes a span of pixels */		virtual void write_span(int y, int x, int lastx, const BYTE *pixels);
										virtual void get_truecolor(int x, int y, int &r, int &g, int &b, int &a);
										virtual void put_truecolor(int x, int y, int r, int g, int b, int a);
	/* set copy/xor line */				virtual void set_line_mode(int mode);
	/* draw line */						virtual void draw_line(int x1, int y1, int x2, int y2, int color);
	/* draw string in graphics mode */	virtual void display_string(int x, int y, int fg, int bg, const char *text);
	/* save graphics */					virtual void save_graphics();
	/* restore graphics */				virtual void restore_graphics();
	/* poll or block for a key */		virtual int get_key();
										virtual void unget_key(int key);
										virtual int key_cursor(int row, int col);
										virtual int key_pressed();
										virtual int wait_key_pressed(int timeout);
										virtual void set_keyboard_timeout(int ms);
	/* set for text mode & save gfx */	virtual void set_for_text();
	/* restores graphics and data */	virtual void set_for_graphics();
	/* clears text screen */			virtual void set_clear();
	/* text screen functions */			virtual void move_cursor(int row, int col);
										virtual void hide_text_cursor();
										virtual void put_string(int row, int col, int attr, const char *text);
										virtual void set_attr(int row, int col, int attr, int count);
										virtual void scroll_up(int top, int bottom);
										virtual void stack_screen();
										virtual void unstack_screen();
										virtual void discard_screen();

	/* returns true if disk video */	virtual int diskp();

										virtual void flush();
	/* refresh alarm */					virtual void schedule_alarm(int secs);

private:
	int check_arg(char *argv);

	int m_width;
	int m_height;
	unsigned char m_clut[256][3];

	static VIDEOINFO s_modes[];
};

Win32DiskDriver::Win32DiskDriver(const char *name, const char *description)
	: Win32BaseDriver(name, description),
	m_width(0),
	m_height(0)
{
	for (int i = 0; i < 256; i++)
	{
		m_clut[i][0] = m_clut[i][1] = m_clut[i][2] = 0;
	}
}
	
/* VIDEOINFO:															*/
/*         char    name[26];       Adapter name (IBM EGA, etc)          */
/*         char    comment[26];    Comments (UNTESTED, etc)             */
/*         int     keynum;         key number used to invoked this mode */
/*                                 2-10 = F2-10, 11-40 = S, C, A{F1-F10}  */
/*         int     videomodeax;    begin with INT 10H, AX=(this)        */
/*         int     videomodebx;                 ...and BX=(this)        */
/*         int     videomodecx;                 ...and CX=(this)        */
/*         int     videomodedx;                 ...and DX=(this)        */
/*                                 NOTE:  IF AX==BX==CX==0, SEE BELOW   */
/*         int     dot_mode;        video access method used by asm code */
/*                                      1 == BIOS 10H, AH=12, 13 (SLOW)  */
/*                                      2 == access like EGA/VGA        */
/*                                      3 == access like MCGA           */
/*                                      4 == Tseng-like  SuperVGA*256   */
/*                                      5 == P'dise-like SuperVGA*256   */
/*                                      6 == Vega-like   SuperVGA*256   */
/*                                      7 == "Tweaked" IBM-VGA ...*256  */
/*                                      8 == "Tweaked" SuperVGA ...*256 */
/*                                      9 == Targa Format               */
/*                                      10 = Hercules                   */
/*                                      11 = "disk video" (no screen)   */
/*                                      12 = 8514/A                     */
/*                                      13 = CGA 320x200x4, 640x200x2   */
/*                                      14 = Tandy 1000                 */
/*                                      15 = TRIDENT  SuperVGA*256      */
/*                                      16 = Chips&Tech SuperVGA*256    */
/*         int     g_x_dots;          number of dots across the screen     */
/*         int     g_y_dots;          number of dots down the screen       */
/*         int     g_colors;         number of g_colors available           */

#define DRIVER_MODE(name_, comment_, key_, width_, height_, mode_) \
	{ name_, comment_, key_, 0, 0, 0, 0, mode_, width_, height_, 256 }
#define MODE19(n_, c_, k_, w_, h_) DRIVER_MODE(n_, c_, k_, w_, h_, 19)
VIDEOINFO Win32DiskDriver::s_modes[] =
{
	MODE19("Win32 Disk Video         ", "                        ", 0,  320,  240),
	MODE19("Win32 Disk Video         ", "                        ", 0,  400,  300),
	MODE19("Win32 Disk Video         ", "                        ", 0,  480,  360),
	MODE19("Win32 Disk Video         ", "                        ", 0,  600,  450),
	MODE19("Win32 Disk Video         ", "                        ", 0,  640,  480),
	MODE19("Win32 Disk Video         ", "                        ", 0,  800,  600),
	MODE19("Win32 Disk Video         ", "                        ", 0, 1024,  768),
	MODE19("Win32 Disk Video         ", "                        ", 0, 1200,  900),
	MODE19("Win32 Disk Video         ", "                        ", 0, 1280,  960),
	MODE19("Win32 Disk Video         ", "                        ", 0, 1400, 1050),
	MODE19("Win32 Disk Video         ", "                        ", 0, 1500, 1125),
	MODE19("Win32 Disk Video         ", "                        ", 0, 1600, 1200)
};
#undef MODE19
#undef DRIVER_MODE

/* check_arg
 *
 *	See if we want to do something with the argument.
 *
 * Results:
 *	Returns 1 if we parsed the argument.
 *
 * Side effects:
 *	Increments i if we use more than 1 argument.
 */
int Win32DiskDriver::check_arg(char *arg)
{
#if 0
	if (strcmp(arg, "-disk") == 0)
	{
		return 1;
	}
	else if (strcmp(arg, "-simple") == 0)
	{
		base.simple_input = 1;
		return 1;
	}
	else if (strcmp(arg, "-geometry") == 0 && *i + 1 < argc)
	{
		base.Xgeometry = argv[(*i) + 1];
		(*i)++;
		return 1;
	}
#endif

	return 0;
}

/*----------------------------------------------------------------------
*
* initdacbox --
*
* Put something nice in the dac.
*
* The conditions are:
*	Colors 1 and 2 should be bright so ifs fractals show up.
*	Color 15 should be bright for lsystem.
*	Color 1 should be bright for bifurcation.
*	Colors 1, 2, 3 should be distinct for periodicity.
*	The color map should look good for mandelbrot.
*
* Results:
*	None.
*
* Side effects:
*	Loads the dac.
*
*----------------------------------------------------------------------
*/
/* TODO: review case when COLOR_CHANNEL_MAX != 63 */
static void initdacbox()
{
	int i;
	for (i = 0; i < 256; i++)
	{
		g_dac_box[i][0] = (i >> 5)*8 + 7;
		g_dac_box[i][1] = (((i + 16) & 28) >> 2)*8 + 7;
		g_dac_box[i][2] = (((i + 2) & 3))*16 + 15;
	}
	g_dac_box[0][0] = g_dac_box[0][1] = g_dac_box[0][2] = 0;
	g_dac_box[1][0] = g_dac_box[1][1] = g_dac_box[1][2] = 255;
	g_dac_box[2][0] = 190; g_dac_box[2][1] = g_dac_box[2][2] = 255;
}

/* handle_help_tab
 *
 * Because we want context sensitive help to work everywhere, with the
 * help to display indicated by a non-zero value, we need
 * to trap the F1 key at a very low level.  The same is true of the
 * TAB display.
 *
 * What we do here is check for these keys and invoke their displays.
 * To avoid a recursive invoke of help(), a static is used to avoid
 * recursing on ourselves as help will invoke get key!
 */
static int handle_help_tab(int ch)
{
	static int inside_help = 0;

	if (FIK_F1 == ch && get_help_mode() && !inside_help)
	{
		inside_help = 1;
		help(0);
		inside_help = 0;
		ch = 0;
	}
	else if (FIK_TAB == ch && g_tab_mode)
	{
		int old_tab = g_tab_mode;
		g_tab_mode = 0;
		tab_display();
		g_tab_mode = old_tab;
		ch = 0;
	}

	return ch;
}

static void parse_geometry(const char *spec, int *x, int *y, int *width, int *height)
{
	/* do something like XParseGeometry() */
	if (2 == sscanf(spec, "%dx%d", width, height))
	{
		/* all we care about is width and height for disk output */
		*x = 0;
		*y = 0;
	}
}

/***********************************************************************
////////////////////////////////////////////////////////////////////////
\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
***********************************************************************/

/*----------------------------------------------------------------------
*
* init --
*
*	Initialize the windows and stuff.
*
* Results:
*	None.
*
* Side effects:
*	Initializes windows.
*
*----------------------------------------------------------------------
*/
int Win32DiskDriver::initialize(int *argc, char **argv)
{
	LPCSTR title = "FractInt for Windows";

	frame_init(g_instance, title);
	if (!wintext_initialize(&m_wintext, g_instance, NULL, title))
	{
		return FALSE;
	}

	initdacbox();

	/* filter out driver arguments */
	{
		int i;

		for (i = 0; i < *argc; i++)
		{
			if (check_arg(argv[i]))
			{
				int j;
				for (j = i; j < *argc-1; j++)
				{
					argv[j] = argv[j + 1];
				}
				argv[j] = NULL;
				--*argc;
			}
		}
	}

	/* add default list of video modes */
	{
		int m;
		for (m = 0; m < NUM_OF(s_modes); m++)
		{
			add_video_mode(this, s_modes[m]);
		}
	}

	return TRUE;
}

/* resize
 *
 * Check if we need resizing.  If no, return 0.
 * If yes, resize internal buffers and return 1.
 */
int Win32DiskDriver::resize()
{
	frame_resize(m_wintext.max_width, m_wintext.max_height);
	if ((g_video_table[g_adapter].x_dots == m_width)
		&& (g_video_table[g_adapter].y_dots == m_height))
	{
		return 0;
	}

	if (g_disk_flag)
	{
		disk_end();
	}
	disk_start();

	return !0;
}


/*----------------------------------------------------------------------
* read_palette
*
*	Reads the current video palette into g_dac_box.
*
*
* Results:
*	None.
*
* Side effects:
*	Fills in g_dac_box.
*
*----------------------------------------------------------------------
*/
int Win32DiskDriver::read_palette()
{
	ODS("disk_read_palette");
	if (g_got_real_dac == 0)
	{
		return -1;
	}
	for (int i = 0; i < 256; i++)
	{
		g_dac_box[i][0] = m_clut[i][0];
		g_dac_box[i][2] = m_clut[i][2];
	}
	return 0;
}

/*
*----------------------------------------------------------------------
*
* write_palette --
*	Writes g_dac_box into the video palette.
*
*
* Results:
*	None.
*
* Side effects:
*	Changes the displayed g_colors.
*
*----------------------------------------------------------------------
*/
int Win32DiskDriver::write_palette()
{
	int i;

	ODS("disk_write_palette");
	for (i = 0; i < 256; i++)
	{
		m_clut[i][0] = g_dac_box[i][0];
		m_clut[i][1] = g_dac_box[i][1];
		m_clut[i][2] = g_dac_box[i][2];
	}

	return 0;
}

/*
*----------------------------------------------------------------------
*
* schedule_alarm --
*
*	Start the refresh alarm
*
* Results:
*	None.
*
* Side effects:
*	Starts the alarm.
*
*----------------------------------------------------------------------
*/
void Win32DiskDriver::schedule_alarm(int soon)
{
	wintext_schedule_alarm(&m_wintext, (soon ? 1 : DRAW_INTERVAL)*1000);
}

/*
*----------------------------------------------------------------------
*
* disk_write_pixel --
*
*	Write a point to the screen
*
* Results:
*	None.
*
* Side effects:
*	Draws point.
*
*----------------------------------------------------------------------
*/
void Win32DiskDriver::write_pixel(int x, int y, int color)
{
	putcolor_a(x, y, color);
}

/*
*----------------------------------------------------------------------
*
* disk_read_pixel --
*
*	Read a point from the screen
*
* Results:
*	Value of point.
*
* Side effects:
*	None.
*
*----------------------------------------------------------------------
*/
int Win32DiskDriver::read_pixel(int x, int y)
{
	return getcolor(x, y);
}

/*
*----------------------------------------------------------------------
*
* disk_write_span --
*
*	Write a line of pixels to the screen.
*
* Results:
*	None.
*
* Side effects:
*	Draws pixels.
*
*----------------------------------------------------------------------
*/
void Win32DiskDriver::write_span(int y, int x, int lastx, const BYTE *pixels)
{
	int i;
	int width = lastx-x + 1;
	ODS3("disk_write_span (%d,%d,%d)", y, x, lastx);

	for (i = 0; i < width; i++)
	{
		write_pixel(x + i, y, pixels[i]);
	}
}

/*
*----------------------------------------------------------------------
*
* disk_read_span --
*
*	Reads a line of pixels from the screen.
*
* Results:
*	None.
*
* Side effects:
*	Gets pixels
*
*----------------------------------------------------------------------
*/
void Win32DiskDriver::read_span(int y, int x, int lastx, BYTE *pixels)
{
	int i, width;
	ODS3("disk_read_span (%d,%d,%d)", y, x, lastx);
	width = lastx-x + 1;
	for (i = 0; i < width; i++)
	{
		pixels[i] = read_pixel(x + i, y);
	}
}

void Win32DiskDriver::set_line_mode(int mode)
{
	ODS1("disk_set_line_mode %d", mode);
}

void Win32DiskDriver::draw_line(int x1, int y1, int x2, int y2, int color)
{
	ODS5("disk_draw_line (%d,%d) (%d,%d) %d", x1, y1, x2, y2, color);
	::draw_line(x1, y1, x2, y2, color);
}

/*
*----------------------------------------------------------------------
*
* redraw --
*
*	Refresh the screen.
*
* Results:
*	None.
*
* Side effects:
*	Redraws the screen.
*
*----------------------------------------------------------------------
*/
void Win32DiskDriver::redraw()
{
	ODS("disk_redraw");
	wintext_paintscreen(&m_wintext, 0, 80, 0, 25);
}

/* key_pressed
 *
 * Return 0 if no key has been pressed, or the FIK value if it has.
 * get_key() must still be called to eat the key; this routine
 * only peeks ahead.
 *
 * When a keystroke has been found by the underlying wintext_xxx
 * message pump, stash it in the one key buffer for later use by
 * get_key.
 */
int Win32DiskDriver::key_pressed()
{
	int ch = m_key_buffer;
	if (ch)
	{
		return ch;
	}
	ch = frame_get_key_press(0);
	ch = handle_help_tab(ch);
	m_key_buffer = ch;

	return ch;
}

/* unget_key
 *
 * Unread a key!  The key buffer is only one character deep, so we
 * assert if its already full.  This should never happen in real life :-).
 */
void Win32DiskDriver::unget_key(int key)
{
	_ASSERTE(0 == m_key_buffer);
	m_key_buffer = key;
}

/* get_key
 *
 * Get a keystroke, blocking if necessary.  First, check the key buffer
 * and if that's empty ask the wintext window to pump a keystroke for us.
 * If we get it, pass it off to handle tab and help displays.  If those
 * displays ate the key, then get another one.
 */
int Win32DiskDriver::get_key()
{
	int ch;

	do
	{
		if (m_key_buffer)
		{
			ch = m_key_buffer;
			m_key_buffer = 0;
		}
		else
		{
			ch = handle_help_tab(frame_get_key_press(1));
		}
	}
	while (ch == 0);

	return ch;
}

void Win32DiskDriver::window()
{
	frame_window(m_wintext.max_width, m_wintext.max_height);
	m_wintext.hWndParent = g_frame.window;
	wintext_texton(&m_wintext);
}

/*
; **************** Function setvideomode(ax, bx, cx, dx) ****************
;       This function sets the (alphanumeric or graphic) video mode
;       of the monitor.   Called with the proper values of AX thru DX.
;       No returned values, as there is no particular standard to
;       adhere to in this case.

;       (SPECIAL "TWEAKED" VGA VALUES:  if AX==BX==CX==0, assume we have a
;       genuine VGA or register compatable adapter and program the registers
;       directly using the coded value in DX)

; Unix: We ignore ax, bx, cx, dx.  g_dot_mode is the "mode" field in the video
; table.  We use mode 19 for the X window.
*/
void Win32DiskDriver::set_video_mode(const VIDEOINFO &mode)
{
	/* initially, set the virtual line to be the scan line length */
	g_vx_dots = g_screen_width;
	g_is_true_color = 0;				/* assume not truecolor */
	g_ok_to_print = FALSE;
	g_good_mode = 1;
	if (g_dot_mode != 0)
	{
		g_and_color = g_colors-1;
		g_box_count = 0;
		g_dac_learn = 1;
		g_dac_count = g_cycle_limit;
		g_got_real_dac = TRUE;

		read_palette();
	}

	resize();

	set_disk_dot();
	set_normal_line();
}

/*
; PUTSTR.asm puts a string directly to video display memory. Called from C by:
;    put_string(row, col, attr, string) where
;         row, col = row and column to start printing.
;         attr = color attribute.
;         string = pointer to the null terminated string to print.
;    Written for the A86 assembler (which has much less 'red tape' than MASM)
;    by Bob Montgomery, Orlando, Fla.             7-11-88
;    Adapted for MASM 5.1 by Tim Wegner          12-11-89
;    Furthur mucked up to handle graphics
;       video modes by Bert Tyler                 1-07-90
;    Reworked for:  row, col update/inherit;
;       620x200x2 inverse video;  ptr to string;
;       fix to avoid scrolling when last posn chgd;
;       divider removed;  newline ctl chars;  PB  9-25-90
*/
void Win32DiskDriver::put_string(int row, int col, int attr, const char *msg)
{
	if (-1 != row)
	{
		g_text_row = row;
	}
	if (-1 != col)
	{
		g_text_col = col;
	}
	{
		int abs_row = g_text_rbase + g_text_row;
		int abs_col = g_text_cbase + g_text_col;
		_ASSERTE(abs_row >= 0 && abs_row < WINTEXT_MAX_ROW);
		_ASSERTE(abs_col >= 0 && abs_col < WINTEXT_MAX_COL);
		wintext_putstring(&m_wintext, abs_col, abs_row, attr, msg, &g_text_row, &g_text_col);
	}
}

void Win32DiskDriver::set_clear()
{
	wintext_clear(&m_wintext);
}

/************** Function scrollup(toprow, botrow) ******************
*
*       Scroll the screen up (from toprow to botrow)
*/
void Win32DiskDriver::scroll_up(int top, int bot)
{
	ODS2("disk_scroll_up %d, %d", top, bot);
	wintext_scroll_up(&m_wintext, top, bot);
}

void Win32DiskDriver::display_string(int x, int y, int fg, int bg, const char *text)
{
}

void Win32DiskDriver::move_cursor(int row, int col)
{
	ODS2("disk_move_cursor %d,%d", row, col);

	if (row != -1)
	{
		m_cursor_row = row;
		g_text_row = row;
	}
	if (col != -1)
	{
		m_cursor_col = col;
		g_text_col = col;
	}
	row = m_cursor_row;
	col = m_cursor_col;
	wintext_cursor(&m_wintext, g_text_cbase + col, g_text_rbase + row, 1);
	m_cursor_shown = TRUE;
}

void Win32DiskDriver::set_attr(int row, int col, int attr, int count)
{
	if (-1 != row)
	{
		g_text_row = row;
	}
	if (-1 != col)
	{
		g_text_col = col;
	}
	wintext_set_attr(&m_wintext, g_text_rbase + g_text_row, g_text_cbase + g_text_col, attr, count);
}

void Win32DiskDriver::hide_text_cursor()
{
	if (TRUE == m_cursor_shown)
	{
		m_cursor_shown = FALSE;
		wintext_hide_cursor(&m_wintext);
	}
	ODS("disk_hide_text_cursor");
}

void Win32DiskDriver::set_for_text()
{
}

void Win32DiskDriver::set_for_graphics()
{
	hide_text_cursor();
}

/*
* Implement stack and unstack window functions by using multiple curses
* windows.
*/
void Win32DiskDriver::stack_screen()
{
	ODS("disk_stack_screen");

	m_saved_cursor[m_screen_count + 1] = g_text_row*80 + g_text_col;
	if (++m_screen_count)
	{
		/* already have some stacked */
		int i = m_screen_count - 1;

		_ASSERTE(i < WIN32_MAXSCREENS);
		if (i >= WIN32_MAXSCREENS)
		{
			/* bug, missing unstack? */
			stop_message(STOPMSG_NO_STACK, "stackscreen overflow");
			exit(1);
		}
		m_saved_screens[i] = wintext_screen_get(&m_wintext);
		set_clear();
	}
	else
	{
		set_for_text();
	}
}

void Win32DiskDriver::unstack_screen()
{
	ODS("disk_unstack_screen");
	_ASSERTE(m_screen_count >= 0);
	g_text_row = m_saved_cursor[m_screen_count] / 80;
	g_text_col = m_saved_cursor[m_screen_count] % 80;
	if (--m_screen_count >= 0)
	{ /* unstack */
		wintext_screen_set(&m_wintext, m_saved_screens[m_screen_count]);
		free(m_saved_screens[m_screen_count]);
		m_saved_screens[m_screen_count] = NULL;
	}
	else
	{
		set_for_graphics();
	}
	move_cursor(-1, -1);
}

void Win32DiskDriver::discard_screen()
{
	_ASSERTE(m_screen_count > 0);
	if (--m_screen_count >= 0)
	{ /* unstack */
		if (m_saved_screens[m_screen_count])
		{
			free(m_saved_screens[m_screen_count]);
			m_saved_screens[m_screen_count] = NULL;
		}
	}
}

int Win32DiskDriver::diskp()
{
	return 1;
}

int Win32DiskDriver::key_cursor(int row, int col)
{
	int result;

	ODS2("disk_key_cursor %d,%d", row, col);
	if (-1 != row)
	{
		m_cursor_row = row;
		g_text_row = row;
	}
	if (-1 != col)
	{
		m_cursor_col = col;
		g_text_col = col;
	}

	col = m_cursor_col;
	row = m_cursor_row;

	if (key_pressed())
	{
		result = get_key();
	}
	else
	{
		m_cursor_shown = TRUE;
		wintext_cursor(&m_wintext, col, row, 1);
		result = get_key();
		hide_text_cursor();
		m_cursor_shown = FALSE;
	}

	return result;
}

int Win32DiskDriver::wait_key_pressed(int timeout)
{
	int count = 10;
	while (!key_pressed())
	{
		Sleep(25);
		if (timeout && (--count == 0))
		{
			break;
		}
	}

	return key_pressed();
}

int Win32DiskDriver::validate_mode(const VIDEOINFO &mode)
{
	/* allow modes of any size with 256 g_colors and g_dot_mode = 19
	   ax/bx/cx/dx must be zero. */
	return (mode.colors == 256) &&
		(mode.videomodeax == 0) &&
		(mode.videomodebx == 0) &&
		(mode.videomodecx == 0) &&
		(mode.videomodedx == 0) &&
		(mode.dotmode == 19);
}

void Win32DiskDriver::pause()
{
	if (m_wintext.hWndCopy)
	{
		ShowWindow(m_wintext.hWndCopy, SW_HIDE);
	}
}

void Win32DiskDriver::resume()
{
	if (!m_wintext.hWndCopy)
	{
		window();
	}

	if (m_wintext.hWndCopy)
	{
		ShowWindow(m_wintext.hWndCopy, SW_NORMAL);
	}
	wintext_resume(&m_wintext);
}

void Win32DiskDriver::get_truecolor(int x, int y, int &r, int &g, int &b, int &a)
{
}

void Win32DiskDriver::put_truecolor(int x, int y, int r, int g, int b, int a)
{
}

void Win32DiskDriver::save_graphics()
{
}

void Win32DiskDriver::restore_graphics()
{
}

void Win32DiskDriver::get_max_screen(int &x_max, int &y_max) const
{
	x_max = -1;
	y_max = -1;
}

void Win32DiskDriver::set_keyboard_timeout(int ms)
{
}

void Win32DiskDriver::flush()
{
}

Win32DiskDriver disk_driver_info("disk", "A disk video driver for 32-bit Windows.");

AbstractDriver *disk_driver = &disk_driver_info;
