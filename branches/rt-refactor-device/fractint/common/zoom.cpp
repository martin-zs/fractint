/*
	zoom.c - routines for zoombox manipulation and for panning

*/

#include <string.h>

/* see Fractint.c for a description of the "include"  hierarchy */
#include "port.h"
#include "prototyp.h"
#include "drivers.h"

#define PIXELROUND 0.00001

int g_box_x[NUM_BOXES] = { 0 };
int g_box_y[NUM_BOXES] = { 0 };
int g_box_values[NUM_BOXES] = { 0 };
int g_box_color;

static void _fastcall zmo_calc(double, double, double *, double *, double);
static void _fastcall zmo_calcbf(bf_t, bf_t, bf_t, bf_t, bf_t, bf_t, bf_t, bf_t, bf_t);
static int  check_pan(void);
static void fix_work_list(void);
static void _fastcall move_row(int fromrow, int torow, int col);

/* big number declarations */
static void calculate_corner(bf_t target, bf_t p1, double p2, bf_t p3, double p4, bf_t p5)
{
	bf_t btmp1 = alloc_stack(rbflength + 2);
	bf_t btmp2 = alloc_stack(rbflength + 2);
	bf_t btmp3 = alloc_stack(rbflength + 2);
	int saved = save_stack();

	/* use target as temporary variable */
	floattobf(btmp3, p2);
	mult_bf(btmp1, btmp3, p3);
	mult_bf(btmp2, floattobf(target, p4), p5);
	add_bf(target, btmp1, btmp2);
	add_a_bf(target, p1);
	restore_stack(saved);
}

#ifndef XFRACT
void display_box(void)
{
	int i;
	int boxc = (g_colors-1)&g_box_color;
	unsigned char *values = (unsigned char *)g_box_values;
	int rgb[3];
	for (i = 0; i < g_box_count; i++)
	{
		if (g_is_true_color && g_true_mode)
		{
			int alpha = 0;
			driver_get_truecolor(g_box_x[i]-g_sx_offset, g_box_y[i]-g_sy_offset, rgb[0], rgb[1], rgb[2], alpha);
			driver_put_truecolor(g_box_x[i]-g_sx_offset, g_box_y[i]-g_sy_offset,
				rgb[0]^255, rgb[1]^255, rgb[2]^255, 255);
		}
		else
		{
			values[i] = (unsigned char)getcolor(g_box_x[i]-g_sx_offset, g_box_y[i]-g_sy_offset);
		}
	}
	/* There is an interaction between getcolor and g_put_color, so separate them */
	if (!(g_is_true_color && g_true_mode)) /* don't need this for truecolor with truemode set */
	{
		for (i = 0; i < g_box_count; i++)
		{
			if (g_colors == 2)
			{
				g_put_color(g_box_x[i]-g_sx_offset, g_box_y[i]-g_sy_offset, (1 - values[i]));
			}
			else
			{
				g_put_color(g_box_x[i]-g_sx_offset, g_box_y[i]-g_sy_offset, boxc);
			}
		}
	}
}

void clear_box(void)
{
	int i;
	if (g_is_true_color && g_true_mode)
	{
		display_box();
	}
	else
	{
		unsigned char *values = (unsigned char *)g_box_values;
		for (i = 0; i < g_box_count; i++)
		{
			g_put_color(g_box_x[i]-g_sx_offset, g_box_y[i]-g_sy_offset, values[i]);
		}
	}
}
#endif

void zoom_box_draw(int drawit)
{
	struct coords tl, bl, tr, br; /* dot addr of topleft, botleft, etc */
	double tmpx, tmpy, dx, dy, rotcos, rotsin, ftemp1, ftemp2;
	double fxwidth, fxskew, fydepth, fyskew, fxadj;
	bf_t bffxwidth, bffxskew, bffydepth, bffyskew, bffxadj;
	int saved = 0;
	if (g_z_width == 0)  /* no box to draw */
	{
		if (g_box_count != 0)  /* remove the old box from display */
		{
			clear_box();
			g_box_count = 0;
		}
		reset_zoom_corners();
		return;
	}
	if (g_bf_math)
	{
		saved = save_stack();
		bffxwidth = alloc_stack(rbflength + 2);
		bffxskew  = alloc_stack(rbflength + 2);
		bffydepth = alloc_stack(rbflength + 2);
		bffyskew  = alloc_stack(rbflength + 2);
		bffxadj   = alloc_stack(rbflength + 2);
	}
	ftemp1 = PI*g_z_rotate/72; /* convert to radians */
	rotcos = cos(ftemp1);   /* sin & cos of rotation */
	rotsin = sin(ftemp1);

	/* do some calcs just once here to reduce fp work a bit */
	fxwidth = g_sx_max-g_sx_3rd;
	fxskew  = g_sx_3rd-g_sx_min;
	fydepth = g_sy_3rd-g_sy_max;
	fyskew  = g_sy_min-g_sy_3rd;
	fxadj   = g_z_width*g_z_skew;

	if (g_bf_math)
	{
		/* do some calcs just once here to reduce fp work a bit */
		sub_bf(bffxwidth, bfsxmax, bfsx3rd);
		sub_bf(bffxskew, bfsx3rd, bfsxmin);
		sub_bf(bffydepth, bfsy3rd, bfsymax);
		sub_bf(bffyskew, bfsymin, bfsy3rd);
		floattobf(bffxadj, fxadj);
	}

	/* calc co-ords of topleft & botright corners of box */
	tmpx = g_z_width/-2 + fxadj; /* from zoombox center as origin, on g_x_dots scale */
	tmpy = g_z_depth*g_final_aspect_ratio/2;
	dx = (rotcos*tmpx - rotsin*tmpy) - tmpx; /* delta x to rotate topleft */
	dy = tmpy - (rotsin*tmpx + rotcos*tmpy); /* delta y to rotate topleft */

	/* calc co-ords of topleft */
	ftemp1 = g_zbx + dx + fxadj;
	ftemp2 = g_zby + dy/g_final_aspect_ratio;

	tl.x   = (int)(ftemp1*(g_dx_size + PIXELROUND)); /* screen co-ords */
	tl.y   = (int)(ftemp2*(g_dy_size + PIXELROUND));
	g_xx_min  = g_sx_min + ftemp1*fxwidth + ftemp2*fxskew; /* real co-ords */
	g_yy_max  = g_sy_max + ftemp2*fydepth + ftemp1*fyskew;
	if (g_bf_math)
	{
		calculate_corner(bfxmin, bfsxmin, ftemp1, bffxwidth, ftemp2, bffxskew);
		calculate_corner(bfymax, bfsymax, ftemp2, bffydepth, ftemp1, bffyskew);
	}

	/* calc co-ords of bottom right */
	ftemp1 = g_zbx + g_z_width - dx - fxadj;
	ftemp2 = g_zby - dy/g_final_aspect_ratio + g_z_depth;
	br.x   = (int)(ftemp1*(g_dx_size + PIXELROUND));
	br.y   = (int)(ftemp2*(g_dy_size + PIXELROUND));
	g_xx_max  = g_sx_min + ftemp1*fxwidth + ftemp2*fxskew;
	g_yy_min  = g_sy_max + ftemp2*fydepth + ftemp1*fyskew;
	if (g_bf_math)
	{
		calculate_corner(bfxmax, bfsxmin, ftemp1, bffxwidth, ftemp2, bffxskew);
		calculate_corner(bfymin, bfsymax, ftemp2, bffydepth, ftemp1, bffyskew);
	}
	/* do the same for botleft & topright */
	tmpx = g_z_width/-2 - fxadj;
	tmpy = -tmpy;
	dx = (rotcos*tmpx - rotsin*tmpy) - tmpx;
	dy = tmpy - (rotsin*tmpx + rotcos*tmpy);
	ftemp1 = g_zbx + dx - fxadj;
	ftemp2 = g_zby + dy/g_final_aspect_ratio + g_z_depth;
	bl.x   = (int)(ftemp1*(g_dx_size + PIXELROUND));
	bl.y   = (int)(ftemp2*(g_dy_size + PIXELROUND));
	g_xx_3rd  = g_sx_min + ftemp1*fxwidth + ftemp2*fxskew;
	g_yy_3rd  = g_sy_max + ftemp2*fydepth + ftemp1*fyskew;
	if (g_bf_math)
	{
		calculate_corner(bfx3rd, bfsxmin, ftemp1, bffxwidth, ftemp2, bffxskew);
		calculate_corner(bfy3rd, bfsymax, ftemp2, bffydepth, ftemp1, bffyskew);
		restore_stack(saved);
	}
	ftemp1 = g_zbx + g_z_width - dx + fxadj;
	ftemp2 = g_zby - dy/g_final_aspect_ratio;
	tr.x   = (int)(ftemp1*(g_dx_size + PIXELROUND));
	tr.y   = (int)(ftemp2*(g_dy_size + PIXELROUND));

	if (g_box_count != 0)  /* remove the old box from display */
	{
		clear_box();
		g_box_count = 0;
	}

	if (drawit)  /* caller wants box drawn as well as co-ords calc'd */
	{
#ifndef XFRACT
		/* build the list of zoom box pixels */
		add_box(tl); add_box(tr);               /* corner pixels */
		add_box(bl); add_box(br);
		draw_lines(tl, tr, bl.x-tl.x, bl.y-tl.y); /* top & bottom lines */
		draw_lines(tl, bl, tr.x-tl.x, tr.y-tl.y); /* left & right lines */
#else
		g_box_x[0] = tl.x + g_sx_offset;
		g_box_y[0] = tl.y + g_sy_offset;
		g_box_x[1] = tr.x + g_sx_offset;
		g_box_y[1] = tr.y + g_sy_offset;
		g_box_x[2] = br.x + g_sx_offset;
		g_box_y[2] = br.y + g_sy_offset;
		g_box_x[3] = bl.x + g_sx_offset;
		g_box_y[3] = bl.y + g_sy_offset;
		g_box_count = 1;
#endif
		display_box();
		}
	}

void _fastcall draw_lines(struct coords fr, struct coords to,
						int dx, int dy)
{
	int xincr, yincr, ctr;
	int altctr, altdec, altinc;
	struct coords tmpp, line1, line2;

	if (abs(to.x-fr.x) > abs(to.y-fr.y))  /* delta.x > delta.y */
	{
		if (fr.x > to.x)  /* swap so from.x is < to.x */
		{
			tmpp = fr;
			fr = to;
			to = tmpp;
		}
		xincr = (to.x-fr.x)*4/g_screen_width + 1; /* do every 1st, 2nd, 3rd, or 4th dot */
		ctr = (to.x-fr.x-1)/xincr;
		altdec = abs(to.y-fr.y)*xincr;
		altinc = to.x-fr.x;
		altctr = altinc/2;
		yincr = (to.y > fr.y)?1:-1;
		line1.x = fr.x;
		line1.y = fr.y;
		line2.x = line1.x + dx;
		line2.y = line1.y + dy;
		while (--ctr >= 0)
		{
			line1.x += xincr;
			line2.x += xincr;
			altctr -= altdec;
			while (altctr < 0)
			{
				altctr  += altinc;
				line1.y += yincr;
				line2.y += yincr;
			}
			add_box(line1);
			add_box(line2);
			}
	}

	else  /* delta.y > delta.x */
	{
		if (fr.y > to.y)  /* swap so from.y is < to.y */
		{
			tmpp = fr;
			fr = to;
			to = tmpp;
		}
		yincr = (to.y-fr.y)*4/g_screen_height + 1; /* do every 1st, 2nd, 3rd, or 4th dot */
		ctr = (to.y-fr.y-1)/yincr;
		altdec = abs(to.x-fr.x)*yincr;
		altinc = to.y-fr.y;
		altctr = altinc/2;
		xincr = (to.x > fr.x) ? 1 : -1;
		line1.x = fr.x;
		line1.y = fr.y;
		line2.x = line1.x + dx;
		line2.y = line1.y + dy;
		while (--ctr >= 0)
		{
			line1.y += yincr;
			line2.y += yincr;
			altctr  -= altdec;
			while (altctr < 0)
			{
				altctr  += altinc;
				line1.x += xincr;
				line2.x += xincr;
			}
			add_box(line1);
			add_box(line2);
		}
	}
}

void _fastcall add_box(struct coords point)
{
#if defined(_WIN32)
	_ASSERTE(g_box_count < NUM_BOXES);
#endif
	point.x += g_sx_offset;
	point.y += g_sy_offset;
	if (point.x >= 0 && point.x < g_screen_width &&
		point.y >= 0 && point.y < g_screen_height)
	{
		g_box_x[g_box_count] = point.x;
		g_box_y[g_box_count] = point.y;
		++g_box_count;
	}
}

void zoom_box_move(double dx, double dy)
{   int align, row, col;
	align = check_pan();
	if (dx != 0.0)
	{
		g_zbx += dx;
		if (g_zbx + g_z_width/2 < 0)  /* center must stay onscreen */
		{
			g_zbx = g_z_width/-2;
		}
		if (g_zbx + g_z_width/2 > 1)
		{
			g_zbx = 1.0 - g_z_width/2;
		}
		if (align != 0
			&& ((col = (int)(g_zbx*(g_dx_size + PIXELROUND))) & (align-1)) != 0)
		{
			if (dx > 0)
			{
				col += align;
			}
			col -= col & (align-1); /* adjust col to pass alignment */
			g_zbx = (double)col/g_dx_size;
		}
	}
	if (dy != 0.0)
	{
		g_zby += dy;
		if (g_zby + g_z_depth/2 < 0)
		{
			g_zby = g_z_depth/-2;
		}
		if (g_zby + g_z_depth/2 > 1)
		{
			g_zby = 1.0 - g_z_depth/2;
		}
		if (align != 0
			&& ((row = (int)(g_zby*(g_dy_size + PIXELROUND))) & (align-1)) != 0)
		{
			if (dy > 0)
			{
				row += align;
			}
			row -= row & (align-1);
			g_zby = (double)row/g_dy_size;
		}
	}
	col = (int)((g_zbx + g_z_width/2)*(g_dx_size + PIXELROUND)) + g_sx_offset;
	row = (int)((g_zby + g_z_depth/2)*(g_dy_size + PIXELROUND)) + g_sy_offset;
}

static void _fastcall chgboxf(double dwidth, double ddepth)
{
	if (g_z_width + dwidth > 1)
	{
		dwidth = 1.0-g_z_width;
	}
	if (g_z_width + dwidth < 0.05)
	{
		dwidth = 0.05-g_z_width;
	}
	g_z_width += dwidth;
	if (g_z_depth + ddepth > 1)
	{
		ddepth = 1.0-g_z_depth;
	}
	if (g_z_depth + ddepth < 0.05)
	{
		ddepth = 0.05-g_z_depth;
	}
	g_z_depth += ddepth;
	zoom_box_move(dwidth/-2, ddepth/-2); /* keep it centered & check limits */
}

void zoom_box_resize(int steps)
{
	double deltax, deltay;
	if (g_z_depth*g_screen_aspect_ratio > g_z_width)  /* box larger on y axis */
	{
		deltay = steps*0.036 / g_screen_aspect_ratio;
		deltax = g_z_width*deltay / g_z_depth;
	}
	else  /* box larger on x axis */
	{
		deltax = steps*0.036;
		deltay = g_z_depth*deltax / g_z_width;
	}
	chgboxf(deltax, deltay);
}

void zoom_box_change_i(int dw, int dd)
{   /* change size by pixels */
	chgboxf((double) dw/g_dx_size, (double) dd/g_dy_size );
}

static void _fastcall zmo_calcbf(bf_t bfdx, bf_t bfdy,
	bf_t bfnewx, bf_t bfnewy, bf_t bfplotmx1, bf_t bfplotmx2, bf_t bfplotmy1,
	bf_t bfplotmy2, bf_t bfftemp)
{
	bf_t btmp1, btmp2, btmp3, btmp4, btempx, btempy ;
	bf_t btmp2a, btmp4a;
	int saved = save_stack();

	btmp1  = alloc_stack(rbflength + 2);
	btmp2  = alloc_stack(rbflength + 2);
	btmp3  = alloc_stack(rbflength + 2);
	btmp4  = alloc_stack(rbflength + 2);
	btmp2a = alloc_stack(rbflength + 2);
	btmp4a = alloc_stack(rbflength + 2);
	btempx = alloc_stack(rbflength + 2);
	btempy = alloc_stack(rbflength + 2);

	/* calc cur screen corner relative to zoombox, when zoombox co-ords
		are taken as (0, 0) topleft thru (1, 1) bottom right */

	/* tempx = dy*g_plot_mx1 - dx*g_plot_mx2; */
	mult_bf(btmp1, bfdy, bfplotmx1);
	mult_bf(btmp2, bfdx, bfplotmx2);
	sub_bf(btempx, btmp1, btmp2);

	/* tempy = dx*g_plot_my1 - dy*g_plot_my2; */
	mult_bf(btmp1, bfdx, bfplotmy1);
	mult_bf(btmp2, bfdy, bfplotmy2);
	sub_bf(btempy, btmp1, btmp2);

	/* calc new corner by extending from current screen corners */
	/* *newx = g_sx_min + tempx*(g_sx_max-g_sx_3rd)/ftemp + tempy*(g_sx_3rd-g_sx_min)/ftemp; */
	sub_bf(btmp1, bfsxmax, bfsx3rd);
	mult_bf(btmp2, btempx, btmp1);
	div_bf(btmp2a, btmp2, bfftemp);
	sub_bf(btmp3, bfsx3rd, bfsxmin);
	mult_bf(btmp4, btempy, btmp3);
	div_bf(btmp4a, btmp4, bfftemp);
	add_bf(bfnewx, bfsxmin, btmp2a);
	add_a_bf(bfnewx, btmp4a);

	/* *newy = g_sy_max + tempy*(g_sy_3rd-g_sy_max)/ftemp + tempx*(g_sy_min-g_sy_3rd)/ftemp; */
	sub_bf(btmp1, bfsy3rd, bfsymax);
	mult_bf(btmp2, btempy, btmp1);
	div_bf(btmp2a, btmp2, bfftemp);
	sub_bf(btmp3, bfsymin, bfsy3rd);
	mult_bf(btmp4, btempx, btmp3);
	div_bf(btmp4a, btmp4, bfftemp);
	add_bf(bfnewy, bfsymax, btmp2a);
	add_a_bf(bfnewy, btmp4a);
	restore_stack(saved);
}

static void _fastcall zmo_calc(double dx, double dy, double *newx, double *newy, double ftemp)
{
	double tempx, tempy;
	/* calc cur screen corner relative to zoombox, when zoombox co-ords
		are taken as (0, 0) topleft thru (1, 1) bottom right */
	tempx = dy*g_plot_mx1 - dx*g_plot_mx2;
	tempy = dx*g_plot_my1 - dy*g_plot_my2;

	/* calc new corner by extending from current screen corners */
	*newx = g_sx_min + tempx*(g_sx_max-g_sx_3rd)/ftemp + tempy*(g_sx_3rd-g_sx_min)/ftemp;
	*newy = g_sy_max + tempy*(g_sy_3rd-g_sy_max)/ftemp + tempx*(g_sy_min-g_sy_3rd)/ftemp;
}

static void zoom_out_bf(void) /* for ctl-enter, calc corners for zooming out */
{
	/* (g_xx_min, g_yy_max), etc, are already set to zoombox corners;
	(g_sx_min, g_sy_max), etc, are still the screen's corners;
	use the same logic as plot_orbit stuff to first calculate current screen
	corners relative to the zoombox, as if the zoombox were a square with
	upper left (0, 0) and width/depth 1; ie calc the current screen corners
	as if plotting them from the zoombox;
	then extend these co-ords from current real screen corners to get
	new actual corners
	*/
	bf_t savbfxmin, savbfymax, bfftemp;
	bf_t tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, bfplotmx1, bfplotmx2, bfplotmy1, bfplotmy2;
	int saved;
	saved = save_stack();
	savbfxmin = alloc_stack(rbflength + 2);
	savbfymax = alloc_stack(rbflength + 2);
	bfftemp   = alloc_stack(rbflength + 2);
	tmp1      = alloc_stack(rbflength + 2);
	tmp2      = alloc_stack(rbflength + 2);
	tmp3      = alloc_stack(rbflength + 2);
	tmp4      = alloc_stack(rbflength + 2);
	tmp5      = alloc_stack(rbflength + 2);
	tmp6      = alloc_stack(rbflength + 2);
	bfplotmx1 = alloc_stack(rbflength + 2);
	bfplotmx2 = alloc_stack(rbflength + 2);
	bfplotmy1 = alloc_stack(rbflength + 2);
	bfplotmy2 = alloc_stack(rbflength + 2);
	/* ftemp = (g_yy_min-g_yy_3rd)*(g_xx_3rd-g_xx_min) - (g_xx_max-g_xx_3rd)*(g_yy_3rd-g_yy_max); */
	sub_bf(tmp1, bfymin, bfy3rd);
	sub_bf(tmp2, bfx3rd, bfxmin);
	sub_bf(tmp3, bfxmax, bfx3rd);
	sub_bf(tmp4, bfy3rd, bfymax);
	mult_bf(tmp5, tmp1, tmp2);
	mult_bf(tmp6, tmp3, tmp4);
	sub_bf(bfftemp, tmp5, tmp6);
	/* g_plot_mx1 = (g_xx_3rd-g_xx_min); */ ; /* reuse the plotxxx vars is safe */
	copy_bf(bfplotmx1, tmp2);
	/* g_plot_mx2 = (g_yy_3rd-g_yy_max); */
	copy_bf(bfplotmx2, tmp4);
	/* g_plot_my1 = (g_yy_min-g_yy_3rd); */
	copy_bf(bfplotmy1, tmp1);
	/* g_plot_my2 = (g_xx_max-g_xx_3rd); */;
	copy_bf(bfplotmy2, tmp3);

	/* savxxmin = g_xx_min; savyymax = g_yy_max; */
	copy_bf(savbfxmin, bfxmin); 
	copy_bf(savbfymax, bfymax);

	sub_bf(tmp1, bfsxmin, savbfxmin);
	sub_bf(tmp2, bfsymax, savbfymax);
	zmo_calcbf(tmp1, tmp2, bfxmin, bfymax, bfplotmx1, bfplotmx2, bfplotmy1,
					bfplotmy2, bfftemp);
	sub_bf(tmp1, bfsxmax, savbfxmin);
	sub_bf(tmp2, bfsymin, savbfymax);
	zmo_calcbf(tmp1, tmp2, bfxmax, bfymin, bfplotmx1, bfplotmx2, bfplotmy1,
					bfplotmy2, bfftemp);
	sub_bf(tmp1, bfsx3rd, savbfxmin);
	sub_bf(tmp2, bfsy3rd, savbfymax);
	zmo_calcbf(tmp1, tmp2, bfx3rd, bfy3rd, bfplotmx1, bfplotmx2, bfplotmy1,
					bfplotmy2, bfftemp);
	restore_stack(saved);
}

static void zoom_out_double(void) /* for ctl-enter, calc corners for zooming out */
{
	/* (g_xx_min, g_yy_max), etc, are already set to zoombox corners;
		(g_sx_min, g_sy_max), etc, are still the screen's corners;
		use the same logic as plot_orbit stuff to first calculate current screen
		corners relative to the zoombox, as if the zoombox were a square with
		upper left (0, 0) and width/depth 1; ie calc the current screen corners
		as if plotting them from the zoombox;
		then extend these co-ords from current real screen corners to get
		new actual corners
		*/
	double savxxmin, savyymax, ftemp;
	ftemp = (g_yy_min-g_yy_3rd)*(g_xx_3rd-g_xx_min) - (g_xx_max-g_xx_3rd)*(g_yy_3rd-g_yy_max);
	g_plot_mx1 = (g_xx_3rd-g_xx_min); /* reuse the plotxxx vars is safe */
	g_plot_mx2 = (g_yy_3rd-g_yy_max);
	g_plot_my1 = (g_yy_min-g_yy_3rd);
	g_plot_my2 = (g_xx_max-g_xx_3rd);
	savxxmin = g_xx_min;
	savyymax = g_yy_max;
	zmo_calc(g_sx_min-savxxmin, g_sy_max-savyymax, &g_xx_min, &g_yy_max, ftemp);
	zmo_calc(g_sx_max-savxxmin, g_sy_min-savyymax, &g_xx_max, &g_yy_min, ftemp);
	zmo_calc(g_sx_3rd-savxxmin, g_sy_3rd-savyymax, &g_xx_3rd, &g_yy_3rd, ftemp);
}

void zoom_box_out(void) /* for ctl-enter, calc corners for zooming out */
{
	if (g_bf_math)
	{
		zoom_out_bf();
	}
	else
	{
		zoom_out_double();
	}
}

void aspect_ratio_crop(float oldaspect, float newaspect)
{
	double ftemp, xmargin, ymargin;
	if (newaspect > oldaspect)  /* new ratio is taller, crop x */
	{
		ftemp = (1.0 - oldaspect / newaspect) / 2;
		xmargin = (g_xx_max - g_xx_3rd)*ftemp;
		ymargin = (g_yy_min - g_yy_3rd)*ftemp;
		g_xx_3rd += xmargin;
		g_yy_3rd += ymargin;
		}
	else                        /* new ratio is wider, crop y */
	{
		ftemp = (1.0 - newaspect / oldaspect) / 2;
		xmargin = (g_xx_3rd - g_xx_min)*ftemp;
		ymargin = (g_yy_3rd - g_yy_max)*ftemp;
		g_xx_3rd -= xmargin;
		g_yy_3rd -= ymargin;
		}
	g_xx_min += xmargin;
	g_yy_max += ymargin;
	g_xx_max -= xmargin;
	g_yy_min -= ymargin;
}

static int check_pan(void) /* return 0 if can't, alignment requirement if can */
{
	int i, j;
	if ((g_calculation_status != CALCSTAT_RESUMABLE && g_calculation_status != CALCSTAT_COMPLETED) || g_evolving)
	{
		return 0; /* not resumable, not complete */
	}
	if (g_current_fractal_specific->calculate_type != standard_fractal
		&& g_current_fractal_specific->calculate_type != calculate_mandelbrot
		&& g_current_fractal_specific->calculate_type != calculate_mandelbrot_fp
		&& g_current_fractal_specific->calculate_type != lyapunov
		&& g_current_fractal_specific->calculate_type != froth_calc)
	{
		return 0; /* not a g_work_list-driven type */
	}
	if (g_z_width != 1.0 || g_z_depth != 1.0 || g_z_skew != 0.0 || g_z_rotate != 0.0)
	{
		return 0; /* not a full size unrotated unskewed zoombox */
	}
	if (g_standard_calculation_mode == 't')
	{
		return 0; /* tesselate, can't do it */
	}
	if (g_standard_calculation_mode == 'd')
	{
		return 0; /* diffusion scan: can't do it either */
	}
	if (g_standard_calculation_mode == 'o')
	{
		return 0; /* orbits, can't do it */
	}

	/* can pan if we get this far */

	if (g_calculation_status == CALCSTAT_COMPLETED)
	{
		return 1; /* image completed, align on any pixel */
	}
	if (g_potential_flag && g_potential_16bit)
	{
		return 1; /* 1 pass forced so align on any pixel */
	}
	if (g_standard_calculation_mode == 'b')
	{
		return 1; /* btm, align on any pixel */
	}
	if (g_standard_calculation_mode != 'g' || (g_current_fractal_specific->flags&NOGUESS))
	{
		if (g_standard_calculation_mode == '2' || g_standard_calculation_mode == '3') /* align on even pixel for 2pass */
		{
			return 2;
		}
		return 1; /* assume 1pass */
	}
	/* solid guessing */
	start_resume();
	get_resume(sizeof(g_num_work_list), &g_num_work_list, sizeof(g_work_list), g_work_list, 0);
	/* don't do end_resume! we're just looking */
	i = 9;
	for (j = 0; j < g_num_work_list; ++j) /* find lowest pass in any pending window */
	{
		if (g_work_list[j].pass < i)
		{
			i = g_work_list[j].pass;
		}
	}
	j = solid_guess_block_size(); /* worst-case alignment requirement */
	while (--i >= 0)
	{
		j = j >> 1; /* reduce requirement */
	}
	return j;
}

static void _fastcall move_row(int fromrow, int torow, int col)
/* move a row on the screen */
{
	int startcol, endcol, tocol;
	memset(g_stack, 0, g_x_dots); /* use g_stack as a temp for the row; clear it */
	if (fromrow >= 0 && fromrow < g_y_dots)
	{
		tocol = startcol = 0;
		endcol = g_x_dots - 1;
		if (col < 0)
		{
			tocol -= col;
			endcol += col;
		}
		if (col > 0)
		{
			startcol += col;
		}
		get_line(fromrow, startcol, endcol, (BYTE *)&g_stack[tocol]);
	}
	put_line(torow, 0, g_x_dots-1, (BYTE *)g_stack);
}

int init_pan_or_recalc(int do_zoomout) /* decide to recalc, or to chg g_work_list & pan */
{
	int i, j, row, col, y, alignmask, listfull;
	if (g_z_width == 0.0)
	{
		return 0; /* no zoombox, leave g_calculation_status as is */
	}
	/* got a zoombox */
	alignmask = check_pan()-1;
	if (alignmask < 0 || g_evolving)
	{
		g_calculation_status = CALCSTAT_PARAMS_CHANGED; /* can't pan, trigger recalc */
		return 0;
	}
	if (g_zbx == 0.0 && g_zby == 0.0)
	{
		clear_box();
		return 0; /* box is full screen, leave g_calculation_status as is */
	}
	col = (int)(g_zbx*(g_dx_size + PIXELROUND)); /* calc dest col, row of topleft pixel */
	row = (int)(g_zby*(g_dy_size + PIXELROUND));
	if (do_zoomout)  /* invert row and col */
	{
		row = -row;
		col = -col;
	}
	if ((row&alignmask) != 0 || (col&alignmask) != 0)
	{
		g_calculation_status = CALCSTAT_PARAMS_CHANGED; /* not on useable pixel alignment, trigger recalc */
		return 0;
	}
	/* pan */
	g_num_work_list = 0;
	if (g_calculation_status == CALCSTAT_RESUMABLE)
	{
		start_resume();
		get_resume(sizeof(g_num_work_list), &g_num_work_list, sizeof(g_work_list), g_work_list, 0);
	} /* don't do end_resume! we might still change our mind */
	/* adjust existing g_work_list entries */
	for (i = 0; i < g_num_work_list; ++i)
	{
		g_work_list[i].yy_start -= row;
		g_work_list[i].yy_stop  -= row;
		g_work_list[i].yy_begin -= row;
		g_work_list[i].xx_start -= col;
		g_work_list[i].xx_stop  -= col;
		g_work_list[i].xx_begin -= col;
	}
	/* add g_work_list entries for the new edges */
	listfull = i = 0;
	j = g_y_dots-1;
	if (row < 0)
	{
		listfull |= work_list_add(0, g_x_dots-1, 0, 0, -row-1, 0, 0, 0);
		i = -row;
	}
	if (row > 0)
	{
		listfull |= work_list_add(0, g_x_dots-1, 0, g_y_dots-row, g_y_dots-1, g_y_dots-row, 0, 0);
		j = g_y_dots - row - 1;
	}
	if (col < 0)
	{
		listfull |= work_list_add(0, -col-1, 0, i, j, i, 0, 0);
	}
	if (col > 0)
	{
		listfull |= work_list_add(g_x_dots-col, g_x_dots-1, g_x_dots-col, i, j, i, 0, 0);
	}
	if (listfull != 0)
	{
		if (stop_message(STOPMSG_CANCEL,
				"Tables full, can't pan current image.\n"
				"Cancel resumes old image, continue pans and calculates a new one."))
		{
			g_z_width = 0; /* cancel the zoombox */
			zoom_box_draw(1);
		}
		else
		{
			g_calculation_status = CALCSTAT_PARAMS_CHANGED; /* trigger recalc */
		}
		return 0;
	}
	/* now we're committed */
	g_calculation_status = CALCSTAT_RESUMABLE;
	clear_box();
	if (row > 0) /* move image up */
	{
		for (y = 0; y < g_y_dots; ++y)
		{
			move_row(y + row, y, col);
		}
	}
	else         /* move image down */
	{
		for (y = g_y_dots; --y >= 0; )
		{
			move_row(y + row, y, col);
		}
	}
	fix_work_list(); /* fixup any out of bounds g_work_list entries */
	alloc_resume(sizeof(g_work_list) + 20, 2); /* post the new g_work_list */
	put_resume(sizeof(g_num_work_list), &g_num_work_list, sizeof(g_work_list), g_work_list, 0);
	return 0;
}

static void _fastcall restart_window(int wknum)
/* force a g_work_list entry to restart */
{
	int yfrom, yto, xfrom, xto;
	yfrom = g_work_list[wknum].yy_start;
	if (yfrom < 0)
	{
		yfrom = 0;
	}
	xfrom = g_work_list[wknum].xx_start;
	if (xfrom < 0)
	{
		xfrom = 0;
	}
	yto = g_work_list[wknum].yy_stop;
	if (yto >= g_y_dots)
	{
		yto = g_y_dots - 1;
	}
	xto = g_work_list[wknum].xx_stop;
	if (xto >= g_x_dots)
	{
		xto = g_x_dots - 1;
	}
	memset(g_stack, 0, g_x_dots); /* use g_stack as a temp for the row; clear it */
	while (yfrom <= yto)
	{
		put_line(yfrom++, xfrom, xto, (BYTE *) g_stack);
	}
	g_work_list[wknum].sym = g_work_list[wknum].pass = 0;
	g_work_list[wknum].yy_begin = g_work_list[wknum].yy_start;
	g_work_list[wknum].xx_begin = g_work_list[wknum].xx_start;
}

static void fix_work_list(void) /* fix out of bounds and symmetry related stuff */
{   int i, j, k;
	WORKLIST *wk;
	for (i = 0; i < g_num_work_list; ++i)
	{
		wk = &g_work_list[i];
		if (wk->yy_start >= g_y_dots || wk->yy_stop < 0
			|| wk->xx_start >= g_x_dots || wk->xx_stop < 0)  /* offscreen, delete */
		{
			for (j = i + 1; j < g_num_work_list; ++j)
			{
				g_work_list[j-1] = g_work_list[j];
			}
			--g_num_work_list;
			--i;
			continue;
		}
		if (wk->yy_start < 0)  /* partly off top edge */
		{
			if ((wk->sym&1) == 0)  /* no sym, easy */
			{
				wk->yy_start = 0;
				wk->xx_begin = 0;
			}
			else  /* xaxis symmetry */
			{
				j = wk->yy_stop + wk->yy_start;
				if (j > 0 && g_num_work_list < MAXCALCWORK)  /* split the sym part */
				{
					g_work_list[g_num_work_list] = g_work_list[i];
					g_work_list[g_num_work_list].yy_start = 0;
					g_work_list[g_num_work_list++].yy_stop = j;
					wk->yy_start = j + 1;
				}
				else
				{
					wk->yy_start = 0;
				}
				restart_window(i); /* restart the no-longer sym part */
			}
		}
		if (wk->yy_stop >= g_y_dots)  /* partly off bottom edge */
		{
			j = g_y_dots-1;
			if ((wk->sym&1) != 0)  /* uses xaxis symmetry */
			{
				k = wk->yy_start + (wk->yy_stop - j);
				if (k < j)
				{
					if (g_num_work_list >= MAXCALCWORK) /* no room to split */
					{
						restart_window(i);
					}
					else  /* split it */
					{
						g_work_list[g_num_work_list] = g_work_list[i];
						g_work_list[g_num_work_list].yy_start = k;
						g_work_list[g_num_work_list++].yy_stop = j;
						j = k-1;
					}
				}
				wk->sym &= -1 - 1;
			}
			wk->yy_stop = j;
		}
		if (wk->xx_start < 0)  /* partly off left edge */
		{
			if ((wk->sym&2) == 0) /* no sym, easy */
				wk->xx_start = 0;
			else  /* yaxis symmetry */
			{
				j = wk->xx_stop + wk->xx_start;
				if (j > 0 && g_num_work_list < MAXCALCWORK)  /* split the sym part */
				{
					g_work_list[g_num_work_list] = g_work_list[i];
					g_work_list[g_num_work_list].xx_start = 0;
					g_work_list[g_num_work_list++].xx_stop = j;
					wk->xx_start = j + 1;
				}
				else
				{
					wk->xx_start = 0;
				}
				restart_window(i); /* restart the no-longer sym part */
			}
		}
		if (wk->xx_stop >= g_x_dots)  /* partly off right edge */
		{
			j = g_x_dots-1;
			if ((wk->sym&2) != 0)  /* uses xaxis symmetry */
			{
				k = wk->xx_start + (wk->xx_stop - j);
				if (k < j)
				{
					if (g_num_work_list >= MAXCALCWORK) /* no room to split */
					{
						restart_window(i);
					}
					else  /* split it */
					{
						g_work_list[g_num_work_list] = g_work_list[i];
						g_work_list[g_num_work_list].xx_start = k;
						g_work_list[g_num_work_list++].xx_stop = j;
						j = k-1;
					}
				}
				wk->sym &= -1 - 2;
			}
			wk->xx_stop = j;
		}
		if (wk->yy_begin < wk->yy_start)
		{
			wk->yy_begin = wk->yy_start;
		}
		if (wk->yy_begin > wk->yy_stop)
		{
			wk->yy_begin = wk->yy_stop;
		}
		if (wk->xx_begin < wk->xx_start)
		{
			wk->xx_begin = wk->xx_start;
		}
		if (wk->xx_begin > wk->xx_stop)
		{
			wk->xx_begin = wk->xx_stop;
		}
	}
	work_list_tidy(); /* combine where possible, re-sort */
}
