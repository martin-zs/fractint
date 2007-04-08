#ifndef EXTERNS_H
#define EXTERNS_H

#if 0
/* #ifndef DEBUG */
#define DEBUG 1
#endif


/* keep var names in column 30 for sorting via sort /+30 <in >out */
extern int					g_adapter;							/* index into g_video_table[] */
extern alternate_math		g_alternate_math[];					/* alternate math function pointers */
extern int					g_alternate_math_len;				/* number of alternate math */
extern int					g_ambient;							/* ambient= parameter value */
extern int					g_and_color;						/* AND mask for iteration to get color index */
extern struct MP			Ans;
extern int					g_a_plus_1_degree;
extern int					g_a_plus_1;
extern int					g_ask_video;
extern float				aspectdrift;
extern int					g_num_attractors;
extern int					g_atan_colors;
extern int					g_attractor_period[];
extern _CMPLX				g_attractors[];
extern int					autobrowse;
extern char					g_autokey_name[];
extern char					g_auto_show_dot;
extern int					AutoStereo_depth;
extern double				AutoStereo_width;
extern BYTE					g_back_color[];
extern int					g_bad_config;
extern int					bad_code_count;
extern int					bad_outside;
extern int					g_bad_value;
extern long					g_bail_out;
extern int					(*g_bail_out_fp)(void);
extern int					(*g_bail_out_l)(void);
extern int					(*g_bail_out_bn)(void);
extern int					(*g_bail_out_bf)(void);
extern enum bailouts		g_bail_out_test;
extern int					g_base_hertz;
extern int					g_basin;
extern int					bf_save_len;
extern int					g_bf_digits;
extern int					g_biomorph;
extern unsigned int			g_bits;
extern int					bitshift;
extern int					g_bit_shift_minus_1;
extern BYTE					block[];
extern int					g_blue_bright;
extern int					g_blue_crop_left;
extern int					g_blue_crop_right;
extern int					boxcolor;
extern int					boxcount;
extern int					boxvalues[];
extern int					boxx[];
extern int					boxy[];
extern char					browsemask[];
extern char					browsename[];
extern int					browsing;
extern char					brwscheckparms;
extern char					brwschecktype;
extern char					busy;
extern long					calctime;
extern long					(*g_calculate_mandelbrot_asm_fp)(void);
extern int					(*g_calculate_type)(void);
extern int					calc_status;
extern char					calibrate;
extern int					g_check_current_dir;
extern int					g_checked_vvs;
extern long					cimag;
extern double				g_close_enough;
extern double				g_proximity;
extern _CMPLX				g_coefficient;
extern int					g_col;
extern int					g_color;
extern char					g_color_file[];
extern long					g_color_iter;
extern int					g_color_preloaded;
extern int					colors;
extern int					g_color_state;
extern int					g_color_bright;					/* brightest color in palette */
extern int					g_color_dark;						/* darkest color in palette */
extern int					g_color_medium;					/* nearest to medbright grey in palette */
extern char					CommandComment[4][MAXCMT];
extern char					CommandFile[FILE_MAX_PATH];
extern char					CommandName[ITEMNAMELEN + 1];
extern int					comparegif;
extern long					con;
extern double				g_cos_x;
extern int					cpu;
extern long					creal;
extern int					g_current_col;
extern int					g_current_pass;
extern int					g_current_row;
extern int					g_cycle_limit;
extern int					g_c_exp;
extern double				g_degree_minus_1_over_degree;
extern BYTE					g_dac_box[256][3];
extern int					g_dac_count;
extern int					g_dac_learn;
extern double				ddelmin;
extern int					g_debug_flag;
extern int					decimals;
extern BYTE					decoderline[];
extern int					g_decomposition[];
extern int					g_degree;
extern long					delmin;
extern long					delx2;
extern long					delx;
extern LDBL					delxx2;
extern LDBL					delxx;
extern long					dely2;
extern long					dely;
extern LDBL					delyy2;
extern LDBL					delyy;
extern float				g_depth_fp;
extern unsigned long		g_diffusion_counter;
extern unsigned long		g_diffusion_limit;
extern int					disk16bit;
extern int					g_disk_flag;						/* disk video active flag */
extern int					disktarga;
extern int					g_display_3d;
extern long					g_distance_test;
extern int					g_distance_test_width;
extern float				g_dist_fp;
extern int					Distribution;
extern int					g_dither_flag;
extern int					g_dont_read_color;
extern int					dotmode;
extern int					doublecaution;
extern double				dpx;
extern double				dpy;
extern BYTE					g_stack[];
extern U16					dv_handle;
extern double				*dx0;
extern double				*dx1;
extern double				(_fastcall *g_dx_pixel)(void); /* set in FRACTALS.C */
extern double				dxsize;
extern double				*dy0;
extern double				*dy1;
extern double				(_fastcall *g_dy_pixel)(void); /* set in FRACTALS.C */
extern double				dysize;
extern int					escape_exit;
extern char					g_exe_path[];
extern BYTE					exitmode;
extern int					evolving;
extern void *				evolve_handle;
extern int					g_eye_separation;
extern float				g_eyes_fp;
extern int					fastrestore;
extern double				fgLimit;
extern long					g_one_fudge;
extern long					g_two_fudge;
extern int					gridsz;
extern double				fiddlefactor;
extern double				fiddle_reduction;
extern float				fileaspectratio;
extern int					filecolors;
extern int					filetype;
extern int					filexdots;
extern int					fileydots;
extern char					file_name_stack[16][FILE_MAX_FNAME];
extern int					g_fill_color;
extern float				finalaspectratio;
extern int					g_finite_attractor;
extern int					finishrow;
extern int					first_init;
extern int					g_first_saved_and;
extern int					g_float_flag;
extern _CMPLX				*g_float_parameter;
extern int					fm_attack;
extern int					fm_decay;
extern int					fm_release;
extern int					fm_sustain;
extern int					fm_wavetype;
extern int					fm_vol; /*volume of OPL-3 soundcard output*/
extern const BYTE			g_font_8x8[8][1024/8];
extern int					g_force_symmetry;
extern char					FormFileName[];
extern char					FormName[];
extern int					fpu;
extern int					fractype;
extern char					*fract_dir1;
extern char					*fract_dir2;
extern char					fromtext_flag;
extern long					fudge;
extern int					functionpreloaded;
extern double				f_at_rad;
extern double				g_f_radius;
extern double				g_f_x_center;
extern double				g_f_y_center;
extern GENEBASE				g_genes[NUMGENES];
extern int					get_corners(void);
extern int					g_gif87a_flag;
extern char					g_gif_mask[];
extern char					Glasses1Map[];
extern int					g_glasses_type;
extern int					g_good_mode;						/* video mode ok? */
extern int					g_got_real_dac;					/* loaddac worked, really got a dac */
extern int					g_got_status;
extern int					g_grayscale_depth;
extern char					GreyFile[];
extern int					hasinverse;
extern int					g_haze;
extern unsigned int			height;
extern float				g_height_fp;
extern int					helpmode;
extern int					hi_atten;
extern char					IFSFileName[];
extern char					IFSName[];
extern float				*ifs_defn;
extern int					ifs_type;
extern char					image_map;
extern int					g_init_3d[20];
extern _CMPLX				g_initial_z;
extern int					g_initialize_batch;
extern int					g_initial_cycle_limit;
extern int					g_init_mode;
extern _CMPLX				g_initial_orbit_z;
extern int					g_save_time;
extern int					g_inside;
extern char					insufficient_ifs_mem[];
extern int					integerfractal;
extern double				g_inversion[];
extern int					g_invert;
extern int					g_is_true_color;
extern int					g_is_mand;
extern int					g_x_stop;
extern int					g_y_stop;
extern char					*JIIMleftright[];
extern char					*JIIMmethod[];
extern int					g_juli_3D_mode;
extern char					*juli3Doptions[];
extern int					julibrot;
extern int					g_input_counter;
extern int					g_keep_screen_coords;
extern int					keybuffer;
extern int					LastInitOp;
extern unsigned				LastOp;
extern int					lastorbittype;
extern _LCMPLX				g_attractors_l[];
extern long					g_close_enough_l;
extern _LCMPLX				g_coefficient_l;
extern int					ldcheck;
extern char					LFileName[];
extern char					g_light_name[];
extern BYTE					*line_buff;
extern _LCMPLX				g_initial_z_l;
extern _LCMPLX				g_init_orbit_l;
extern long					linitx;
extern long					linity;
extern long					g_limit2_l;
extern long					g_limit_l;
extern long					g_magnitude_l;
extern char					LName[];
extern _LCMPLX				g_new_z_l;
extern int					loaded3d;
extern int					LodPtr;
extern int					Log_Auto_Calc;
extern int					Log_Calc;
extern int					Log_Fly_Calc;
extern long					LogFlag;
extern BYTE					*LogTable;
extern _LCMPLX				g_old_z_l;
extern _LCMPLX *			g_long_parameter;
extern int					lookatmouse;
extern _LCMPLX				g_parameter2_l;
extern _LCMPLX				g_parameter_l;
extern long					g_temp_sqr_x_l;
extern long					g_temp_sqr_y_l;
extern _LCMPLX				g_tmp_z_l;
extern long					*lx0;
extern long					*lx1;
extern long					(_fastcall *g_lx_pixel)(void); /* set in FRACTALS.C */
extern long					*ly0;
extern long					*ly1;
extern long					(_fastcall *g_ly_pixel)(void); /* set in FRACTALS.C */
extern int					lzw[2];
extern long					l_at_rad;
extern double				g_magnitude;
extern unsigned long		g_magnitude_limit;
extern enum Major			g_major_method;
extern BYTE					*g_map_dac_box;
extern int					mapset;
extern char					MAP_name[];
extern int					matherr_ct;
extern double				g_math_tolerance[2];
extern int					g_max_color;
extern long					g_max_count;
extern char					maxfn;
extern long					maxit;
extern int					maxlinelength;
extern long					MaxLTSize;
extern unsigned				Max_Args;
extern unsigned				Max_Ops;
extern long					maxptr;
extern int					max_colors;
extern int					g_max_input_counter;
extern int					maxhistory;
extern int					max_rhombus_depth;
extern int					minbox;
extern enum Minor			g_minor_method;
extern int					minstack;
extern int					minstackavail;
extern int					g_mode_7_text;						/* for egamono and hgc */
extern more_parameters		g_more_parameters[];
extern struct MP			g_a_plus_1_degree_mp;
extern struct MP			g_a_plus_1_mp;
extern struct MPC			MPCone;
extern struct MPC			*g_roots_mpc;
extern struct MPC			g_temp_parameter_mpc;
extern struct MP			mpd1overd;
extern struct MP			mpone;
extern int					MPOverflow;
extern struct MP			mproverd;
extern struct MP			mpt2;
extern struct MP			mpthreshold;
extern struct MP			mptmpparm2x;
extern double				g_m_x_max_fp;
extern double				g_m_x_min_fp;
extern double				g_m_y_max_fp;
extern double				g_m_y_min_fp;
extern int					name_stack_ptr;
extern _CMPLX				g_new_z;
extern char					newodpx;
extern char					newodpy;
extern double				newopx;
extern double				newopy;
extern int					g_new_orbit_type;
extern int					g_next_saved_incr;
extern int					no_sub_images;
extern int					g_no_magnitude_calculation;
extern int					nobof;
extern int					numaffine;
extern unsigned				numcolors;
extern const int			numtrigfn;
extern int					g_num_fractal_types;
extern int					g_num_work_list;
extern int					g_next_screen_flag;
extern int					Offset;
extern int					g_ok_to_print;
extern _CMPLX				g_old_z;
extern long					g_old_color_iter;
extern BYTE					olddacbox[256][3];
extern int					g_old_demm_colors;
extern char					old_stdcalcmode;
extern char					odpx;
extern char					odpy;
extern double				opx;
extern double				opy;
extern int					orbitsave;
extern int					g_orbit_color;
extern int					orbit_delay;
extern int					g_orbit_draw_mode;
extern long					g_orbit_interval;
extern int					g_orbit_index;
extern char					g_organize_formula_dir[];
extern int					g_organize_formula_search;
extern float				g_origin_fp;
extern int					(*outln) (BYTE *, int);
extern void					(*outln_cleanup)(void);
extern int					g_outside;
extern int					overflow;
extern int					g_overlay_3d;
extern int					g_fractal_overwrite;
extern double				g_orbit_x_3rd;
extern double				g_orbit_x_max;
extern double				g_orbit_x_min;
extern double				g_orbit_y_3rd;
extern double				g_orbit_y_max;
extern double				g_orbit_y_min;
extern double				param[];
extern double				paramrangex;
extern double				paramrangey;
extern double				parmzoom;
extern _CMPLX				g_parameter2;
extern _CMPLX				g_parameter;
extern int					g_passes;
extern int					g_patch_level;
extern int					g_periodicity_check;
extern struct fls			*pfls;
extern int					pixelpi;
extern void					(_fastcall *g_plot_color)(int,int,int);
extern double				plotmx1;
extern double				plotmx2;
extern double				plotmy1;
extern double				plotmy2;
extern int					polyphony;
extern unsigned				posp;
extern int					g_potential_16bit;
extern int					g_potential_flag;
extern double				potparam[];
#ifndef XFRACT
extern U16					prefix[];
#endif
extern int					g_preview;
extern int					g_preview_factor;
extern int					px;
extern int					py;
extern int					prmboxcount;
extern int					g_pseudo_x;
extern int					g_pseudo_y;
extern void					(_fastcall *g_put_color)(int,int,int);
extern _CMPLX				g_power;
extern double				g_quaternion_c;
extern double				g_quaternion_ci;
extern double				g_quaternion_cj;
extern double				g_quaternion_ck;
extern int					g_quick_calculate;
extern int					g_randomize;
extern int *				g_ranges;
extern int					g_ranges_length;
extern int					g_raytrace_brief;
extern int					g_raytrace_output;
extern char					g_ray_name[];
extern char					g_read_name[];
extern long					g_real_color_iter;
extern char					g_record_colors;
extern int					g_red_bright;
extern int					g_red_crop_left;
extern int					g_red_crop_right;
extern int					g_release;
extern int					resave_flag;
extern int					g_reset_periodicity;
extern char					*g_resume_info;
extern int					g_resume_length;
extern int					g_resuming;
extern int					g_random_flag;
extern char					rlebuf[];
extern int					rhombus_stack[];
extern int					g_root;
extern _CMPLX				*g_roots;
extern int					g_rotate_hi;
extern int					g_rotate_lo;
extern double				g_root_over_degree;
extern int					g_row;
extern int					g_row_count;						/* row-counter for decoder and out_line */
extern double				g_rq_limit2;
extern double				g_rq_limit;
extern int					g_random_seed;
extern long					savebase;
extern _CMPLX				SaveC;
extern int					savedac;
extern char					g_save_name[];
extern long					saveticks;
extern int					g_save_release;
extern int					save_system;
extern int					scale_map[];
extern float				screenaspect;
extern char					scrnfile[];
extern struct SearchPath	searchfor;
extern int					g_set_orbit_corners;
extern int					g_show_box;
extern int					g_show_dot;
extern int					g_show_file;
extern int					g_show_orbit;
extern double				g_sin_x;
extern int					g_size_dot;
extern short				sizeofstring[];
extern short				skipxdots;
extern short				skipydots;
extern int					g_slides;
extern int					Slope;
extern int					g_sound_flags;
extern int					sound_rollover;
extern char					speed_prompt[];
extern void					(_fastcall *g_standard_plot)(int x, int y, int color);
extern char					g_start_show_orbit;
extern int					started_resaves;
extern _CMPLX				g_static_roots[];
extern char					stdcalcmode;
extern char					stereomapname[];
extern int					StoPtr;
extern int					g_stop_pass;
extern unsigned int			strlocn[];
extern BYTE					suffix[];
extern double				sx3rd;
extern int					sxdots;
extern double				sxmax;
extern double				sxmin;
extern int					sxoffs;
extern double				sy3rd;
extern int					sydots;
extern double				symax;
extern double				symin;
extern int					g_symmetry;
extern int					syoffs;
extern char					s_makepar[];
extern int					tabmode;
extern int					taborhelp;
extern int					g_targa_output;
extern int					g_targa_overlay;
extern double				g_temp_sqr_x;
extern double				g_temp_sqr_y;
extern BYTE					teststring[];
extern int					g_text_cbase;						/* g_text_col is relative to this */
extern int					g_text_col;						/* current column in text mode */
extern int					g_text_rbase;						/* g_text_row is relative to this */
extern int					g_text_row;						/* current row in text mode */
extern unsigned int			this_gen_rseed;
extern unsigned *			tga16;
extern long					*tga32;
extern int					g_three_pass;
extern double				g_threshold;
extern int					timedsave;
extern int					g_timer_flag;
extern long					timer_interval;
extern long					timer_start;
extern _CMPLX				g_temp_z;
extern char					g_temp_dir[];
extern double				toosmall;
extern int					g_total_passes;
extern long					total_formula_mem;
extern int					transparent[];
extern BYTE					trigndx[];
extern int					g_true_color;
extern int					g_true_mode;
extern char					tstack[];
extern double				g_two_pi;
extern VOIDPTR				g_type_specific_work_area;
extern char					g_use_initial_orbit_z;
extern int					g_use_grid;	
extern int					g_use_center_mag;
extern short				uses_ismand;
extern short				uses_p1;
extern short				uses_p2;
extern short				uses_p3;
extern short				uses_p4;
extern short				uses_p5;
extern int					g_use_old_distance_test;
extern int					g_use_old_periodicity;
extern int					using_jiim;
extern int					g_user_biomorph;
extern long					usr_distest;
extern char					usr_floatflag;
extern int					usr_periodicitycheck;
extern char					usr_stdcalcmode;
extern struct videoinfo		g_video_entry;
extern VIDEOINFO			g_video_table[];
extern int					g_video_table_len;
extern int					g_video_type;						/* video adapter type */
extern VECTOR				g_view;
extern int					viewcrop;
extern float				viewreduction;
extern int					viewwindow;
extern int					viewxdots;
extern int					viewydots;
extern unsigned				vsp;
extern int					g_vxdots;
extern int					g_which_image;
extern float				g_width_fp;
extern char					g_work_dir[];
extern WORKLIST				g_work_list[MAXCALCWORK];
extern long					x3rd;
extern int					g_x_adjust;
extern double				xcjul;
extern int					xdots;
extern long					xmax;
extern long					xmin;
extern int					g_x_shift1;
extern int					g_x_shift;
extern int					g_x_trans;
extern double				xx3rd;
extern int					g_xx_adjust1;
extern int					g_xx_adjust;
extern double				xxmax;
extern double				xxmin;
extern long					XXOne;
extern int					g_xx_start;
extern int					g_xx_stop;
extern long					y3rd;
extern int					g_y_adjust;
extern double				ycjul;
extern int					ydots;
extern long					ymax;
extern long					ymin;
extern int					g_y_shift1;
extern int					g_y_shift;
extern int					g_y_trans;
extern double				yy3rd;
extern int					g_yy_adjust1;
extern int					g_yy_adjust;
extern double				yymax;
extern double				yymin;
extern int					g_yy_start;
extern int					g_yy_stop;
extern double				zbx;
extern double				zby;
extern double				zdepth;
extern int					g_z_dots;
extern int					zoomoff;
extern int					zrotate;
extern double				zskew;
extern double				zwidth;

#ifdef XFRACT
extern  int					fake_lut;
#endif

#endif
