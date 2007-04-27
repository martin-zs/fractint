#ifndef EXTERNS_H
#define EXTERNS_H

#if 0
/* #ifndef DEBUG */
#define DEBUG 1
#endif

extern BYTE g_trig_index[];
extern struct trig_funct_lst trigfn[];
extern  void   (_fastcall *plot)(int, int, int);

/* keep var names in column 30 for sorting via sort /+30 <in >out */
extern int					g_adapter;							/* index into g_video_table[] */
extern alternate_math		g_alternate_math[];					/* alternate math function pointers */
extern int					g_alternate_math_len;				/* number of alternate math */
extern int					g_ambient;							/* ambient= parameter value */
extern int					g_and_color;						/* AND mask for iteration to get color index */
extern struct MP			g_ans;
extern int					g_ask_video;
extern float				g_aspect_drift;
extern int					g_num_attractors;
extern int					g_atan_colors;
extern int					g_attractor_period[];
extern _CMPLX				g_attractors[];
extern int					g_auto_browse;
extern char					g_autokey_name[];
extern char					g_auto_show_dot;
extern int					g_auto_stereo_depth;
extern double				g_auto_stereo_width;
extern BYTE					g_back_color[];
extern int					g_bad_config;
extern int					g_bad_code_count;
extern int					g_bad_outside;
extern int					g_bad_value;
extern long					g_bail_out;
extern int					(*g_bail_out_fp)(void);
extern int					(*g_bail_out_l)(void);
extern int					(*g_bail_out_bn)(void);
extern int					(*g_bail_out_bf)(void);
extern enum bailouts		g_bail_out_test;
extern int					g_base_hertz;
extern int					g_basin;
extern int					g_bf_save_len;
extern int					g_bf_digits;
extern int					g_biomorph;
extern unsigned int			g_bits;
extern int					g_bit_shift;
extern int					g_bit_shift_minus_1;
extern BYTE					g_block[];
extern int					g_blue_bright;
extern int					g_blue_crop_left;
extern int					g_blue_crop_right;
extern int					g_box_color;
extern int					g_box_count;
extern int					g_box_values[];
extern int					g_box_x[];
extern int					g_box_y[];
extern char					g_browse_mask[];
extern char					g_browse_name[];
extern int					g_browsing;
extern char					g_browse_check_parameters;
extern char					g_browse_check_type;
extern long					g_calculation_time;
extern long					(*g_calculate_mandelbrot_asm_fp)(void);
extern int					(*g_calculate_type)(void);
extern int					g_calculation_status;
extern char					g_calibrate;
extern int					g_cfg_line_nums[];
extern int					g_check_current_dir;
extern int					g_checked_vvs;
extern long					g_c_imag;
extern double				g_close_enough;
extern double				g_proximity;
extern _CMPLX				g_coefficient;
extern int					g_col;
extern int					g_color;
extern char					g_color_file[];
extern long					g_color_iter;
extern int					g_color_preloaded;
extern int					g_colors;
extern int					g_color_state;
extern int					g_color_bright;					/* brightest color in palette */
extern int					g_color_dark;						/* darkest color in palette */
extern int					g_color_medium;					/* nearest to medbright grey in palette */
extern char					g_command_comment[4][MAX_COMMENT];
extern char					g_command_file[FILE_MAX_PATH];
extern char					g_command_name[ITEMNAMELEN + 1];
extern int					g_compare_gif;
extern long					g_gaussian_constant;
extern double				g_cos_x;
extern int					g_cpu;
extern long					g_c_real;
extern int					g_current_col;
extern int					g_current_pass;
extern int					g_current_row;
extern int					g_cycle_limit;
extern int					g_c_exp;
extern BYTE					g_dac_box[256][3];
extern int					g_dac_count;
extern int					g_dac_learn;
extern double				g_delta_min_fp;
extern int					g_debug_flag;
extern int					g_decimals;
extern BYTE					g_decoder_line[];
extern int					g_decomposition[];
extern int					g_degree;
extern long					g_delta_min;
extern long					g_delta_x;
extern LDBL					g_delta_x_fp;
extern long					g_delta_x2;
extern LDBL					g_delta_x2_fp;
extern long					g_delta_y;
extern LDBL					g_delta_y_fp;
extern long					g_delta_y2;
extern LDBL					g_delta_y2_fp;
extern float				g_depth_fp;
extern unsigned long		g_diffusion_counter;
extern unsigned long		g_diffusion_limit;
extern int					g_disk_16bit;
extern int					g_disk_flag;						/* disk video active flag */
extern int					g_disk_targa;
extern int					g_display_3d;
extern long					g_distance_test;
extern int					g_distance_test_width;
extern float				g_screen_distance_fp;
extern int					g_gaussian_distribution;
extern int					g_dither_flag;
extern int					g_dont_read_color;
extern int					g_dot_mode;
extern int					g_double_caution;
extern double				g_delta_parameter_image_x;
extern double				g_delta_parameter_image_y;
extern struct DIR_SEARCH	g_dta;   /* Disk Transfer Area */
extern BYTE					g_stack[];
extern double				*g_x0;
extern double				*g_x1;
extern double				(_fastcall *g_dx_pixel)(void); /* set in FRACTALS.C */
extern double				g_dx_size;
extern double				*g_y0;
extern double				*g_y1;
extern double				(_fastcall *g_dy_pixel)(void); /* set in FRACTALS.C */
extern double				g_dy_size;
extern int					g_escape_exit_flag;
extern char					g_exe_path[];
extern int					g_evolving;
extern void					*g_evolve_handle;
extern int					g_eye_separation;
extern float				g_eyes_fp;
extern int					g_fast_restore;
extern double				g_fudge_limit;
extern long					g_one_fudge;
extern long					g_two_fudge;
extern int					g_grid_size;
extern double				g_fiddle_factor;
extern double				g_fiddle_reduction;
extern float				g_file_aspect_ratio;
extern int					g_file_colors;
extern int					g_file_type;
extern int					g_file_x_dots;
extern int					g_file_y_dots;
extern char					g_file_name_stack[16][FILE_MAX_FNAME];
extern int					g_fill_color;
extern float				g_final_aspect_ratio;
extern int					g_finite_attractor;
extern int					g_finish_row;
extern int					g_command_initialize;
extern int					g_first_saved_and;
extern int					g_float_flag;
extern _CMPLX				*g_float_parameter;
extern int					g_fm_attack;
extern int					g_fm_decay;
extern int					g_fm_release;
extern int					g_fm_sustain;
extern int					g_fm_wave_type;
extern int					g_fm_volume; /*volume of OPL-3 soundcard output*/
extern const BYTE			g_font_8x8[8][1024/8];
extern int					g_force_symmetry;
extern char					g_formula_filename[];
extern char					g_formula_name[];
extern int					g_fpu;
extern int					g_fractal_type;
extern char					*g_fract_dir1;
extern char					*g_fract_dir2;
extern char					g_from_text_flag;
extern long					g_fudge;
extern int					g_function_preloaded;
extern double				g_attractor_radius_fp;
extern double				g_f_radius;
extern double				g_f_x_center;
extern double				g_f_y_center;
extern GENEBASE				g_genes[NUMGENES];
extern int					g_gif87a_flag;
extern char					g_gif_mask[];
extern char					g_glasses1_map[];
extern int					g_glasses_type;
extern int					g_good_mode;						/* video mode ok? */
extern int					g_got_real_dac;					/* loaddac worked, really got a dac */
extern int					g_got_status;
extern int					g_grayscale_depth;
extern char					g_grey_file[];
extern int					g_has_inverse;
extern int					g_haze;
extern unsigned int			g_height;
extern float				g_height_fp;
extern int					g_note_attenuation;
extern char					g_ifs_filename[];
extern char					g_ifs_name[];
extern float				*g_ifs_definition;
extern int					g_ifs_type;
extern char					g_image_map;
extern int					g_init_3d[20];
extern _CMPLX				g_initial_z;
extern int					g_initialize_batch;
extern int					g_initial_cycle_limit;
extern int					g_init_mode;
extern _CMPLX				g_initial_orbit_z;
extern int					g_save_time;
extern int					g_inside;
extern char					g_insufficient_ifs_memory[];
extern int					g_integer_fractal;
extern double				g_inversion[];
extern int					g_invert;
extern int					g_is_true_color;
extern int					g_is_mand;
extern int					g_x_stop;
extern int					g_y_stop;
extern char					*g_jiim_left_right[];
extern char					*g_jiim_method[];
extern double				g_julia_c_x;
extern double				g_julia_c_y;
extern int					g_juli_3d_mode;
extern char					*g_juli_3d_options[];
extern int					g_julibrot;
extern int					g_input_counter;
extern int					g_keep_screen_coords;
extern int					g_last_init_op;
extern unsigned				g_last_op;
extern int					g_last_orbit_type;
extern _LCMPLX				g_attractors_l[];
extern long					g_close_enough_l;
extern _LCMPLX				g_coefficient_l;
extern int					g_use_old_complex_power;
extern char					g_l_system_filename[];
extern char					g_light_name[];
extern BYTE					*g_line_buffer;
extern _LCMPLX				g_initial_z_l;
extern _LCMPLX				g_init_orbit_l;
extern long					g_initial_x_l;
extern long					g_initial_y_l;
extern long					g_limit2_l;
extern long					g_limit_l;
extern long					g_magnitude_l;
extern char					g_l_system_name[];
extern _LCMPLX				g_new_z_l;
extern int					g_loaded_3d;
extern int					g_lod_ptr;
extern int					g_log_automatic_flag;
extern int					g_log_calculation;
extern int					g_log_dynamic_calculate;
extern long					g_log_palette_flag;
extern BYTE					*g_log_table;
extern _LCMPLX				g_old_z_l;
extern _LCMPLX				*g_long_parameter;
extern int					g_look_at_mouse;
extern _LCMPLX				g_parameter2_l;
extern _LCMPLX				g_parameter_l;
extern long					g_temp_sqr_x_l;
extern long					g_temp_sqr_y_l;
extern _LCMPLX				g_tmp_z_l;
extern long					*g_x0_l;
extern long					*g_x1_l;
extern long					(_fastcall *g_lx_pixel)(void); /* set in FRACTALS.C */
extern long					*g_y0_l;
extern long					*g_y1_l;
extern long					(_fastcall *g_ly_pixel)(void); /* set in FRACTALS.C */
extern long					g_attractor_radius_l;
extern void					(*g_trig0_l)(void);
extern void					(*g_trig1_l)(void);
extern void					(*g_trig2_l)(void);
extern void					(*g_trig3_l)(void);
extern void					(*g_trig0_d)(void);
extern void					(*g_trig1_d)(void);
extern void					(*g_trig2_d)(void);
extern void					(*g_trig3_d)(void);
extern void					(*g_trig0_m)(void);
extern void					(*g_trig1_m)(void);
extern void					(*g_trig2_m)(void);
extern void					(*g_trig3_m)(void);
extern double				g_magnitude;
extern unsigned long		g_magnitude_limit;
extern enum Major			g_major_method;
extern BYTE					*g_map_dac_box;
extern int					g_map_set;
extern char					g_map_name[];
extern int					g_math_error_count;
extern double				g_math_tolerance[2];
extern int					g_max_color;
extern long					g_max_count;
extern char					g_max_fn;
extern long					g_max_iteration;
extern int					g_max_line_length;
extern long					g_max_log_table_size;
extern unsigned				g_formula_max_args;
extern unsigned				g_formula_max_ops;
extern long					g_bn_max_stack;
extern int					g_max_colors;
extern int					g_max_input_counter;
extern int					g_max_history;
extern int					g_max_rhombus_depth;
extern int					g_cross_hair_box_size;
extern enum Minor			g_minor_method;
extern int					g_minimum_stack;
extern int					g_minimum_stack_available;
extern int					g_mode_7_text;						/* for egamono and hgc */
extern more_parameters		g_more_parameters[];
extern struct MPC			g_one_mpc;
extern struct MPC			*g_roots_mpc;
extern struct MPC			g_temp_parameter_mpc;
extern struct MP			g_one_mp;
extern int					g_overflow_mp;
extern struct MP			g_root_over_degree_mp;
extern struct MP			g_threshold_mp;
extern struct MP			g_parameter2_x_mp;
extern double				g_m_x_max_fp;
extern double				g_m_x_min_fp;
extern double				g_m_y_max_fp;
extern double				g_m_y_min_fp;
extern int					g_name_stack_ptr;
extern _CMPLX				g_new_z;
extern char					g_new_discrete_parameter_offset_x;
extern char					g_new_discrete_parameter_offset_y;
extern double				g_new_parameter_offset_x;
extern double				g_new_parameter_offset_y;
extern int					g_new_orbit_type;
extern int					g_next_saved_incr;
extern int					g_no_sub_images;
extern int					g_no_magnitude_calculation;
extern int					g_no_bof;
extern int					g_num_affine;
extern unsigned				g_num_colors;
extern const int			g_num_trig_fn;
extern int					g_num_fractal_types;
extern int					g_num_work_list;
extern int					g_next_screen_flag;
extern int					g_gaussian_offset;
extern int					g_ok_to_print;
extern _CMPLX				g_old_z;
extern long					g_old_color_iter;
extern BYTE					g_old_dac_box[256][3];
extern int					g_old_demm_colors;
extern char					g_standard_calculation_mode_old;
extern char					g_discrete_parameter_offset_x;
extern char					g_discrete_parameter_offset_y;
extern double				g_parameter_offset_x;
extern double				g_parameter_offset_y;
extern int					g_orbit_save;
extern int					g_orbit_color;
extern int					g_orbit_delay;
extern int					g_orbit_draw_mode;
extern long					g_orbit_interval;
extern int					g_orbit_index;
extern char					g_organize_formula_dir[];
extern int					g_organize_formula_search;
extern float				g_origin_fp;
extern int					(*g_out_line) (BYTE *, int);
extern void					(*g_out_line_cleanup)(void);
extern int					g_outside;
extern int					g_overflow;
extern int					g_overlay_3d;
extern int					g_fractal_overwrite;
extern double				g_orbit_x_3rd;
extern double				g_orbit_x_max;
extern double				g_orbit_x_min;
extern double				g_orbit_y_3rd;
extern double				g_orbit_y_max;
extern double				g_orbit_y_min;
extern double				g_parameters[];
extern double				g_parameter_range_x;
extern double				g_parameter_range_y;
extern double				g_parameter_zoom;
extern _CMPLX				g_parameter2;
extern _CMPLX				g_parameter;
extern unsigned				g_parser_vsp;
extern int					g_passes;
extern int					g_patch_level;
extern int					g_periodicity_check;
extern struct fls			*g_function_load_store_pointers;
extern void					(_fastcall *g_plot_color)(int,int,int);
extern double				g_plot_mx1;
extern double				g_plot_mx2;
extern double				g_plot_my1;
extern double				g_plot_my2;
extern int					g_polyphony;
extern unsigned				g_posp;
extern int					g_potential_16bit;
extern int					g_potential_flag;
extern double				g_potential_parameter[];
#ifndef XFRACT
extern U16					prefix[];
#endif
extern int					g_preview;
extern int					g_preview_factor;
extern int					g_px;
extern int					g_py;
extern int					g_parameter_box_count;
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
extern int					*g_ranges;
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
extern int					g_resave_flag;
extern int					g_reset_periodicity;
extern char					*g_resume_info;
extern int					g_resume_length;
extern int					g_resuming;
extern int					g_random_flag;
extern char					g_rle_buffer[];
extern int					g_rhombus_stack[];
extern int					g_root;
extern _CMPLX				*g_roots;
extern int					g_rotate_hi;
extern int					g_rotate_lo;
extern int					g_row;
extern int					g_row_count;						/* row-counter for decoder and out_line */
extern double				g_rq_limit2;
extern double				g_rq_limit;
extern int					g_random_seed;
extern long					g_save_base;
extern _CMPLX				g_save_c;
extern int					g_save_dac;
extern char					g_save_name[];
extern long					g_save_ticks;
extern int					g_save_release;
extern int					g_save_system;
extern int					g_scale_map[];
extern float				g_screen_aspect_ratio;
extern char					g_screen_file[];
extern int					g_screen_height;
extern int					g_screen_width;
extern struct search_path	g_search_for;
extern int					g_set_orbit_corners;
extern int					g_show_box;
extern int					g_show_dot;
extern int					g_show_file;
extern int					g_show_orbit;
extern double				g_sin_x;
extern int					g_size_dot;
extern short				g_size_of_string[];
extern short				g_skip_x_dots;
extern short				g_skip_y_dots;
extern int					g_slides;
extern int					g_gaussian_slope;
extern int					g_sound_flags;
extern int					g_sound_roll_over;
extern void					(_fastcall *g_standard_plot)(int x, int y, int color);
extern char					g_start_show_orbit;
extern int					g_started_resaves;
extern _CMPLX				g_static_roots[];
extern char					g_standard_calculation_mode;
extern char					g_stereo_map_name[];
extern int					g_store_ptr;
extern int					g_stop_pass;
extern unsigned int			g_string_location[];
extern BYTE					g_suffix[];
extern double				g_sx_3rd;
extern double				g_sx_max;
extern double				g_sx_min;
extern int					g_sx_offset;
extern double				g_sy_3rd;
extern double				g_sy_max;
extern double				g_sy_min;
extern int					g_symmetry;
extern int					g_sy_offset;
extern char					g_make_par[];
extern int					g_tab_mode;
extern int					g_targa_output;
extern int					g_targa_overlay;
extern double				g_temp_sqr_x;
extern double				g_temp_sqr_y;
extern int					g_text_cbase;						/* g_text_col is relative to this */
extern int					g_text_col;						/* current column in text mode */
extern BYTE					g_text_colors[];
extern int					g_text_rbase;						/* g_text_row is relative to this */
extern int					g_text_row;						/* current row in text mode */
extern unsigned int			g_this_generation_random_seed;
extern int					g_three_pass;
extern double				g_threshold;
extern int					g_timed_save;
extern int					g_timer_flag;
extern long					g_timer_interval;
extern long					g_timer_start;
extern _CMPLX				g_temp_z;
extern char					g_temp_dir[];
extern double				g_too_small;
extern int					g_total_passes;
extern long					g_total_formula_mem;
extern int					g_transparent[];
extern BYTE					g_trig_index[];
extern int					g_true_color;
extern int					g_true_mode;
extern char					g_text_stack[];
extern double				g_two_pi;
extern VOIDPTR				g_type_specific_work_area;
extern char					g_use_initial_orbit_z;
extern int					g_use_grid;	
extern int					g_use_center_mag;
extern short				g_uses_is_mand;
extern short				g_uses_p1;
extern short				g_uses_p2;
extern short				g_uses_p3;
extern short				g_uses_p4;
extern short				g_uses_p5;
extern int					g_use_old_distance_test;
extern int					g_use_old_periodicity;
extern int					g_using_jiim;
extern int					g_user_biomorph;
extern long					g_user_distance_test;
extern char					g_user_float_flag;
extern int					g_user_periodicity_check;
extern char					g_user_standard_calculation_mode;
extern struct video_info	g_video_entry;
extern VIDEOINFO			g_video_table[];
extern int					g_video_table_len;
extern int					g_video_type;						/* video adapter type */
extern VECTOR				g_view;
extern int					g_view_crop;
extern float				g_view_reduction;
extern int					g_view_window;
extern int					g_view_x_dots;
extern int					g_view_y_dots;
extern int					g_vx_dots;
extern int					g_which_image;
extern float				g_width_fp;
extern char					g_work_dir[];
extern WORKLIST				g_work_list[MAXCALCWORK];
extern long					g_x_3rd;
extern int					g_x_adjust;
extern int					g_x_dots;
extern long					g_x_max;
extern long					g_x_min;
extern int					g_x_shift1;
extern int					g_x_shift;
extern int					g_x_trans;
extern double				g_xx_3rd;
extern int					g_xx_adjust1;
extern int					g_xx_adjust;
extern double				g_xx_max;
extern double				g_xx_min;
extern long					g_xx_one;
extern int					g_xx_start;
extern int					g_xx_stop;
extern long					g_y_3rd;
extern int					g_y_adjust;
extern int					g_y_dots;
extern long					g_y_max;
extern long					g_y_min;
extern int					g_y_shift;
extern int					g_y_shift1;
extern int					g_y_trans;
extern double				g_yy_3rd;
extern int					g_yy_adjust;
extern int					g_yy_adjust1;
extern double				g_yy_max;
extern double				g_yy_min;
extern int					g_yy_start;
extern int					g_yy_stop;
extern double				g_zbx;
extern double				g_zby;
extern double				g_z_depth;
extern int					g_z_dots;
extern int					g_z_rotate;
extern double				g_z_skew;
extern double				g_z_width;
extern int					g_zoom_off;

#ifdef XFRACT
extern  int					g_fake_lut;
#endif

#endif
