/*
        This module consists only of the fractalspecific structure
        and a *slew* of defines needed to get it to compile
*/

/* includes needed for fractalspecific */

  /* see Fractint.c for a description of the "include"  hierarchy */
#include "port.h"
#include "prototyp.h"
#include "helpdefs.h"
#include "fractype.h"

/* functions defined elswhere needed for fractalspecific */
/* moved to prototyp.h */

/* parameter descriptions */
/* Note: parameters preceded by + are integer parameters */
/*       parameters preceded by # are U32 parameters */

/* for Mandelbrots */
static char far realz0[] = "Real Perturbation of Z(0)";
static char far imagz0[] = "Imaginary Perturbation of Z(0)";

/* for Julias */
static char far realparm[] = "Real Part of Parameter";
static char far imagparm[] = "Imaginary Part of Parameter";

/* for Newtons */
static char far newtdegree[] = "+Polynomial Degree (>= 2)";

/* for MarksMandel/Julia */
static char far exponent[]   = "Real part of Exponent";
static char far imexponent[] = "Imag part of Exponent";

/* for Complex Newton */
static char far realroot[]   = "Real part of Root";
static char far imagroot[]   = "Imag part of Root";
static char far realdegree[] = "Real part of Degree";
static char far imagdegree[] = "Imag part of Degree";

/* for Lorenz */
static char far timestep[]     = "Time Step";

/* for formula */
static char far p1real[] = "Real portion of p1";
static char far p2real[] = "Real portion of p2";
static char far p3real[] = "Real portion of p3";
static char far p4real[] = "Real portion of p4";
static char far p5real[] = "Real portion of p5";
static char far p1imag[] = "Imaginary portion of p1";
static char far p2imag[] = "Imaginary portion of p2";
static char far p3imag[] = "Imaginary portion of p3";
static char far p4imag[] = "Imaginary portion of p4";
static char far p5imag[] = "Imaginary portion of p5";

/* trig functions */
static char far recoeftrg1[] = "Real Coefficient First Function";
static char far imcoeftrg1[] = "Imag Coefficient First Function";
static char far recoeftrg2[] = "Real Coefficient Second Function";
static char far imcoeftrg2[] = "Imag Coefficient Second Function";

/* MCP 7-7-91
static char far recoefsqr[] = "Real Coefficient Square Term";
static char far imcoefsqr[] = "Imag Coefficient Square Term";
*/

static char far recoef2nd[] = "Real Coefficient Second Term";
static char far imcoef2nd[] = "Imag Coefficient Second Term";

/* KAM Torus */
static char far kamangle[] = "Angle (radians)";
static char far kamstep[] =  "Step size";
static char far kamstop[] =  "Stop value";
static char far pointsperorbit[] = "+Points per orbit";

/* Newtbasin */
static char far stripes[] = "Enter non-zero value for stripes";

/* Gingerbreadman */
static char far initx[] = "Initial x";
static char far inity[] = "Initial y";

/* popcorn and julia popcorn generalized */
static char far step_x[] = "Step size (real)";
static char far step_y[] = "Step size (imaginary)";
static char far constant_x[] = "Constant C (real)";
static char far constant_y[] = "Constant C (imaginary)";

/* bifurcations */
static char far filt[] = "+Filter Cycles";
static char far seed[] = "Seed Population";

/* frothy basins */
static char far frothmapping[] = "+Apply mapping once (1) or twice (2)";
static char far frothshade[] =  "+Enter non-zero value for alternate color shading";
static char far frothavalue[] =  "A (imaginary part of C)";

/* symmetrical icon fractals */
static char far s_lambda[] = "Lambda";
static char far s_alpha[]  = "Alpha";
static char far s_beta[]   = "Beta";
static char far s_gamma[]  = "Gamma";
static char far s_omega[]  = "Omega";
static char far symdegree[] = "+Degree of symmetry";

static char far shiftval[] = "Function Shift Value";

/* plasma and ant */

static char far s_randomseed[] = "+Random Seed Value (0 = Random, 1 = Reuse Last)";

/* ifs */
static char far color_method[] = "+Coloring method (0,1)";

/* orbit fractals */
static char A[] = "a";
static char B[] = "b";
static char D[] = "d";
static char H[] = "h";
static char P[] = "p";

/* 4D fractals */
static char C[] = "c";
static char C1[] = "c1";
static char CI[] = "ci";
static char CJ[] = "cj";
static char CK[] = "ck";
static char ZJ[] = "zj";
static char ZK[] = "zk";
static char far notused[] = "notused";
/* phoenix fractals */
static char far degreeZ[] = "Degree = 0 | >= 2 | <= -3";

/* empty string */
static char far ES[] = "";

/* julia inverse */
static char far s_maxhits[] = "Max Hits per Pixel";
#ifdef RANDOM_RUN
static char far randomruninterval[] = "Random Run Interval";
#endif
/* halley */
static char far order[] = {"+Order (integer > 1)"};
static char far real_relax[] = {"Real Relaxation coefficient"};
static char far epsilon[] = {"Epsilon"};
static char far imag_relax[] = {"Imag Relaxation coefficient"};
/* cellular */
static char far cell_init[] = {"#Initial String | 0 = Random | -1 = Reuse Last Random"};
static char far cell_rule[] = {"#Rule = # of digits (see below) | 0 = Random"};
static char far cell_type[] = {"+Type (see below)"};
static char far cell_strt[] = {"#Starting Row Number"};

/* bailout defines */
#define FTRIGBAILOUT 2500
#define LTRIGBAILOUT   64
#define FROTHBAILOUT    7
#define STDBAILOUT      4
#define NOBAILOUT       0

MOREPARAMS moreparams[] =
{
    {ICON             ,{ s_omega, symdegree,   ES,ES,ES,ES},{0,3,0,0,0,0}},
    {ICON3D           ,{ s_omega, symdegree,   ES,ES,ES,ES},{0,3,0,0,0,0}},
    {HYPERCMPLXJFP    ,{ ZJ,      ZK,          ES,ES,ES,ES},{0,0,0,0,0,0}},
    {QUATJULFP        ,{ ZJ,      ZK,          ES,ES,ES,ES},{0,0,0,0,0,0}},
    {PHOENIXFPCPLX    ,{ degreeZ, ES,          ES,ES,ES,ES},{0,0,0,0,0,0}},
    {MANDPHOENIXFPCPLX,{ degreeZ, ES,          ES,ES,ES,ES},{0,0,0,0,0,0}},
    {FFORMULA ,{ p3real,p3imag,p4real,p4imag,p5real,p5imag},{0,0,0,0,0,0}},
    {ANT              ,{ "+Wrap?",s_randomseed,ES,ES,ES,ES},{1,0,0,0,0,0}},
    {-1               ,{ NULL,NULL,NULL,NULL,NULL,NULL    },{0,0,0,0,0,0}}
};

/*
     type math orbitcalc fnct per_pixel fnct per_image fnct
   |-----|----|--------------|--------------|--------------| */
struct alternatemathstuff far alternatemath[] =
{
   {JULIAFP, 1,JuliabnFractal,juliabn_per_pixel,  MandelbnSetup},
   {MANDELFP,1,JuliabnFractal,mandelbn_per_pixel, MandelbnSetup},
/*
NOTE: The default precision for bf_math=BIGNUM is not high enough
      for JuliaZpowerbnFractal.  If you want to test BIGNUM (1) instead
      of the usual BIGFLT (2), then set bfdigits on the command to
      increase the precision.
*/
   {FPJULIAZPOWER,2,JuliaZpowerbfFractal,juliabf_per_pixel, MandelbfSetup  },
   {FPMANDELZPOWER,2,JuliaZpowerbfFractal,mandelbf_per_pixel, MandelbfSetup},
   {-1,            0,NULL,                NULL,               NULL         }
};

/* These are only needed for types with both integer and float variations */
char t_barnsleyj1[]= "barnsleyj1";
char t_barnsleyj2[]= "barnsleyj2";
char t_barnsleyj3[]= "barnsleyj3";
char t_barnsleym1[]= "barnsleym1";
char t_barnsleym2[]= "barnsleym2";
char t_barnsleym3[]= "barnsleym3";
char t_bifplussinpi[]= "bif+sinpi";
char t_bifeqsinpi[]= "bif=sinpi";
char t_biflambda[]= "biflambda";
char t_bifmay[]= "bifmay";
char t_bifstewart[]= "bifstewart";
char t_bifurcation[]= "bifurcation";
char t_fn_z_plusfn_pix_[]= "fn(z)+fn(pix)";
char t_fn_zz_[]= "fn(z*z)";
char t_fnfn[]= "fn*fn";
char t_fnzplusz[]= "fn*z+z";
char t_fnplusfn[]= "fn+fn";
char t_formula[]= "formula";
char t_henon[]= "henon";
char t_ifs3d[]= "ifs3d";
char t_julfnplusexp[]= "julfn+exp";
char t_julfnpluszsqrd[]= "julfn+zsqrd";
char t_julia[]= "julia";
char t_julia_fnorfn_[]= "julia(fn||fn)";
char t_julia4[]= "julia4";
char t_julia_inverse[]= "julia_inverse";
char t_julibrot[]= "julibrot";
char t_julzpower[]= "julzpower";
char t_kamtorus[]= "kamtorus";
char t_kamtorus3d[]= "kamtorus3d";
char t_lambda[]= "lambda";
char t_lambda_fnorfn_[]= "lambda(fn||fn)";
char t_lambdafn[]= "lambdafn";
char t_lorenz[]= "lorenz";
char t_lorenz3d[]= "lorenz3d";
char t_mandel[]= "mandel";
char t_mandel_fnorfn_[]= "mandel(fn||fn)";
char t_mandel4[]= "mandel4";
char t_mandelfn[]= "mandelfn";
char t_mandellambda[]= "mandellambda";
char t_mandphoenix[]= "mandphoenix";
char t_mandphoenixcplx[]= "mandphoenixclx";
char t_manfnplusexp[]= "manfn+exp";
char t_manfnpluszsqrd[]= "manfn+zsqrd";
char t_manlam_fnorfn_[]= "manlam(fn||fn)";
char t_manowar[]= "manowar";
char t_manowarj[]= "manowarj";
char t_manzpower[]= "manzpower";
char t_marksjulia[]= "marksjulia";
char t_marksmandel[]= "marksmandel";
char t_marksmandelpwr[]= "marksmandelpwr";
char t_newtbasin[]= "newtbasin";
char t_newton[]= "newton";
char t_phoenix[]= "phoenix";
char t_phoenixcplx[]= "phoenixcplx";
char t_popcorn[]= "popcorn";
char t_popcornjul[]= "popcornjul";
char t_rossler3d[]= "rossler3d";
char t_sierpinski[]= "sierpinski";
char t_spider[]= "spider";
char t_sqr_1divfn_[]= "sqr(1/fn)";
char t_sqr_fn_[]= "sqr(fn)";
char t_tims_error[]= "tim's_error";
char t_unity[]= "unity";
char t_frothybasin[]= "frothybasin";
char t_halley[]= "halley";

/* use next to cast orbitcalcs() that have arguments */
#define VF int(*)(void)

struct fractalspecificstuff far fractalspecific[]=
{
   /*
   {
   fractal name,
      {parameter text strings},
      {parameter values},
      helptext, helpformula, flags,
      xmin, xmax, ymin, ymax,
      tojulia, tomandel, symmetry,
      orbit fnct, per_pixel fnct, per_image fnct, calctype fcnt,
      bailout
   }
   */
   {
   t_mandel,
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_MANDEL, HF_MANDEL, WINFRAC+BAILTEST+BF_MATH,
      (float)-2.5, (float)1.5, (float)-1.5, (float)1.5,
      JULIAFP, NOFRACTAL, XAXIS_NOPARM,
      JuliafpFractal, mandelfp_per_pixel, MandelfpSetup, StandardFractal,
      STDBAILOUT
   },

   {
   t_newtbasin,
      {newtdegree, stripes, ES, ES},
      {3, 0, 0, 0},
      HT_NEWTBAS, HF_NEWTBAS, WINFRAC,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      NewtonFractal2, otherjuliafp_per_pixel, NewtonSetup, StandardFractal,
      NOBAILOUT
   },

   {
   t_newton,
      {newtdegree, ES, ES, ES},
      {3, 0, 0, 0},
      HT_NEWT, HF_NEWT, WINFRAC,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, XAXIS,
      NewtonFractal2, otherjuliafp_per_pixel, NewtonSetup, StandardFractal,
      NOBAILOUT
   },

   {
   t_julia,
      {realparm, imagparm, ES, ES},
      {0.3, 0.6, 0, 0},
      HT_JULIA, HF_JULIA, WINFRAC+OKJB+BAILTEST+BF_MATH,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, MANDELFP, ORIGIN,
      JuliafpFractal, juliafp_per_pixel,  JuliafpSetup, StandardFractal,
      STDBAILOUT
   },

   {
   "plasma",
      {"Graininess Factor (0 or 0.125 to 100, default is 2)",
       "+Algorithm (0 = original, 1 = new)",
       "+Random Seed Value (0 = Random, 1 = Reuse Last)",
       "+Save as Pot File? (0 = No,     1 = Yes)"
      },
      {2, 0, 0, 0},
      HT_PLASMA, HF_PLASMA, NOZOOM+NOGUESS+NOTRACE+NORESUME+WINFRAC,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      NULL, NULL, StandaloneSetup, plasma,
      NOBAILOUT
   },

   {
   t_mandelfn,
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_MANDFN, HF_MANDFN, TRIG1+WINFRAC,
      (float)-8.0, (float)8.0, (float)-6.0, (float)6.0,
      LAMBDATRIGFP, NOFRACTAL, XYAXIS_NOPARM,
      LambdaTrigfpFractal,othermandelfp_per_pixel,MandelTrigSetup,
                                                         StandardFractal,
      LTRIGBAILOUT
   },

   {
   t_manowar,
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_SCOTSKIN, HF_MANOWAR, WINFRAC+BAILTEST,
      (float)-2.5, (float)1.5, (float)-1.5, (float)1.5,
      MANOWARJFP, NOFRACTAL, XAXIS_NOPARM,
      ManOWarfpFractal, mandelfp_per_pixel, MandelfpSetup, StandardFractal,
      STDBAILOUT
   },

   {
   "test",
      {"(testpt Param #1)",
       "(testpt param #2)",
       "(testpt param #3)",
       "(testpt param #4)"
      },
      {0, 0, 0, 0},
      HT_TEST, HF_TEST, 0,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      NULL, NULL, StandaloneSetup, test,
      STDBAILOUT
   },

   {
   t_sqr_fn_,
      {ES, ES, ES, ES},
      {0, 0, 0, 0},
      HT_SCOTSKIN, HF_SQRFN, TRIG1+WINFRAC+BAILTEST,
      (float)-4.0, (float)4.0, (float)-3.0, (float)3.0,
      NOFRACTAL, NOFRACTAL, XAXIS,
      SqrTrigfpFractal, otherjuliafp_per_pixel, SqrTrigSetup, StandardFractal,
      LTRIGBAILOUT
   },

   {
   "ifs",
      {color_method, ES, ES, ES},
      {0, 0, 0, 0},
      HT_IFS, -4, NOGUESS+NOTRACE+NORESUME+WINFRAC+INFCALC,
      (float)-8.0, (float)8.0, (float)-1.0, (float)11.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      NULL, NULL, StandaloneSetup, ifs,
      NOBAILOUT
   },

   {
   t_ifs3d,
      {color_method, ES, ES, ES},
      {0, 0, 0, 0},
      HT_IFS, -4, NOGUESS+NOTRACE+NORESUME+WINFRAC+PARMS3D+INFCALC,
      (float)-11.0, (float)11.0, (float)-11.0, (float)11.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      NULL, NULL, StandaloneSetup, ifs,
      NOBAILOUT
   },

   {
   t_fn_zz_,
      {ES, ES, ES, ES},
      {0, 0, 0, 0},
      HT_SCOTSKIN, HF_FNZTIMESZ, TRIG1+WINFRAC+BAILTEST,
      (float)-4.0, (float)4.0, (float)-3.0, (float)3.0,
      NOFRACTAL, NOFRACTAL, XYAXIS,
      TrigZsqrdfpFractal, juliafp_per_pixel, JuliafpSetup, StandardFractal,
      STDBAILOUT
   },

   {
   t_bifurcation,
      {filt, seed, ES, ES},
      {1000.0, 0.66, 0, 0},
      HT_BIF, HF_BIFURCATION, TRIG1+NOGUESS+NOTRACE+NOROTATE+WINFRAC,
      (float)1.9, (float)3.0, (float)0.0, (float)1.34,
      NOFRACTAL, NOFRACTAL, NOSYM,
      BifurcVerhulstTrig, NULL, StandaloneSetup, Bifurcation,
      NOBAILOUT
   },

   {
   t_fnplusfn,
      {recoeftrg1, imcoeftrg1, recoeftrg2, imcoeftrg2},
      {1, 0, 1, 0},
      HT_SCOTSKIN, HF_FNPLUSFN, TRIG2+WINFRAC+BAILTEST,
      (float)-4.0, (float)4.0, (float)-3.0, (float)3.0,
      NOFRACTAL, NOFRACTAL, XAXIS,
      TrigPlusTrigfpFractal, otherjuliafp_per_pixel, TrigPlusTrigfpSetup,
                                                            StandardFractal,
      LTRIGBAILOUT
   },

   {
   t_fnfn,
      {ES, ES, ES, ES},
      {0, 0, 0, 0},
      HT_SCOTSKIN, HF_FNTIMESFN, TRIG2+WINFRAC+BAILTEST,
      (float)-4.0, (float)4.0, (float)-3.0, (float)3.0,
      NOFRACTAL, NOFRACTAL, XAXIS,
      TrigXTrigfpFractal, otherjuliafp_per_pixel, FnXFnSetup, StandardFractal,
      LTRIGBAILOUT
   },

   {
   t_sqr_1divfn_,
      {ES, ES, ES, ES},
      {0, 0, 0, 0},
      HT_SCOTSKIN, HF_SQROVFN, TRIG1+WINFRAC+BAILTEST,
      (float)-4.0, (float)4.0, (float)-3.0, (float)3.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      Sqr1overTrigfpFractal, otherjuliafp_per_pixel, SqrTrigSetup,
                                                        StandardFractal,
      LTRIGBAILOUT
   },

   {
   t_fnzplusz,
      {recoeftrg1, imcoeftrg1, recoef2nd, imcoef2nd},
      {1, 0, 1, 0},
      HT_SCOTSKIN, HF_FNXZPLUSZ, TRIG1+WINFRAC+BAILTEST,
      (float)-4.0, (float)4.0, (float)-3.0, (float)3.0,
      NOFRACTAL, NOFRACTAL, XAXIS,
      ZXTrigPlusZfpFractal, juliafp_per_pixel, ZXTrigPlusZSetup,
                                                           StandardFractal,
      LTRIGBAILOUT
   },

   {
   t_kamtorus,
      {kamangle, kamstep, kamstop, pointsperorbit},
      {1.3, .05, 1.5, 150},
      HT_KAM, HF_KAM, NOGUESS+NOTRACE+WINFRAC,
      (float)-1.0, (float)1.0, (float)-.75, (float).75,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)kamtorusfloatorbit, NULL, orbit3dfloatsetup, orbit2dfloat,
      NOBAILOUT
   },

   {
   t_kamtorus3d,
      {kamangle, kamstep, kamstop, pointsperorbit},
      {1.3, .05, 1.5, 150},
      HT_KAM, HF_KAM, NOGUESS+NOTRACE+NORESUME+WINFRAC+PARMS3D,
      (float)-3.0, (float)3.0, (float)-1.0, (float)3.5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)kamtorusfloatorbit, NULL, orbit3dfloatsetup, orbit3dfloat,
      NOBAILOUT
   },

   {
   t_manfnpluszsqrd,
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_PICKMJ, HF_MANDFNPLUSZSQRD, TRIG1+WINFRAC+BAILTEST,
      (float)-2.5, (float)1.5, (float)-1.5, (float)1.5,
      FPJULTRIGPLUSZSQRD,   NOFRACTAL, XAXIS_NOPARM,
      TrigPlusZsquaredfpFractal, mandelfp_per_pixel, MandelfpSetup,
                                                            StandardFractal,
      STDBAILOUT
   },

   {
   t_julfnpluszsqrd,
      {realparm, imagparm, ES, ES},
      {-0.5, 0.5, 0, 0},
      HT_PICKMJ, HF_JULFNPLUSZSQRD, TRIG1+WINFRAC+OKJB+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, FPMANTRIGPLUSZSQRD, NOSYM,
      TrigPlusZsquaredfpFractal, juliafp_per_pixel, JuliafnPlusZsqrdSetup,
                                                            StandardFractal,
      STDBAILOUT
   },

   {
   t_lambdafn,
      {realparm, imagparm, ES, ES},
      {1.0, 0.4, 0, 0},
      HT_LAMBDAFN, HF_LAMBDAFN, TRIG1+WINFRAC+OKJB,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, MANDELTRIGFP, PI_SYM,
      LambdaTrigfpFractal, otherjuliafp_per_pixel, LambdaTrigSetup,
                                                          StandardFractal,
      LTRIGBAILOUT
   },

   {
   t_manzpower,
      {realz0, imagz0, exponent, imexponent},
      {0, 0, 2, 0},
      HT_PICKMJ, HF_MANZPOWER, WINFRAC+BAILTEST+BF_MATH,
      (float)-2.5, (float)1.5, (float)-1.5, (float)1.5,
      FPJULIAZPOWER, NOFRACTAL, XAXIS_NOIMAG,
      floatZpowerFractal, othermandelfp_per_pixel, MandelfpSetup,
                                                       StandardFractal,
      STDBAILOUT
   },

   {
   t_julzpower,
      {realparm, imagparm, exponent, imexponent},
      {0.3, 0.6, 2, 0},
      HT_PICKMJ, HF_JULZPOWER, WINFRAC+OKJB+BAILTEST+BF_MATH,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, FPMANDELZPOWER, ORIGIN,
      floatZpowerFractal, otherjuliafp_per_pixel, JuliafpSetup,
                                                       StandardFractal,
      STDBAILOUT
   },

   {
   "manzzpwr",
      {realz0, imagz0, exponent, ES},
      {0, 0, 2, 0},
      HT_PICKMJ, HF_MANZZPWR, WINFRAC+BAILTEST,
      (float)-2.5, (float)1.5, (float)-1.5, (float)1.5,
      FPJULZTOZPLUSZPWR, NOFRACTAL, XAXIS_NOPARM,
      floatZtozPluszpwrFractal, othermandelfp_per_pixel, MandelfpSetup,
                                                          StandardFractal,
      STDBAILOUT
   },

   {
   "julzzpwr",
      {realparm, imagparm, exponent, ES},
      {-0.3, 0.3, 2, 0},
      HT_PICKMJ, HF_JULZZPWR, WINFRAC+OKJB+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, FPMANZTOZPLUSZPWR, NOSYM,
      floatZtozPluszpwrFractal, otherjuliafp_per_pixel, JuliafpSetup,
                                                          StandardFractal,
      STDBAILOUT
   },

   {
   t_manfnplusexp,
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_PICKMJ, HF_MANDFNPLUSEXP, TRIG1+WINFRAC+BAILTEST,
      (float)-8.0, (float)8.0, (float)-6.0, (float)6.0,
      FPJULTRIGPLUSEXP, NOFRACTAL, XAXIS_NOPARM,
      FloatTrigPlusExponentFractal, othermandelfp_per_pixel, MandelfpSetup,
                                                             StandardFractal,
      STDBAILOUT
   },

   {
   t_julfnplusexp,
      {realparm, imagparm, ES, ES},
      {0, 0, 0, 0},
      HT_PICKMJ, HF_JULFNPLUSEXP, TRIG1+WINFRAC+OKJB+BAILTEST,
      (float)-4.0, (float)4.0, (float)-3.0, (float)3.0,
      NOFRACTAL, FPMANTRIGPLUSEXP, NOSYM,
      FloatTrigPlusExponentFractal, otherjuliafp_per_pixel, JuliafpSetup,
                                                           StandardFractal,
      STDBAILOUT
   },

   {
   t_popcorn,
      {step_x, step_y, constant_x, constant_y},
      {0.05, 0, 3.00, 0},
      HT_POPCORN, HF_POPCORN, NOGUESS+NOTRACE+WINFRAC+TRIG4,
      (float)-3.0, (float)3.0, (float)-2.25, (float)2.25,
      NOFRACTAL, NOFRACTAL, NOPLOT,
      PopcornFractalFn, otherjuliafp_per_pixel, JuliafpSetup, popcorn,
      STDBAILOUT
   },

   {
   t_lorenz,
      {timestep, A, B, C},
      {.02, 5, 15, 1},
      HT_LORENZ, HF_LORENZ, NOGUESS+NOTRACE+INFCALC+WINFRAC,
      (float)-15.0, (float)15.0, (float)0.0, (float)30.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)lorenz3dfloatorbit, NULL, orbit3dfloatsetup, orbit2dfloat,
      NOBAILOUT
   },

   {
   "complexnewton",
      {realdegree, imagdegree, realroot, imagroot},
      {3, 0, 1, 0},
      HT_NEWTCMPLX, HF_COMPLEXNEWT, WINFRAC,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      ComplexNewton, otherjuliafp_per_pixel, ComplexNewtonSetup,
                                                     StandardFractal,
      NOBAILOUT
   },

   {
   "complexbasin",
      {realdegree, imagdegree, realroot, imagroot},
      {3, 0, 1, 0},
      HT_NEWTCMPLX, HF_COMPLEXNEWT, WINFRAC,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      ComplexBasin, otherjuliafp_per_pixel, ComplexNewtonSetup,
                                                     StandardFractal,
      NOBAILOUT
   },

   {
   "cmplxmarksmand",
      {realz0, imagz0, exponent, imexponent},
      {0, 0, 1, 0},
      HT_MARKS, HF_CMPLXMARKSMAND, WINFRAC+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      COMPLEXMARKSJUL, NOFRACTAL, NOSYM,
      MarksCplxMand, MarksCplxMandperp, MandelfpSetup, StandardFractal,
      STDBAILOUT
   },

   {
   "cmplxmarksjul",
      {realparm, imagparm, exponent, imexponent},
      {0.3, 0.6, 1, 0},
      HT_MARKS, HF_CMPLXMARKSJUL, WINFRAC+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, COMPLEXMARKSMAND, NOSYM,
      MarksCplxMand, juliafp_per_pixel, JuliafpSetup, StandardFractal,
      STDBAILOUT
   },

   {
   t_formula,
      {p1real, p1imag, p2real, p2imag},
      {0, 0, 0, 0},
      HT_FORMULA, -2, WINFRAC+MORE,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, SETUP_SYM,
      Formula, form_per_pixel, fpFormulaSetup, StandardFractal,
      0
   },

   {
   t_sierpinski,
      {ES, ES, ES, ES},
      {0, 0, 0, 0},
      HT_SIER, HF_SIER, WINFRAC,
      (float)-4/3, (float)96/45, (float)-0.9, (float)1.7,
      NOFRACTAL, NOFRACTAL, NOSYM,
      SierpinskiFPFractal, otherjuliafp_per_pixel, SierpinskiFPSetup,
                                                       StandardFractal,
      127
   },

   {
   t_lambda,
      {realparm, imagparm, ES, ES},
      {0.85, 0.6, 0, 0},
      HT_LAMBDA, HF_LAMBDA, WINFRAC+OKJB+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, MANDELLAMBDAFP, NOSYM,
      LambdaFPFractal, juliafp_per_pixel, JuliafpSetup, StandardFractal,
      STDBAILOUT
   },

   {
   t_barnsleym1,
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_BARNS, HF_BARNSM1, WINFRAC+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      BARNSLEYJ1FP,NOFRACTAL, XYAXIS_NOPARM,
      Barnsley1FPFractal, othermandelfp_per_pixel, MandelfpSetup,
                                                       StandardFractal,
      STDBAILOUT
   },

   {
   t_barnsleyj1,
      {realparm, imagparm, ES, ES},
      {0.6, 1.1, 0, 0},
      HT_BARNS, HF_BARNSJ1, WINFRAC+OKJB+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, BARNSLEYM1FP, ORIGIN,
      Barnsley1FPFractal, otherjuliafp_per_pixel, JuliafpSetup,
                                                     StandardFractal,
      STDBAILOUT
   },

   {
   t_barnsleym2,
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_BARNS, HF_BARNSM2, WINFRAC+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      BARNSLEYJ2FP,NOFRACTAL, YAXIS_NOPARM,
      Barnsley2FPFractal, othermandelfp_per_pixel, MandelfpSetup,
                                                        StandardFractal,
      STDBAILOUT
   },

   {
   t_barnsleyj2,
      {realparm, imagparm, ES, ES},
      {0.6, 1.1, 0, 0},
      HT_BARNS, HF_BARNSJ2, WINFRAC+OKJB+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, BARNSLEYM2FP, ORIGIN,
      Barnsley2FPFractal, otherjuliafp_per_pixel, JuliafpSetup,
                                                       StandardFractal,
      STDBAILOUT
   },

   {
   t_barnsleym3,
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_BARNS, HF_BARNSM3, WINFRAC+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      BARNSLEYJ3FP, NOFRACTAL, XAXIS_NOPARM,
      Barnsley3FPFractal, othermandelfp_per_pixel, MandelfpSetup,
                                                      StandardFractal,
      STDBAILOUT
   },

   {
   t_barnsleyj3,
      {realparm, imagparm, ES, ES},
      {0.6, 1.1, 0, 0},
      HT_BARNS, HF_BARNSJ3, WINFRAC+OKJB+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, BARNSLEYM3FP, NOSYM,
      Barnsley3FPFractal, otherjuliafp_per_pixel, JuliafpSetup,
                                                    StandardFractal,
      STDBAILOUT
   },

   {
   t_mandellambda,
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_MLAMBDA, HF_MLAMBDA, WINFRAC+BAILTEST,
      (float)-3.0, (float)5.0, (float)-3.0, (float)3.0,
      LAMBDAFP, NOFRACTAL, XAXIS_NOPARM,
      LambdaFPFractal, mandelfp_per_pixel, MandelfpSetup, StandardFractal,
      STDBAILOUT
   },

   {
   t_lorenz3d,
      {timestep, A, B, C},
      {.02, 5, 15, 1},
      HT_LORENZ, HF_LORENZ, NOGUESS+NOTRACE+NORESUME+WINFRAC+PARMS3D+INFCALC,
      (float)-30.0, (float)30.0, (float)-30.0, (float)30.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)lorenz3dfloatorbit, NULL, orbit3dfloatsetup, orbit3dfloat,
      NOBAILOUT
   },

   {
   t_rossler3d,
      {timestep, A, B, C},
      {.04, .2, .2, 5.7},
      HT_ROSS, HF_ROSS, NOGUESS+NOTRACE+NORESUME+WINFRAC+PARMS3D+INFCALC,
      (float)-30.0, (float)30.0, (float)-20.0, (float)40.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)rosslerfloatorbit, NULL, orbit3dfloatsetup, orbit3dfloat,
      NOBAILOUT
   },

   {
   t_henon,
      {A, B, ES, ES},
      {1.4, .3, 0, 0},
      HT_HENON, HF_HENON, NOGUESS+NOTRACE+INFCALC+WINFRAC,
      (float)-1.4, (float)1.4, (float)-.5, (float).5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)henonfloatorbit, NULL, orbit3dfloatsetup, orbit2dfloat,
      NOBAILOUT
   },

   {
   "pickover",
      {A, B, C, D},
      {2.24, .43, -.65, -2.43},
      HT_PICK, HF_PICKOVER, NOGUESS+NOTRACE+NORESUME+WINFRAC+PARMS3D,
      (float)-8/3, (float)8/3, (float)-2, (float)2,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)pickoverfloatorbit, NULL, orbit3dfloatsetup, orbit3dfloat,
      NOBAILOUT
   },

   {
   "gingerbreadman",
      {initx, inity, ES, ES},
      {-.1, 0, 0, 0},
      HT_GINGER, HF_GINGER, NOGUESS+NOTRACE+INFCALC+WINFRAC,
      (float)-4.5, (float)8.5, (float)-4.5, (float)8.5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)gingerbreadfloatorbit, NULL, orbit3dfloatsetup, orbit2dfloat,
      NOBAILOUT
   },

   {
   "diffusion",
      {"+Border size",
       "+Type (0=Central,1=Falling,2=Square Cavity)",
       "+Color change rate (0=Random)",
        ES
      },
      {10, 0, 0, 0},
      HT_DIFFUS, HF_DIFFUS, NOZOOM+NOGUESS+NOTRACE+WINFRAC,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      NULL, NULL, StandaloneSetup, diffusion,
      NOBAILOUT
   },

   {
   t_unity,
      {ES, ES, ES, ES},
      {0, 0, 0, 0},
      HT_UNITY, HF_UNITY, WINFRAC,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, XYAXIS,
      UnityfpFractal, otherjuliafp_per_pixel, StandardSetup, StandardFractal,
      NOBAILOUT
   },

   {
   t_spider,
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_SCOTSKIN, HF_SPIDER, WINFRAC+BAILTEST,
      (float)-2.5, (float)1.5, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, XAXIS_NOPARM,
      SpiderfpFractal, mandelfp_per_pixel, MandelfpSetup, StandardFractal,
      STDBAILOUT
   },

   {
   "tetrate",
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_SCOTSKIN, HF_TETRATE, WINFRAC+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, XAXIS_NOIMAG,
      TetratefpFractal, othermandelfp_per_pixel, MandelfpSetup,
                                                     StandardFractal,
      STDBAILOUT
   },

   {
   "magnet1m",
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_MAGNET, HF_MAGM1, WINFRAC,
      (float)-4.0, (float)4.0, (float)-3.0, (float)3.0,
      MAGNET1J,NOFRACTAL,XAXIS_NOPARM,
      Magnet1Fractal, mandelfp_per_pixel, MandelfpSetup, StandardFractal,
      100
   },

   {
   "magnet1j",
      {realparm, imagparm, ES, ES},
      {0, 0, 0, 0},
      HT_MAGNET, HF_MAGJ1, WINFRAC,
      (float)-8.0, (float)8.0, (float)-6.0, (float)6.0,
      NOFRACTAL,MAGNET1M,XAXIS_NOIMAG,
      Magnet1Fractal, juliafp_per_pixel, JuliafpSetup, StandardFractal,
      100
   },

   {
   "magnet2m",
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_MAGNET, HF_MAGM2, WINFRAC,
      (float)-1.5, (float)3.7, (float)-1.95, (float)1.95,
      MAGNET2J, NOFRACTAL, XAXIS_NOPARM,
      Magnet2Fractal, mandelfp_per_pixel, MandelfpSetup, StandardFractal,
      100
   },

   {
   "magnet2j",
      {realparm, imagparm, ES, ES},
      {0, 0, 0, 0},
      HT_MAGNET, HF_MAGJ2, WINFRAC,
      (float)-8.0, (float)8.0, (float)-6.0, (float)6.0,
      NOFRACTAL, MAGNET2M, XAXIS_NOIMAG,
      Magnet2Fractal, juliafp_per_pixel, JuliafpSetup, StandardFractal,
      100
   },

   {
   t_biflambda,
      {filt, seed, ES, ES},
      {1000.0, 0.66, 0, 0},
      HT_BIF, HF_BIFLAMBDA, TRIG1+NOGUESS+NOTRACE+NOROTATE+WINFRAC,
      (float)-2.0, (float)4.0, (float)-1.0, (float)2.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      BifurcLambdaTrig, NULL, StandaloneSetup, Bifurcation,
      NOBAILOUT
   },

   {
   t_bifplussinpi,
      {filt, seed, ES, ES},
      {1000.0, 0.66, 0, 0},
      HT_BIF, HF_BIFPLUSSINPI, TRIG1+NOGUESS+NOTRACE+NOROTATE+WINFRAC,
      (float)0.275, (float)1.45, (float)0.0, (float)2.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      BifurcAddTrigPi, NULL, StandaloneSetup, Bifurcation,
      NOBAILOUT
   },

   {
   t_bifeqsinpi,
      {filt, seed, ES, ES},
      {1000.0, 0.66, 0, 0},
      HT_BIF, HF_BIFEQSINPI, TRIG1+NOGUESS+NOTRACE+NOROTATE+WINFRAC,
      (float)-2.5, (float)2.5, (float)-3.5, (float)3.5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      BifurcSetTrigPi, NULL, StandaloneSetup, Bifurcation,
      NOBAILOUT
   },

   {
   t_popcornjul,
      {step_x, step_y, constant_x, constant_y},
      {0.05, 0, 3.00, 0},
      HT_POPCORN, HF_POPCJUL, WINFRAC+TRIG4,
      (float)-3.0, (float)3.0, (float)-2.25, (float)2.25,
      NOFRACTAL, NOFRACTAL, NOSYM,
      PopcornFractalFn, otherjuliafp_per_pixel, JuliafpSetup,StandardFractal,
      STDBAILOUT
   },

   {
   "lsystem",
      {"+Order", ES, ES, ES},
      {2, 0, 0, 0},
      HT_LSYS, -3, NOZOOM+NORESUME+NOGUESS+NOTRACE+WINFRAC,
      (float)-1.0, (float)1.0, (float)-1.0, (float)1.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      NULL, NULL, StandaloneSetup, Lsystem,
      NOBAILOUT
   },

   {
   t_manowarj,
      {realparm, imagparm, ES, ES},
      {0, 0, 0, 0},
      HT_SCOTSKIN, HF_MANOWARJ, WINFRAC+OKJB+BAILTEST,
      (float)-2.5, (float)1.5, (float)-1.5, (float)1.5,
      NOFRACTAL, MANOWARFP, NOSYM,
      ManOWarfpFractal, juliafp_per_pixel, JuliafpSetup, StandardFractal,
      STDBAILOUT
   },

   {
   t_fn_z_plusfn_pix_,
      {realz0, imagz0, recoeftrg2, imcoeftrg2},
      {0, 0, 1, 0},
      HT_SCOTSKIN, HF_FNPLUSFNPIX, TRIG2+WINFRAC+BAILTEST,
      (float)-3.6, (float)3.6, (float)-2.7, (float)2.7,
      NOFRACTAL, NOFRACTAL, NOSYM,
      Richard8fpFractal, otherrichard8fp_per_pixel, MandelfpSetup,
                                                       StandardFractal,
      LTRIGBAILOUT
   },

   {
   t_marksmandelpwr,
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_MARKS, HF_MARKSMANDPWR, TRIG1+WINFRAC+BAILTEST,
      (float)-2.5, (float)1.5, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, XAXIS_NOPARM,
      MarksMandelPwrfpFractal, marks_mandelpwrfp_per_pixel, MandelfpSetup,
                                                             StandardFractal,
      STDBAILOUT
   },

   {
   t_tims_error,
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_MARKS, HF_TIMSERR, WINFRAC+TRIG1+BAILTEST,
      (float)-2.9, (float)4.3, (float)-2.7, (float)2.7,
      NOFRACTAL, NOFRACTAL, XAXIS_NOPARM,
      TimsErrorfpFractal, marks_mandelpwrfp_per_pixel, MandelfpSetup,
                                                         StandardFractal,
      STDBAILOUT
   },

   {
   t_bifstewart,
      {filt, seed, ES, ES},
      {1000.0, 0.66, 0, 0},
      HT_BIF, HF_BIFSTEWART, TRIG1+NOGUESS+NOTRACE+NOROTATE+WINFRAC,
      (float)0.7, (float)2.0, (float)-1.1, (float)1.1,
      NOFRACTAL, NOFRACTAL, NOSYM,
      BifurcStewartTrig, NULL, StandaloneSetup, Bifurcation,
      NOBAILOUT
   },

   {
   "hopalong",
      {A, B, C, ES},
      {.4, 1, 0, 0},
      HT_MARTIN, HF_HOPALONG, NOGUESS+NOTRACE+INFCALC+WINFRAC,
      (float)-2.0, (float)3.0, (float)-1.625, (float)2.625,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)hopalong2dfloatorbit, NULL, orbit3dfloatsetup, orbit2dfloat,
      NOBAILOUT
   },

   {
   "circle",
      {"magnification", ES, ES, ES},
      {200000L, 0, 0, 0},
      HT_CIRCLE, HF_CIRCLE, WINFRAC,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, XYAXIS,
      CirclefpFractal, juliafp_per_pixel, JuliafpSetup, StandardFractal,
      NOBAILOUT
   },

   {
   "martin",
      {A, ES, ES, ES},
      {3.14, 0, 0, 0},
      HT_MARTIN, HF_MARTIN, NOGUESS+NOTRACE+INFCALC+WINFRAC,
      (float)-32, (float)32, (float)-24, (float)24,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)martin2dfloatorbit, NULL, orbit3dfloatsetup, orbit2dfloat,
      NOBAILOUT
   },

   {
   "lyapunov",
      {"+Order (integer)", "Population Seed", "+Filter Cycles", ES},
      {0, 0.5, 0, 0},
      HT_LYAPUNOV, HT_LYAPUNOV, WINFRAC,
      (float)-8.0, (float)8.0, (float)-6.0, (float)6.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      BifurcLambda, NULL, lya_setup, lyapunov,
      NOBAILOUT
   },

   {
   "lorenz3d1",
      {timestep, A, B, C},
      {.02, 5, 15, 1},
      HT_LORENZ, HF_LORENZ3D1,
                        NOGUESS+NOTRACE+NORESUME+WINFRAC+PARMS3D+INFCALC,
      (float)-30.0, (float)30.0, (float)-30.0, (float)30.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)lorenz3d1floatorbit, NULL, orbit3dfloatsetup, orbit3dfloat,
      NOBAILOUT
   },

   {
   "lorenz3d3",
      {timestep, A, B, C},
      {.02, 10, 28, 2.66},
      HT_LORENZ, HF_LORENZ3D3,
                      NOGUESS+NOTRACE+NORESUME+WINFRAC+PARMS3D+INFCALC,
      (float)-30.0, (float)30.0, (float)-30.0, (float)30.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)lorenz3d3floatorbit, NULL, orbit3dfloatsetup, orbit3dfloat,
      NOBAILOUT
   },

   {
   "lorenz3d4",
      {timestep, A, B, C},
      {.02, 10, 28, 2.66},
      HT_LORENZ, HF_LORENZ3D4,
                       NOGUESS+NOTRACE+NORESUME+WINFRAC+PARMS3D+INFCALC,
      (float)-30.0, (float)30.0, (float)-30.0, (float)30.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)lorenz3d4floatorbit, NULL, orbit3dfloatsetup, orbit3dfloat,
      NOBAILOUT
   },

   {
   t_lambda_fnorfn_,
      {realparm, imagparm, shiftval, ES},
      {1, 0.1, 1, 0},
      HT_FNORFN, HF_LAMBDAFNFN, TRIG2+WINFRAC+BAILTEST,
      (float)-4.0, (float)4.0, (float)-3.0, (float)3.0,
      NOFRACTAL, FPMANLAMFNFN, ORIGIN,
      LambdaTrigOrTrigfpFractal, otherjuliafp_per_pixel,
                                      LambdaTrigOrTrigSetup, StandardFractal,
      LTRIGBAILOUT
   },

   {
   t_julia_fnorfn_,
      {realparm, imagparm, shiftval, ES},
      {0, 0, 8, 0},
      HT_FNORFN, HF_JULIAFNFN, TRIG2+WINFRAC+BAILTEST,
      (float)-4.0, (float)4.0, (float)-3.0, (float)3.0,
      NOFRACTAL, FPMANFNFN, XAXIS,
      JuliaTrigOrTrigfpFractal, otherjuliafp_per_pixel,
                                    JuliaTrigOrTrigSetup, StandardFractal,
      LTRIGBAILOUT
   },

   {
   t_manlam_fnorfn_,
      {realz0, imagz0, shiftval, ES},
      {0, 0, 10, 0},
      HT_FNORFN, HF_MANLAMFNFN, TRIG2+WINFRAC+BAILTEST,
      (float)-4.0, (float)4.0, (float)-3.0, (float)3.0,
      FPLAMBDAFNFN, NOFRACTAL, XAXIS_NOPARM,
      LambdaTrigOrTrigfpFractal, othermandelfp_per_pixel,
                                     ManlamTrigOrTrigSetup, StandardFractal,
      LTRIGBAILOUT
   },

   {
   t_mandel_fnorfn_,
      {realz0, imagz0, shiftval, ES},
      {0, 0, 0.5, 0},
      HT_FNORFN, HF_MANDELFNFN, TRIG2+WINFRAC+BAILTEST,
      (float)-4.0, (float)4.0, (float)-3.0, (float)3.0,
      FPJULFNFN, NOFRACTAL, XAXIS_NOPARM,
      JuliaTrigOrTrigfpFractal, othermandelfp_per_pixel,
                                    MandelTrigOrTrigSetup, StandardFractal,
      LTRIGBAILOUT
   },

   {
   t_bifmay,
      {filt, seed, "Beta >= 2", ES},
      {300.0, 0.9, 5, 0},
      HT_BIF, HF_BIFMAY, NOGUESS+NOTRACE+NOROTATE+WINFRAC,
      (float)-3.5, (float)-0.9, (float)-0.5, (float)3.2,
      NOFRACTAL, NOFRACTAL, NOSYM,
      BifurcMay, NULL, BifurcMaySetup, Bifurcation,
      NOBAILOUT
   },

   {
   t_halley,
      {order, real_relax, epsilon, imag_relax},
      {6, 1.0, 0.0001, 0},
      HT_HALLEY, HF_HALLEY, WINFRAC,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, XYAXIS,
      HalleyFractal, Halley_per_pixel, HalleySetup, StandardFractal,
      NOBAILOUT
   },

   {
   "dynamic",
      {"+# of intervals (<0 = connect)", "time step (<0 = Euler)", A, B},
      {50, .1, 1, 3},
      HT_DYNAM, HF_DYNAM, NOGUESS+NOTRACE+WINFRAC+TRIG1,
      (float)-20.0, (float)20.0, (float)-20.0, (float)20.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)dynamfloat, NULL, dynam2dfloatsetup, dynam2dfloat,
      NOBAILOUT
   },

   {
   "quat",
      {notused, notused, CJ, CK},
      {0, 0, 0, 0},
      HT_QUAT, HF_QUAT, WINFRAC+OKJB,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      QUATJULFP, NOFRACTAL, XAXIS,
      QuaternionFPFractal, quaternionfp_per_pixel, MandelfpSetup,
                                                       StandardFractal,
      LTRIGBAILOUT
   },

   {
   "quatjul",
      {C1, CI, CJ, CK},
      {-.745, 0, .113, .05},
      HT_QUAT, HF_QUATJ, WINFRAC+OKJB+MORE,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, QUATFP, ORIGIN,
      QuaternionFPFractal, quaternionjulfp_per_pixel, JuliafpSetup,
                                                        StandardFractal,
      LTRIGBAILOUT
   },

   {
   "cellular",
      {cell_init, cell_rule, cell_type, cell_strt},
      {11.0, 3311100320.0, 41.0, 0},
      HT_CELLULAR, HF_CELLULAR, NOGUESS+NOTRACE+NOZOOM+WINFRAC,
      (float)-1.0, (float)1.0, (float)-1.0, (float)1.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      NULL, NULL, CellularSetup, cellular,
      NOBAILOUT
   },

   {
   t_julibrot,
      {ES, ES, ES, ES},
      {0, 0, 0, 0},
      HT_JULIBROT, -1, NOGUESS+NOTRACE+NOROTATE+NORESUME+WINFRAC,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      JuliafpFractal, jbfp_per_pixel, JulibrotSetup, Std4dfpFractal,
      STDBAILOUT
   },

#ifdef RANDOM_RUN
   {
   t_julia_inverse,
      {realparm, imagparm, s_maxhits, randomruninterval},
      {-0.11, 0.6557, 4, 1024},
      HT_INVERSE, HF_INVERSE, NOGUESS+NOTRACE+INFCALC+WINFRAC+NORESUME,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, MANDELFP, NOSYM,
      Minverse_julia_orbit, NULL, orbit3dfloatsetup, inverse_julia_per_image,
      NOBAILOUT
   },
#else
   {
   t_julia_inverse,
      {realparm, imagparm, s_maxhits, ES},
      {-0.11, 0.6557, 4, 1024},
      HT_INVERSE, HF_INVERSE, NOGUESS+NOTRACE+INFCALC+WINFRAC+NORESUME,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, MANDELFP, NOSYM,
      Minverse_julia_orbit, NULL, orbit3dfloatsetup, inverse_julia_per_image,
      NOBAILOUT
   },

#endif
   {
   "mandelcloud",
      {"+# of intervals (<0 = connect)", ES, ES, ES},
      {50, 0, 0, 0},
      HT_MANDELCLOUD, HF_MANDELCLOUD, NOGUESS+NOTRACE+WINFRAC,
      (float)-2.5, (float)1.5, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)mandelcloudfloat, NULL, dynam2dfloatsetup, dynam2dfloat,
      NOBAILOUT
   },

   {
   t_phoenix,
      {p1real, p2real, degreeZ, ES},
      {0.56667, -0.5, 0, 0},
      HT_PHOENIX, HF_PHOENIX, WINFRAC+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, MANDPHOENIXFP, XAXIS,
      PhoenixFractal, phoenix_per_pixel, PhoenixSetup, StandardFractal,
      STDBAILOUT
   },

   {
   t_mandphoenix,
      {realz0, imagz0, degreeZ, ES},
      {0.0, 0.0, 0, 0},
      HT_PHOENIX, HF_MANDPHOENIX, WINFRAC+BAILTEST,
      (float)-2.5, (float)1.5, (float)-1.5, (float)1.5,
      PHOENIXFP, NOFRACTAL, NOSYM,
      PhoenixFractal, mandphoenix_per_pixel, MandPhoenixSetup,
                                                     StandardFractal,
      STDBAILOUT
   },

   {
   "hypercomplex",
      {notused, notused, CJ, CK},
      {0, 0, 0, 0},
      HT_HYPERC, HF_HYPERC, WINFRAC+OKJB+TRIG1,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      HYPERCMPLXJFP, NOFRACTAL, XAXIS,
      HyperComplexFPFractal, quaternionfp_per_pixel, MandelfpSetup,
                                                        StandardFractal,
      LTRIGBAILOUT
   },

   {
   "hypercomplexj",
      {C1, CI, CJ, CK},
      {-.745, 0, .113, .05},
      HT_HYPERC, HF_HYPERCJ, WINFRAC+OKJB+TRIG1+MORE,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, HYPERCMPLXFP, ORIGIN,
      HyperComplexFPFractal, quaternionjulfp_per_pixel, JuliafpSetup,
                                                          StandardFractal,
      LTRIGBAILOUT
   },

   {
   t_frothybasin,
      {frothmapping, frothshade, frothavalue, ES},
      {1, 0, 1.028713768218725, 0},
      HT_FROTH, HF_FROTH, NOTRACE+WINFRAC,
      (float)-2.8, (float)2.8, (float)-2.355, (float)1.845,
      NOFRACTAL, NOFRACTAL, NOSYM,
      froth_per_orbit, froth_per_pixel, froth_setup, calcfroth,
      FROTHBAILOUT
   },

   {
   t_mandel4,
      {realz0, imagz0, ES, ES},
      {0, 0, 0, 0},
      HT_MANDJUL4, HF_MANDEL4, WINFRAC+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      JULIA4FP, NOFRACTAL, XAXIS_NOPARM,
      Mandel4fpFractal, mandelfp_per_pixel, MandelfpSetup, StandardFractal,
      STDBAILOUT
   },

   {
   t_julia4,
      {realparm, imagparm, ES, ES},
      {0.6, 0.55, 0, 0},
      HT_MANDJUL4, HF_JULIA4, WINFRAC+OKJB+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, MANDEL4FP, ORIGIN,
      Mandel4fpFractal, juliafp_per_pixel, JuliafpSetup,StandardFractal,
      STDBAILOUT
   },

   {
   t_marksmandel,
      {realz0, imagz0, exponent, ES},
      {0, 0, 1, 0},
      HT_MARKS, HF_MARKSMAND, WINFRAC+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      MARKSJULIAFP, NOFRACTAL, NOSYM,
      MarksLambdafpFractal, marksmandelfp_per_pixel, MandelfpSetup,
                                                        StandardFractal,
      STDBAILOUT
   },

   {
   t_marksjulia,
      {realparm, imagparm, exponent, ES},
      {0.1, 0.9, 1, 0},
      HT_MARKS, HF_MARKSJULIA, WINFRAC+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, MARKSMANDELFP, ORIGIN,
      MarksLambdafpFractal, juliafp_per_pixel, MarksJuliafpSetup,
                                                      StandardFractal,
      STDBAILOUT
   },

   {
   "icons",
      {s_lambda, s_alpha, s_beta, s_gamma},
      {-2.34, 2.0, 0.2, 0.1},
      HT_ICON, HF_ICON, NOGUESS+NOTRACE+WINFRAC+INFCALC+MORE,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)iconfloatorbit, NULL, orbit3dfloatsetup, orbit2dfloat,
      NOBAILOUT
   },

   {
   "icons3d",
      {s_lambda, s_alpha, s_beta, s_gamma},
      {-2.34, 2.0, 0.2, 0.1},
      HT_ICON, HF_ICON, NOGUESS+NOTRACE+WINFRAC+INFCALC+PARMS3D+MORE,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)iconfloatorbit, NULL, orbit3dfloatsetup, orbit3dfloat,
      NOBAILOUT
   },

   {
   t_phoenixcplx,
      {p1real, p1imag, p2real, p2imag},
      {0.2, 0, 0.3, 0},
      HT_PHOENIX, HF_PHOENIXCPLX, WINFRAC+MORE+BAILTEST,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, MANDPHOENIXFPCPLX, ORIGIN,
      PhoenixFractalcplx, phoenix_per_pixel, PhoenixCplxSetup,
                                                   StandardFractal,
      STDBAILOUT
   },

   {
   t_mandphoenixcplx,
      {realz0, imagz0, p2real, p2imag},
      {0, 0, 0.5, 0},
      HT_PHOENIX, HF_MANDPHOENIXCPLX, WINFRAC+MORE+BAILTEST,
      (float)-2.5, (float)1.5, (float)-1.5, (float)1.5,
      PHOENIXFPCPLX, NOFRACTAL, XAXIS,
      PhoenixFractalcplx, mandphoenix_per_pixel, MandPhoenixCplxSetup,
                                                          StandardFractal,
      STDBAILOUT
   },

   {
   "ant",
      {"#Rule String (1's and non-1's, 0 rand)",
       "#Maxpts",
       "+Numants (max 256)",
       "+Ant type (1 or 2)"
      },
      {1100, 1.0E9, 1, 1},
      HT_ANT, HF_ANT, NOZOOM+NOGUESS+NOTRACE+NORESUME+MORE,
      (float)-1.0, (float)1.0, (float)-1.0, (float)1.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      NULL, NULL, StandaloneSetup, ant,
      NOBAILOUT
   },

   {
   "chip",
      {A, B, C, ES},
      {-15,-19,1,0},
      HT_MARTIN, HF_CHIP, NOGUESS+NOTRACE+INFCALC+WINFRAC,
      (float)-760.0, (float)760.0, (float)-570.0, (float)570.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)chip2dfloatorbit, NULL, orbit3dfloatsetup, orbit2dfloat,
      NOBAILOUT
   },

   {
   "quadruptwo",
      {A, B, C, ES},
      {34, 1, 5, 0},
      HT_MARTIN, HF_QUADRUPTWO, NOGUESS+NOTRACE+INFCALC+WINFRAC,
      (float)-82.93367, (float)112.2749, (float)-55.76383, (float)90.64257,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)quadruptwo2dfloatorbit, NULL, orbit3dfloatsetup, orbit2dfloat,
      NOBAILOUT
   },

   {
   "threeply",
      {A, B, C, ES},
      {-55, -1, -42, 0},
      HT_MARTIN, HF_THREEPLY, NOGUESS+NOTRACE+INFCALC+WINFRAC,
      (float)-8000.0, (float)8000.0, (float)-6000.0, (float)6000.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)threeply2dfloatorbit, NULL, orbit3dfloatsetup, orbit2dfloat,
      NOBAILOUT
   },

   {
   "volterra-lotka",
      {H, P, ES, ES},
      {0.739, 0.739, 0, 0},
      HT_VL, HF_VL, WINFRAC,
      (float)0.0, (float)6.0, (float)0.0, (float)4.5, 
      NOFRACTAL, NOFRACTAL, NOSYM,
      VLfpFractal, otherjuliafp_per_pixel, VLSetup, StandardFractal, 
      256
   },

   {
   "escher_julia",
      {realparm, imagparm, ES, ES},
      {0.32, 0.043, 0, 0},
      HT_ESCHER, HF_ESCHER, WINFRAC,
      (float)-1.6, (float)1.6, (float)-1.2, (float)1.2,
      NOFRACTAL, NOFRACTAL, ORIGIN,
      EscherfpFractal, juliafp_per_pixel, StandardSetup, 
          StandardFractal,
      STDBAILOUT
   },                     
  
/* From Pickovers' "Chaos in Wonderland"      */
/* included by Humberto R. Baptista           */
/* code adapted from king.cpp bt James Rankin */

   {
   "latoocarfian",
      {A, B, C, D},
      {-0.966918, 2.879879, 0.765145, 0.744728},
      HT_LATOO, HF_LATOO, NOGUESS+NOTRACE+WINFRAC+INFCALC+MORE+TRIG4,
      (float)-2.0, (float)2.0, (float)-1.5, (float)1.5,
      NOFRACTAL, NOFRACTAL, NOSYM,
      (VF)latoofloatorbit, NULL, orbit3dfloatsetup, orbit2dfloat,
      NOBAILOUT
   },

   {
   NULL,            /* marks the END of the list */
      {NULL, NULL, NULL, NULL},
      {0, 0, 0, 0},
      -1, -1, 0,
      (float)0.0, (float)0.0, (float)0.0, (float)0.0,
      NOFRACTAL, NOFRACTAL, NOSYM,
      NULL, NULL, NULL, NULL,
      0
   }
};

int num_fractal_types = (sizeof(fractalspecific)/
        sizeof(struct fractalspecificstuff)) -1;

/*
 *  Returns 1 if the formula parameter is not used in the current
 *  formula.  If the parameter is used, or not a formula fractal,
 *  a 0 is returned.  Note: this routine only works for formula types.
 */
int paramnotused(int parm)
{
   int ret = 0;

   /* sanity check */
   if (fractype != FFORMULA)
      return (0);

   switch (parm/2) {
      case 0:
         if (!uses_p1)
            ret = 1;
         break;
      case 1:
         if (!uses_p2)
            ret = 1;
         break;
      case 2:
         if (!uses_p3)
            ret = 1;
         break;
      case 3:
         if (!uses_p4)
            ret = 1;
         break;
      case 4:
         if (!uses_p5)
            ret = 1;
         break;
      default:
         ret = 0;
         break;
   }
   return (ret);
}

/* 
 *  Returns 1 if parameter number parm exists for type. If the
 *  parameter exists, the parameter prompt string is copied to buf. 
 *  Pass in NULL for buf if only the existence of the parameter is 
 *  needed, and not the prompt string.
 */
int typehasparm(int type,int parm,char *buf)
{
   int extra;
   char far *ret = NULL;
   if(0 <= parm && parm < 4)
      ret=fractalspecific[type].param[parm];
   else if(parm >= 4 && parm < MAXPARAMS)
      if((extra=find_extra_param(type)) > -1)
         ret=moreparams[extra].param[parm-4];
   if(ret)
      if(*ret == 0)
         ret = NULL;

   if(fractype == FFORMULA)
      if(paramnotused(parm))
           ret = NULL;

   if(ret && buf != NULL)
      far_strcpy(buf,ret);
   return(ret?1:0);
}
