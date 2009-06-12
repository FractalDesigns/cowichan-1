/**
 * Defaults for Cowichan programs.
 */
#ifndef __cowichan_defaults_hpp__
#define __cowichan_defaults_hpp__

// common
#define ALL_NR 1000
#define ALL_NC 1000
#define ALL_N 1000
#define RAND_MEAN 0
#define RAND_RANGE 20

// chain
#define CHAIN_NR ALL_NR
#define CHAIN_NC ALL_NC

// mandel
#define MANDEL_NR ALL_NR
#define MANDEL_NC ALL_NC
#define MANDEL_X0 0
#define MANDEL_Y0 0;
#define MANDEL_DX 1.5;
#define MANDEL_DY 1.5;
#define MANDEL_INFINITY 2.0
#define MANDEL_MAX_ITER 150

// randmat
#define RANDMAT_NR ALL_NR
#define RANDMAT_NC ALL_NC
#define RANDMAT_SEED 681304
#define RANDMAT_A  1291
#define RANDMAT_C   917
#define RANDMAT_M 56197

// half
#define HALF_NR ALL_NR
#define HALF_NC ALL_NC

// invperc
#define INVPERC_NR ALL_NR
#define INVPERC_NC ALL_NC
#define INVPERC_NFILL 200

// thresh
#define THRESH_NR ALL_NR
#define THRESH_NC ALL_NC
#define THRESH_PERCENT 0.5

// life
#define LIFE_NR ALL_NR
#define LIFE_NC ALL_NC
#define LIFE_ITERATIONS 200

// winnow
#define WINNOW_NR ALL_NR
#define WINNOW_NC ALL_NC
#define WINNOW_N ALL_N

// norm
#define NORM_N ALL_N

// hull
#define HULL_N ALL_N

// outer
#define OUTER_N ALL_N

// gauss
#define GAUSS_N ALL_N

// sor
#define SOR_N ALL_N
#define SOR_OMEGA 0.9
#define SOR_TOLERANCE 10e-6
#define SOR_MAX_ITERS 1000000000
 
#endif

