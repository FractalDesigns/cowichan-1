/**
 * Defaults for Cowichan programs.
 */
#ifndef __cowichan_defaults_hpp__
#define __cowichan_defaults_hpp__

// common
#define ALL_NR 1000
#define ALL_NC 1000

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

// sor
#define SOR_OMEGA 0.9
#define SOR_TOLERANCE 10e-6
 
#endif

