/*==============================================================*/
/* generic/hdr/generic.h : generic definitions			*/
/*==============================================================*/

/*--------------------------------------------------------------*/
/* miscellaneous definitions					*/
/*--------------------------------------------------------------*/

/* Booleans */

#define TRUE 1
#define FALSE 0

/*--------------------------------------------------------------*/
/* parameters							*/
/*--------------------------------------------------------------*/

#define MANDEL_INFINITY 2.0
#define MANDEL_MAX_ITER 150

#define RAND_A  1291
#define RAND_C   917
#define RAND_M 56197

#define SOR_MAX_ITERS 1000000
#define SOR_OMEGA 1.25

/*--------------------------------------------------------------*/
/* functional macros						*/
/*--------------------------------------------------------------*/

/* assertion */

#define ASSERT(cond_)\
  if (!(cond_)){\
    fprintf(stderr, "\tassertion failed in %s at %d\n", __FILE__, __LINE__);\
    exit(1);\
  }

/* integer ceiling */

#define INT_CEIL(num_, denom_) ((num_ + denom_ - 1) / denom_)

