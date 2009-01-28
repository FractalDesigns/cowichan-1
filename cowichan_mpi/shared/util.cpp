/**
 * Utility functions
 *
 * \file util.cpp
 * \author Andrew Borzenko
 * \date 01-26-09
 */

#include "../include/main.h"

/*
 * @ fail : generic failure handler
 * > none (never returns)
 */

void
fail(
  char	      * caller,			/* calling function */
  char	      * descrip,		/* error description */
  ...					/* other things to print */
){
  va_list	stackPtr;		/* stack pointer */
  char	      * msg;			/* error string */
  char	      * fmt;			/* error format */
  int		i_val;			/* integer value */
  real		r_val;			/* real value (NOTE TYPE) */
  char	      * s_val;			/* string value */

  ASSERT(caller  != NULL);
  ASSERT(descrip != NULL);

  fflush(stdout);
  fflush(stderr);
  fprintf(stderr, "ERROR (%s): %s\n", caller, descrip);

  va_start(stackPtr, descrip);
  while ((msg = va_arg(stackPtr, char*)) != NULL){
    if ((fmt = va_arg(stackPtr, char*)) == NULL){
      fprintf(stderr, "fail: null format for %s\n", msg);
      exit(1);
    } else if ((fmt[0] != '%') || (fmt[1] == '\0')){
      fprintf(stderr, "fail: bad format string \"%s\" for %s\n", fmt, msg);
      exit(1);
    } else {
      switch(fmt[1]){
	case 'd' :
	  i_val = va_arg(stackPtr, int);
	  fprintf(stderr, "%s: %d\n", msg, i_val);
	  break;
	case 'r' :
	  r_val = va_arg(stackPtr, real);
	  fprintf(stderr, "%s: ", msg);
	  fprintf(stderr, FMT_REAL_WR, r_val);
	  break;
	case 's' :
	  s_val = va_arg(stackPtr, char*);
	  if (s_val == NULL){
	    fprintf(stderr, "%s: <<NULL>>\n", msg);
	  } else {
	    fprintf(stderr, "%s: \"%s\"\n", msg, s_val);
	  }
	  break;
	case 'x' :
	  i_val = va_arg(stackPtr, int);
	  fprintf(stderr, "%s: %08x\n", msg, i_val);
	  break;
	default :
	  fprintf(stderr, "fail: unrecognized format \"%s\"\n", fmt);
	  exit(1);
      }
      fflush(stderr);
    }
  }
  va_end(stackPtr);
  fflush(stderr);

  exit(1);

  /* return */
}

void print_matrix (bool2D matrix, int nr, int nc)
{
  int i, j;
  for (i = 0; i < nr; i++)
  {
    for (j = 0; j < nc; j++)
    {
      if (matrix[i][j] == 0) {
        printf ("0");
      }
      else {
        printf ("1");
      }
    }
    printf ("\n");
  }
  printf ("\n");
}

void print_matrix (int2D matrix, int nr, int nc)
{
  int i, j;
  for (i = 0; i < nr; i++)
  {
    for (j = 0; j < nc; j++)
    {
      printf ("%d", matrix[i][j]);
    }
    printf ("\n");
  }
  printf ("\n");
}

void print_matrix (bool1DX matrix, int nr, int nc)
{
  int i, j;
  for (i = 0; i < nr; i++)
  {
    for (j = 0; j < nc; j++)
    {
      if (matrix[i * nc + j] == 0) {
        printf ("0");
      }
      else {
        printf ("1");
      }
    }
    printf ("\n");
  }
  printf ("\n");
}

void print_matrix (int1DX matrix, int nr, int nc)
{
  int i, j;
  for (i = 0; i < nr; i++)
  {
    for (j = 0; j < nc; j++)
    {
      printf ("%d", matrix[i * nc + j]);
    }
    printf ("\n");
  }
  printf ("\n");
}

/*
 * @ sch_block : block scheduling function
 * > none
 * + set start/end/stride values for block work allocation
 */

bool
sch_block(
  int		n,			/* number of threads */
  int		i,			/* this thread's ID */
  int		base,			/* base of loop section */
  int		lim,			/* limit of loop section */
  int	      * start,			/* loop start */
  int	      * end,			/* loop end */
  int	      * stride			/* loop stride */
){
  int		nl;			/* number of loops */
  int		num;			/* number to do */
  int		extra;			/* spillage */

  nl    = lim - base;
  num   = nl / n;
  extra = nl % n;

  if ((nl <= 0) || (i >= nl)){		/* do nothing */
    *start = 0;
    *end = -1;
    *stride = 1;
  } else {				/* do share of work */
    if (i < extra){
      num += 1;
      *start = base + i * num;
    } else {
      *start = base + (extra * (num + 1)) + ((i - extra) * num);
    }
    *end = *start + num;
    *stride = 1;
  }

  return (*end != -1);
}

/*
 * @ sch_cyclic : cyclic scheduling function
 * > none
 * + set start/end/stride values for cyclic work allocation
 */

bool
sch_cyclic(
  int		n,			/* number of threads */
  int		i,			/* this thread's ID */
  int		base,			/* base of loop section */
  int		lim,			/* limit of loop section */
  int	      * start,			/* loop start */
  int	      * end,			/* loop end */
  int	      * stride			/* loop stride */
){
  int		nl;			/* number of loops */

  nl = lim - base;

  if ((nl <= 0) || (i >= nl)){		/* do nothing */
    *start = 0;
    *end = -1;
    *stride = 1;
  } else {				/* do share of work */
    *start  = base + i;
    *end    = lim;
    *stride = n;
  }

  return (*end != -1);
}
