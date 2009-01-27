/**
 * Utility functions
 *
 * \file util.cpp
 * \author Andrew Borzenko
 * \date 01-26-09
 */

#include "../include/stdafx.h"

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

void print_world (bool2D world, int nr, int nc)
{
  int i, j;
  for (i = 0; i < MAXEXT; i++)
  {
    for (j = 0; j < MAXEXT; j++)
    {
      if (world[i][j] == 0) {
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

void print_world (int2D world, int nr, int nc)
{
  int i, j;
  for (i = 0; i < MAXEXT; i++)
  {
    for (j = 0; j < MAXEXT; j++)
    {
      printf ("%d", world[i][j]);
    }
    printf ("\n");
  }
  printf ("\n");
}
