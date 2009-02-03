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
      printf ("%d\t", matrix[i][j]);
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
      printf ("%d\t", matrix[i * nc + j]);
    }
    printf ("\n");
  }
  printf ("\n");
}

/**
 * Assign elements to this process
 *
 * \param world [in] Communicator
 * \param lo [in] Low element
 * \param hi [in] High element
 * \param start [out] Start element
 * \param end [out] End element
 * \param stride [out] Element stride
 * \return Returns whether at least one element is assigned
 */
bool get_block_rows_mpi (mpi::communicator world, int lo, int hi,
                         int* start, int* end, int* stride)
{
  int size = world.size ();
  int rank = world.rank ();
  
  int nl;	   /* number of elements */
  int num;	 /* number to do */
  int extra; /* spillage */

  nl    = hi - lo;
  num   = nl / size;
  extra = nl % size;

  if ((nl <= 0) || (rank >= nl)) {
    /* do nothing */
    *start = 0;
    *end = -1;
    *stride = 1;
  }
  else {
    /* do share of work */
    if (rank < extra){
      num += 1;
      *start = lo + rank * num;
    } else {
      *start = lo + (extra * (num + 1)) + ((rank - extra) * num);
    }
    *end = *start + num;
    *stride = 1;
  }

  return (*end != -1);
}

/**
 * Return which process is working on element
 *
 * \param world [in] Communicator
 * \param lo [in] Low element
 * \param hi [in] High element
 * \param element [in] Element
 * \return Returns process number assigned to element
 */
int get_block_rank_mpi (mpi::communicator world, int lo, int hi,
                        int element)
{
  int size = world.size ();
  int rank;

  int nl;	   /* number of elements */
  int num;	 /* number to do */
  int extra; /* spillage */

  nl    = hi - lo;
  num   = nl / size;
  extra = nl % size;

  if (element < lo + extra * (num + 1)) {
    rank = (element - lo) / (num + 1);
  }
  else {
    rank = (element - extra * (num + 1)) / num + extra;
  }

  return rank;
}

/**
 * Assign elements to this process
 *
 * \param world [in] Communicator
 * \param lo [in] Low element
 * \param hi [in] High element
 * \param start [out] Start element
 * \param end [out] End element
 * \param stride [out] Element stride
 * \return Returns whether at least one element is assigned
 */
bool get_cyclic_rows_mpi(mpi::communicator world, int lo, int hi,
                         int* start, int* end, int* stride)
{
  int size = world.size ();
  int rank = world.rank ();
  
  int		nl;			/* number of loops */

  nl = hi - lo;

  if ((nl <= 0) || (rank >= nl)){		/* do nothing */
    *start = 0;
    *end = -1;
    *stride = 1;
  } else {				/* do share of work */
    *start  = lo + rank;
    *end    = hi;
    *stride = size;
  }

  return (*end != -1);
}

/**
 * Return which process is working on element
 *
 * \param world [in] Communicator
 * \param lo [in] Low element
 * \param hi [in] High element
 * \param element [in] Element
 * \return Returns process number assigned to element
 */
int get_cyclic_rank_mpi (mpi::communicator world, int lo, int hi,
                        int element)
{
  int size = world.size ();
  int rank;

  int nl;	   /* number of rows */

  nl    = hi - lo;
  rank = (element - lo) % size;

  return rank;
}

/*
 * @ randStateInit : initialize parallel random state vector
 * > none
 * + fill vector and calculate constants
 */

void
randStateInit(
  unsigned int		seed,			/* RNG seed */
  int		width,			/* number of participants */
  unsigned int	      * state,			/* per-thread state vector */
  unsigned int	      * aPrime,			/* new multiplicative */
  unsigned int	      * cPrime			/* new additive value */
){
  int		i;			/* loop index */

  state[0] = seed % RAND_M;
  *aPrime = RAND_A;
  *cPrime = 1;
  for (i=1; i<width; i++){
    state[i] = (RAND_A * state[i-1] + RAND_C) % RAND_M;
    *cPrime = (*cPrime + *aPrime) % RAND_M;
    *aPrime = (*aPrime * RAND_A) % RAND_M;
  }
  *cPrime = (*cPrime * RAND_C) % RAND_M;

  /* return */
} 