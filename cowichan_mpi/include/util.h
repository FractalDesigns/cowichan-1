/**
 * Utility functions
 *
 * \file util.h
 * \author Andrew Borzenko
 * \date 01-26-09
 */

void
fail(
  char	      * caller,			/* calling function */
  char	      * descrip,		/* error description */
  ...					/* other things to print */
);

void print_matrix (bool2D matrix, int nr, int nc);
void print_matrix (int2D matrix, int nr, int nc);
void print_matrix (bool1DX matrix, int nr, int nc);
void print_matrix (int1DX matrix, int nr, int nc);

bool
sch_block(
  int		n,			/* number of threads */
  int		i,			/* this thread's ID */
  int		base,			/* base of loop section */
  int		lim,			/* limit of loop section */
  int	      * start,			/* loop start */
  int	      * end,			/* loop end */
  int	      * stride			/* loop stride */
);

bool
sch_cyclic(
  int		n,			/* number of threads */
  int		i,			/* this thread's ID */
  int		base,			/* base of loop section */
  int		lim,			/* limit of loop section */
  int	      * start,			/* loop start */
  int	      * end,			/* loop end */
  int	      * stride			/* loop stride */
);
