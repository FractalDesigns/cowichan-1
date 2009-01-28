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
