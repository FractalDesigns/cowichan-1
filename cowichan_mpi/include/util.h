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

void print_world (bool2D world, int nr, int nc);
void print_world (int2D world, int nr, int nc);
