/**
 * Random matrix generation
 *
 * \file randmat.cpp
 * \author Andrew Borzenko
 * \date 01-26-09
 */

#include "../include/main.h"
#include "serial.h"
//#include "parallel.h"

int main(int argc, char* argv[])
{
  int2D matrix; /* to fill */
  int   nr;     /* row size */
  int   nc;     /* column size */
  int   limit;  /* value limit */
  int   seed;   /* RNG seed */

  nr = MAXEXT;
  nc = MAXEXT;
  limit = 10;
  seed = 222;

  randmat (matrix, nr, nc, limit, seed);

  print_matrix (matrix, nr, nc);

	return 0;
}
