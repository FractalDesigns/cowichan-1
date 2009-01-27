/**
 * Random matrix generation
 *
 * \file randmat.cpp
 * \author Andrew Borzenko
 * \date 01-26-09
 */

#include "stdafx.h"
#include "serial.h"
//#include "parallel.h"

int _tmain(int argc, _TCHAR* argv[])
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

  print_world (matrix, nr, nc);

	return 0;
}
