/**
 * Random matrix generation
 *
 * \file randmat.cpp
 * \author Andrew Borzenko
 * \date 01-26-09
 */

#include "../include/main.h"
#ifdef IS_PARALLEL
  #include "parallel.h"
#else
  #include "serial.h"
#endif

int main(int argc, char* argv[])
{
#ifdef IS_PARALLEL
  mpi::environment env(argc, argv);
  mpi::communicator world;

  printf ("I am process %d\n", world.rank ());

  int2D matrix; /* to fill */
  int   nr;     /* row size */
  int   nc;     /* column size */
  unsigned int   limit;  /* value limit */
  unsigned int   seed;   /* RNG seed */

  nr = MAXEXT;
  nc = MAXEXT;
  limit = 10;
  seed = 222;

  randmat_mpi (world, matrix, nr, nc, limit, seed);

  print_matrix (matrix, nr, nc);
#else
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
#endif

	return 0;
}
