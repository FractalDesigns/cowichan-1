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
#endif

  int2D* matrix; /* to fill */
  int   nr;     /* row size */
  int   nc;     /* column size */
  unsigned int   limit;  /* value limit */
  unsigned int   seed;   /* RNG seed */

  nr = MAXEXT;
  nc = MAXEXT;
  limit = 10;
  seed = 222;

  matrix = new int2D[MAXEXT];

#ifdef IS_PARALLEL
  randmat_mpi (world, matrix, nr, nc, limit, seed);
#else
  randmat (matrix, nr, nc, limit, seed);
#endif

  print_matrix (matrix, nr, nc);

  delete [] matrix;

  return 0;
}
