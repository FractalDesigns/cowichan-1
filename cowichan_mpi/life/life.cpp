/**
 * Conway's game of life
 *
 * \file life.cpp
 * \author Andrew Borzenko
 * \date 01-26-09
 */

#include "../include/main.h"
#ifdef IS_PARALLEL
  #include "parallel.h"
#else
  #include "serial.h"
#endif

namespace mpi = boost::mpi;

int main(int argc, char* argv[])
{
#ifdef IS_PARALLEL
  mpi::environment env(argc, argv);
  mpi::communicator world;

  printf ("I am process %d\n", world.rank ());

  bool2D matrix; /* world to evolve */
  int    nr;    /* row size */
  int    nc;    /* column size */
  int    iters; /* number of iterations */

  int i, j;

  //srand ((unsigned int) time (NULL));
  srand (222);

  for (i = 0; i < MAXEXT; i++)
  {
    for (j = 0; j < MAXEXT; j++)
    {
      matrix[i][j] = rand () % 2;
    }
  }

  nr = MAXEXT;
  nc = MAXEXT;
  iters = 10;

  life_mpi (matrix, nr, nc, iters);

  print_matrix (matrix, nr, nc);
#else
  bool2D matrix; /* world to evolve */
  int    nr;    /* row size */
  int    nc;    /* column size */
  int    iters; /* number of iterations */

  int i, j;

  printf ("serial execution...\n");

  //srand ((unsigned int) time (NULL));
  srand (222);

  for (i = 0; i < MAXEXT; i++)
  {
    for (j = 0; j < MAXEXT; j++)
    {
      matrix[i][j] = rand () % 2;
    }
  }

  nr = MAXEXT;
  nc = MAXEXT;
  iters = 10;

  life (matrix, nr, nc, iters);

  print_matrix (matrix, nr, nc);
#endif

  return 0;
}
