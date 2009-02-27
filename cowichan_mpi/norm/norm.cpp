/**
 * Vector norm
 *
 * \file norm.cpp
 * \author Andrew Borzenko
 * \date 02-10-09
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

  pt1D* vec;
  int n;
  int limit;
  int i;

  n = MAXEXT;
  limit = MAXEXT;

  srand (333);

  vec = new pt1D[MAXEXT];
  for (i = 0; i < n; i++) {
    vec[i].x = rand () % limit;
    vec[i].y = rand () % limit;
    vec[i].w = rand () % limit;
  }

  printf ("Vector:\n");
  print_vector (vec, n);

#ifdef IS_PARALLEL
  norm_mpi (world, vec, n);
#else
  norm (vec, n);
#endif

  printf ("Norm:\n");
  print_vector (vec, n);

  delete [] vec;

  return 0;
}
