/**
 * Chained cowichan implementations
 *
 * \file chain.cpp
 * \author Andrew Borzenko
 * \date 03-13-09
 */

#include "../include/main.h"
#ifdef IS_PARALLEL
  #include "../randmat/parallel.h"
#else
  #include "../randmat/serial.h"
#endif

int main(int argc, char* argv[])
{
#ifdef IS_PARALLEL
  mpi::environment env(argc, argv);
  mpi::communicator world;

#ifdef TEST_OUTPUT
  printf ("I am process %d\n", world.rank ());
#endif
#endif

  int2D* matrix;
  int nr;
  int nc;
  unsigned int limit;
  unsigned int seed;

  nr = MAXEXT;
  nc = MAXEXT;
  limit = 10;
  seed = 222;

  matrix = new int2D[MAXEXT];

#ifdef TEST_TIME
  INT64 start, end;
  start = get_ticks ();
#endif

#ifdef IS_PARALLEL
  randmat_mpi (world, matrix, nr, nc, limit, seed);
#else
  randmat (matrix, nr, nc, limit, seed);
#endif

#ifdef TEST_TIME
  end = get_ticks ();
  print_elapsed_time (start, end);
#endif

  delete [] matrix;

  return 0;
}
