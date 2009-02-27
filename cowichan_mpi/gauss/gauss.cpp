/**
 * Generic gauss implementation
 *
 * \file randmat.cpp
 * \author Andrew Borzenko
 * \date 02-03-09
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

  real2D*	matrix;			/* matrix x */
  real1D*	answer;			/* answer */
  real1D*	vector;			/* = vector */
  int		n;			/* matrix size */
  int limit;
  int i, j;

  srand (333);

  n = MAXEXT;
  limit = 10;

  matrix = new real2D[MAXEXT];
  for (i = 0; i < n; i++)
  {
    for (j = 0; j < n; j++)
    {
      matrix[i][j] = rand () % limit;
    }
  }

  vector = new real1D[MAXEXT];
  for (i = 0; i < n; i++) {
    vector[i] = rand () % limit;
  }

  answer = new real1D[MAXEXT];

  printf ("\n");

  printf ("Matrix\n");
  print_matrix (matrix, n, n);

  printf ("Target\n");
  print_vector (vector, n);

#ifdef IS_PARALLEL
  gauss_mpi (world, matrix, vector, answer, n);
#else
  gauss (matrix, vector, answer, n);
#endif

  printf ("Answer\n");
  print_vector (answer, n);

  delete [] matrix;
  delete [] vector;
  delete [] answer;

  return 0;
}
