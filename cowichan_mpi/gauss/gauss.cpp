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

  real2D	matrix;			/* to multiply by */
  real1D	vector = {197,129,175,209,125,228,152,176,118,262};			/* to be multiplied */
  real1D	answer;			/* result of multiply */
  int		n;			/* matrix size */
  int limit;
  int i, j;

  //srand ((unsigned int) time (NULL));
  srand (222);

  n = MAXEXT;
  limit = 10;

  for (i = 0; i < n; i++)
  {
    for (j = 0; j < n; j++)
    {
      matrix[i][j] = rand () % limit;
    }
  }

  printf ("\n");

  printf ("Matrix\n");
  print_matrix (matrix, n, n);

  printf ("Target\n");
  print_vector (vector, n);

  gauss_mpi (world, matrix, vector, answer, n);

  printf ("Answer\n");
  print_vector (answer, n);
#else
  real2D	matrix;			/* matrix x */
  real1D	answer;			/* answer */
  real1D	vector;			/* = vector */
  int		n;			/* matrix size */
  int limit;
  int i, j;

  //srand ((unsigned int) time (NULL));
  srand (333);

  n = MAXEXT;
  limit = 10;

  for (i = 0; i < n; i++)
  {
    for (j = 0; j < n; j++)
    {
      matrix[i][j] = rand () % limit;
    }
  }

  for (i = 0; i < n; i++) {
    vector[i] = rand () % limit;
  }

  printf ("\n");

  printf ("Matrix\n");
  print_matrix (matrix, n, n);

  printf ("Target\n");
  print_vector (vector, n);

  gauss (matrix, vector, answer, n);

  printf ("Answer\n");
  print_vector (answer, n);

#endif

	return 0;
}
