/**
 * Matrix-vector product
 *
 * \file product.cpp
 * \author Andrew Borzenko
 * \date 02-02-09
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
  real1D	vector;			/* to be multiplied */
  real1D	result;			/* result of multiply */
  int		nr;			/* row size */
  int		nc;			/* column size */
  int limit;
  int i, j;

  //srand ((unsigned int) time (NULL));
  srand (222);

  nr = MAXEXT;
  nc = MAXEXT;
  limit = 10;

  for (i = 0; i < nr; i++)
  {
    for (j = 0; j < nc; j++)
    {
      matrix[i][j] = rand () % limit;
    }
  }

  for (i = 0; i < nr; i++)
  {
    vector[i] = rand () % limit;
  }

//  printf ("Matrix\n");
//  print_matrix (matrix, nr, nc);
//  printf ("x Vector\n");
//  print_vector (vector, nr);

  DWORD startTime,endTime;

  startTime = GetTickCount ();
  DWORD total = 0;

  for (i = 0; i < 100; i++) {
    total += product_mpi (world, matrix, vector, result, nr, nc);
  }

  endTime = GetTickCount ();

  printf ("Done without broadcast in %ums\n\n", total);
  printf ("Done in %ums\n\n", endTime - startTime);

//  printf ("=\n");
//  print_vector (result, nr);
#else
  real2D	matrix;			/* to multiply by */
  real1D	vector;			/* to be multiplied */
  real1D	result;			/* result of multiply */
  int		nr;			/* row size */
  int		nc;			/* column size */
  int limit;
  int i, j;

  //srand ((unsigned int) time (NULL));
  srand (222);

  nr = MAXEXT;
  nc = MAXEXT;
  limit = 10;

  for (i = 0; i < nr; i++)
  {
    for (j = 0; j < nc; j++)
    {
      matrix[i][j] = rand () % limit;
    }
  }

  for (i = 0; i < nr; i++)
  {
    vector[i] = rand () % limit;
  }

//  printf ("Matrix\n");
//  print_matrix (matrix, nr, nc);
//  printf ("x Vector\n");
//  print_vector (vector, nr);

  DWORD startTime,endTime;

  startTime = GetTickCount ();

  product (matrix, vector, result, nr, nc);

  endTime = GetTickCount ();

  printf ("Done in %ums\n\n", endTime - startTime);

//  printf ("=\n");
//  print_vector (result, nr);
#endif

	return 0;
}
