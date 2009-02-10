/**
 * Mandelbrot set implementation
 *
 * \file mandel.cpp
 * \author Andrew Borzenko
 * \date 02-09-09
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

  int2D	    matrix;			/* matrix to fill */
  int		nr, nc;			/* matrix size */
  real       base_x, base_y;
  real       ext_x, ext_y;

  nr = MAXEXT;
  nc = MAXEXT;
  base_x = 0;
  base_y = 0;
  ext_x = 1.5;
  ext_y = 1.5;

  mandel_mpi (world, matrix, nr, nc, base_x, base_y, ext_x, ext_y);

//  printf ("Mandelbrot set:\n");
//  print_matrix (matrix, nr, nc);
#else
  int2D	    matrix;			/* matrix to fill */
  int		nr, nc;			/* matrix size */
  real       base_x, base_y;
  real       ext_x, ext_y;

  nr = MAXEXT;
  nc = MAXEXT;
  base_x = 0;
  base_y = 0;
  ext_x = 1.5;
  ext_y = 1.5;

  mandel (matrix, nr, nc, base_x, base_y, ext_x, ext_y);

  printf ("Mandelbrot set:\n");
  print_matrix (matrix, nr, nc);
#endif

  return 0;
}
