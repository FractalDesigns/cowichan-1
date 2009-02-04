/**
 * Parallel implementation of matrix-vector product
 *
 * \file parallel.cpp
 * \author Andrew Borzenko
 * \date 02-02-09
 */

#include "../include/main.h"
#include "parallel.h"

void product_mpi (mpi::communicator world,
                  real2D matrix,           /* to multiply by */
                  real1D	vector,          /* to be multiplied */
                  real1D	result,          /* result of multiply */
                  int   nr,                /* row size */
                  int		nc)                /* column size */
{
  int		lo, hi, str;		/* work controls */
  int		r, c;			/* loop indices */ 
  int rank;

  // work
  if (get_block_rows_mpi (world, 0, nr, &lo, &hi, &str)) {
    printf ("lo is %d, hi is %d, str is %d\n", lo, hi, str);
    for (r = lo; r < hi; r += str) {
      result[r] = matrix[r][0] * vector[0];
      for (c = 1; c < nc; c++) {
        result[r] += matrix[r][c] * vector[c];
      }
    }
  }
  else {
    printf ("no work\n");
  }
  // broadcast result
  for (r = 0; r < nr; r++) {
    rank = get_block_rank_mpi (world, 0, nr, r);
    broadcast (world, result[r], rank);
  }
}