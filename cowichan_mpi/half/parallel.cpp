/**
 * Parallel implementation of halving shuffle
 *
 * \file parallel.cpp
 * \author Andrew Borzenko
 * \date 02-27-09
 */

#include "../include/main.h"
#include "parallel.h"

/*
 * @ half : body of halving shuffle
 * > none
 * + shuffle rows and columns
 */

void
half_mpi (mpi::communicator world,
  int2D*		matrix,			/* to shuffle */
  int		nr,			/* row size */
  int		nc			/* column size */
){
  bool work;
  int		lo, hi;		/* work controls */
  int		rlo, rhi;		/* work controls */
  int		r, c, i;		/* loop indices */

  int middle_r = (nr + 1) / 2;
  int middle_c = (nc + 1) / 2;
  int previous_r, previous_c;

  int2D* tmp_matrix = new int2D[MAXEXT];

  // work
  work = get_block_rows_mpi (world, 0, nr, &lo, &hi);

  // shuffle along rows
  if (work) {
    for (r = lo; r < hi; r++) {
      for (c = 0; c < nc; c++) {
        // get original column
				if (c < middle_c) {
					previous_c = c * 2;
				} else {
					previous_c = (c - middle_c) * 2 + 1;
        }
        tmp_matrix[r][c] = matrix[r][previous_c];
      }
    }
  }
  
  // broadcast tmp_matrix rows
  for (i = 0; i < world.size (); i++) {
    get_block_rows_mpi (world, 0, nr, &rlo, &rhi, i);
    broadcast (world, tmp_matrix[rlo], (rhi - rlo) * nc, i);
  }

  // shuffle along columns
  if (work) {
    for (r = lo; r < hi; r++) {
      for (c = 0; c < nc; c++) {
        // get original row
				if (r < middle_r) {
					previous_r = r * 2;
				} else {
					previous_r = (r - middle_r) * 2 + 1;
        }
        matrix[r][c] = tmp_matrix[previous_r][c];
      }
    }
  }

  // broadcast matrix rows
  for (i = 0; i < world.size (); i++) {
    get_block_rows_mpi (world, 0, nr, &rlo, &rhi, i);
    broadcast (world, matrix[rlo], (rhi - rlo) * nc, i);
  }

  // cleanup
  delete [] tmp_matrix;

  /* return */
}
