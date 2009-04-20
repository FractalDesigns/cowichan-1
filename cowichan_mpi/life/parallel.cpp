/**
 * Parallel implementation of conway's game of life
 *
 * \file parallel.cpp
 * \author Andrew Borzenko
 * \date 01-26-09
 */

#include "../include/main.h"
#include "parallel.h"

// public

void
life_mpi(
  mpi::communicator world,
  bool2D*	matrix,          // world to evolve
  int nr,                  // row size
  int nc,                  // column size
  int iters                // number of iterations
){
  int i;                   // iteration index
  int r;                   // row index
  int alive;               // number alive
  int lo, hi;              // work controls
  int rlo, rhi;            // for broadcast
  bool work;               // useful work to do?
  int is_alive = 1;        // some cells still alive?
  bool2D* matrix2;         // new matrix
  bool2D* m_tmp;           // tmp pointer

  // initialize
  matrix2 = new bool2D[MAXEXT];

  // work
  work = get_block_rows_mpi (world, 0, nr, &lo, &hi);
  for (i = 0; (i < iters) && (is_alive > 0); i++) {
    // reset alive neighbour count
    alive = 0;

    // count neighbours and fill new matrix
    if (work) {
      for (r = lo; r < hi; r++) {
        life_row_mpi(matrix, matrix2, &alive, nr, nc, r,
          (nr + r - 1) % nr, (nr + r + 1) % nr);
      }
    }
    // broadcast matrix
    for (r = 0; r < world.size (); r++) {
      if (get_block_rows_mpi (world, 0, nr, &rlo, &rhi, r)) {
        broadcast (world, matrix2[rlo], (rhi - rlo) * nc, r);
      }
    }
    // is_alive is maximum of local alive's
    all_reduce (world, alive, is_alive, mpi::maximum<int>());
    m_tmp = matrix;
    matrix = matrix2;
    matrix2 = m_tmp;
  }

  delete [] matrix2;

  // check
  ASSERT (is_alive > 0);
}

// private

void
life_one_mpi(
  bool2D*       matrix,                 // old world
  bool2D*       matrix2,                // new world
  int*          alive,                  // alive count
  int           r,                      // this row
  int           r_lo,                   // lower row
  int           r_hi,                   // higher row
  int           c,                      // this column
  int           c_lo,                   // lower column
  int           c_hi                    // higher column
){
  int count;                            // neighbour count

  count = matrix[r_lo][c_lo] + matrix[r_lo][c] + matrix[r_lo][c_hi]
          + matrix[r][c_lo]            +          matrix[r][c_hi]
          + matrix[r_hi][c_lo] + matrix[r_hi][c] + matrix[r_hi][c_hi];

  if ((count == 3) || ((count == 2) && matrix[r][c])) {
    matrix2[r][c] = TRUE;
    (*alive)++;
  }
  else {
    matrix2[r][c] = FALSE;
  }
}

void
life_row_mpi(
  bool2D*       matrix,                 // old world
  bool2D*       matrix2,                // new world
  int*          alive,                  // alive count
  int           nr,                     // row size
  int           nc,                     // column size
  int           r,                      // this row
  int           r_lo,                   // lower row
  int           r_hi                    // higher row
){
  int c;                                // column index

  life_one_mpi(matrix, matrix2, alive, r, r_lo, r_hi, 0, nc-1, 1);
  for (c=1; c<(nc-1); c++){
    life_one_mpi(matrix, matrix2, alive, r, r_lo, r_hi, c, c-1, c+1);
  }
  life_one_mpi(matrix, matrix2, alive, r, r_lo, r_hi, nc-1, nc-2, 0);
}
