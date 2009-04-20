/**
 * Parallel implementation of vector norm
 *
 * \file parallel.cpp
 * \author Andrew Borzenko
 * \date 02-10-09
 */

#include "../include/main.h"
#include "parallel.h"

void norm_mpi (mpi::communicator world,
               pt1D* vec,    // points to normalize
               int  n)       // length of vector
{
  pt ptMin_local, ptMax_local; //pseudo-points
  pt ptMin, ptMax; // pseudo-points
  real sclX, sclY; // scaling factors
  int i; // loop index
  int lo, hi; // work controls
  bool work; // useful work to do?

  // initialize
  ptMin_local = vec[0];
  ptMax_local = vec[0];

  work = get_block_rows_mpi (world, 0, n, &lo, &hi);
  if (work) {
    redPt1DPos(&vec[lo], hi - lo, &ptMin_local, &ptMax_local);
  }

  all_reduce (world, ptMin_local, ptMin, minimum_pt ());
  all_reduce (world, ptMax_local, ptMax, maximum_pt ());

  if (work) {
    // scaling factors
    sclX = (ptMax.x == ptMin.x) ? 0.0 : 1/(ptMax.x - ptMin.x);
    sclY = (ptMax.y == ptMin.y) ? 0.0 : 1/(ptMax.y - ptMin.y);
    // scale
    for (i = lo; i < hi; i++) {
      vec[i].x = sclX * (vec[i].x - ptMin.x);
      vec[i].y = sclY * (vec[i].y - ptMin.y);
    }
  }

  // broadcast normalized values
  for (i = 0; i < world.size (); i++) {
    if (get_block_rows_mpi (world, 0, n, &lo, &hi, i)) {
      broadcast (world, &vec[lo], hi - lo, i);
    }
  }

  /* return */
}
