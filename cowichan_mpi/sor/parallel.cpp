/**
 * Parallel implementation of successive over-relaxation
 *
 * \file parallel.cpp
 * \author Andrew Borzenko
 * \date 03-02-09
 */

#include "../include/main.h"
#include "parallel.h"

/*
 * @ sor : do body of successive over-relaxation
 * > none
 * + relax matrix to find solution
 */

void sor_mpi (mpi::communicator world,
              real2D*	matrix,			/* to solve */
              real1D*	vector,			/* target vector */
              real1D*	answer,			/* solution found */
              int		n,			/* size */
              real		tol)			/* tolerance on answer */
{
  int		lo, hi;		/* work controls */
  int		blo, bhi;		/* work controls */
  bool		work;			/* do useful work? */
  int		r, c, t;		/* indices */
  real1D*	sums;			/* per-row sums */
  real		old, d;			/* temporaries */
  int		i;			/* loop index */
  real dmax_local;
  real dmax;

  /* more setup */
  for (i = 0; i < n; i++) {
    answer[i] = 1.0;
  }
  dmax = 2 * tol;			/* to forestall early exit */
  sums = new real1D[MAXEXT];

  /* work */
  work = get_block_rows_mpi (world, 0, n, &lo, &hi);
  for (t = 0; (t < SOR_MAX_ITERS) && (dmax >= tol); t++) {
    if (work) {
      // compute sums
      for (r = lo; r < hi; r++) {
	    sums[r] = 0.0;
        for (c = 0; c < r; c++) {
          sums[r] += matrix[r][c] * answer[c];
        }
        for (c = r + 1; c < n; c++) {
          sums[r] += matrix[r][c] * answer[c];
        }
      }
    }

    // broadcast sums
    for (i = 0; i < world.size (); i++) {
      if (get_block_rows_mpi (world, 0, n, &blo, &bhi, i)) {
        broadcast (world, &sums[blo], bhi - blo, i);
      }
    }

    if (work) {
      for (r = lo; r < hi; r++) {
        // compute difference
        dmax_local = 0.0;
        old = answer[r];
        answer[r] = (1.0 - SOR_OMEGA) * old
                  + SOR_OMEGA * (vector[r] - sums[r]) / matrix[r][r];
        d = (real) fabs ((double) (old - answer[r]));
        if (d > dmax_local) {
          dmax_local = d;
        }
      }
    }

    // broadcast next answer
    for (i = 0; i < world.size (); i++) {
      if (get_block_rows_mpi (world, 0, n, &blo, &bhi, i)) {
        broadcast (world, &answer[blo], bhi - blo, i);
      }
    }

    // compute overall difference
    all_reduce (world, dmax_local, dmax, mpi::maximum<real>());
  }

  delete [] sums;

  /* return */
}
