/**
 * Parallel implementation of outer product matrix
 *
 * \file parallel.h
 * \author Andrew Borzenko
 * \date 02-27-09
 */

#pragma once
#ifndef PARALLEL_H
#define PARALLEL_H

/*--------------------------------------------------------------*/
/* public functions						*/
/*--------------------------------------------------------------*/

void
outer_mpi(mpi::communicator world,
  pt1D*		ptVec,			/* vector of points */
  real2D*	matrix,			/* matrix to fill */
  real1D*	realVec,		/* vector to fill */
  int		n			/* size */
);

#endif /* PARALLEL_H */
