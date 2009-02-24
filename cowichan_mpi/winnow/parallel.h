/**
 * Parallel implementation of weighted point selection
 *
 * \file parallel.h
 * \author Andrew Borzenko
 * \date 02-23-09
 */

#pragma once
#ifndef PARALLEL_H
#define PARALLEL_H

/*--------------------------------------------------------------*/
/* private function prototypes					*/
/*--------------------------------------------------------------*/

static int
winnow_redBool2DCount_mpi(
  bool2D*	mask,			/* to reduce */
  int		nr,			/* row size */
  int		nc			/* column size */
);

/*--------------------------------------------------------------*/
/* public functions						*/
/*--------------------------------------------------------------*/

/*
 * @ winnow : do point winnowing
 * > none
 * + create vector of points
 */

void
winnow_mpi(mpi::communicator world,
  int2D*		matrix,			/* point values */
  bool2D*	mask,			/* suitable points */
  int		nr,			/* row size */
  int		nc,			/* column size */
  pt1D*		pt,			/* points to create */
  int		npt			/* number of points */
);

#endif /* PARALLEL_H */
