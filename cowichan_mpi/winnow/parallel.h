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

void
winnow_copy(mpi::communicator world,
  int2D*		matrix,			/* matrix of values */
  bool2D*	mask,			/* mask on values */
  int		nr,			/* row size */
  int		nc			/* column size */
);
void
winnow_count(mpi::communicator world,
  bool2D*	mask,			/* mask on points */
  int		nr,			/* row size */
  int		nc			/* column size */
);
void
winnow_pack(
  pt1D*		ptDst,			/* to pack into */
  int		nDst,			/* number of points */
  pt1D*		ptSrc,			/* to pull from */
  int		nSrc,			/* number of tmps */
  int		nt,			/* number of threads */
  int		rank
);
void
winnow_psrs_1(
  mpi::communicator world
);
void
winnow_psrs_2(
  mpi::communicator world
);
void
winnow_psrs_3(
  mpi::communicator world
);
void
winnow_psrs_4(
  mpi::communicator world
);
void
winnow_sched(
  int		rank,			/* caller thread ID */
  int	      * start,			/* start of own interval */
  int	      * end			/* end of own interval */
); 

int
scanIntSum(mpi::communicator world,
  int	      * vec,			/* to sum */
  int		len			/* vector length */
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
