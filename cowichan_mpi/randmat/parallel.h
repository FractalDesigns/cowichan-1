/**
 * Parallel implementation of random matrix generation
 *
 * \file parallel.h
 * \author Andrew Borzenko
 * \date 02-02-09
 */

#pragma once
#ifndef PARALLEL_H
#define PARALLEL_H

/*--------------------------------------------------------------*/
/* public functions						*/
/*--------------------------------------------------------------*/

/*
 * @ randmat : do random matrix generation
 * > none
 * + fill matrix
 */

void
randmat_mpi(
  mpi::communicator world,			/* own ID */
  int2D		matrix,			/* to fill */
  int		nr,			/* row size */
  int		nc,			/* column size */
  int		limit,			/* value limit */
  int		seed			/* RNG seed */
);

#endif /* PARALLEL_H */
