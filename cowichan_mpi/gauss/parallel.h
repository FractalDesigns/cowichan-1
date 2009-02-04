/**
 * Parallel implementation of generic gauss
 *
 * \file parallel.h
 * \author Andrew Borzenko
 * \date 02-03-09
 */

#pragma once
#ifndef PARALLEL_H
#define PARALLEL_H

/*--------------------------------------------------------------*/
/* public functions						*/
/*--------------------------------------------------------------*/

void gauss_mpi (mpi::communicator world,
                real2D	matrix,			/* to solve */
                real1D	vector,			/* target vector */
                real1D	answer,			/* solution found */
                int		n);			/* size */

#endif /* PARALLEL_H */
