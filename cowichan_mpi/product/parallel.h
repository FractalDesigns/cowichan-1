/**
 * Parallel implementation of matrix-vector product
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

DWORD product_mpi(mpi::communicator world,
                 real2D matrix,          /* to multiply by */
                 real1D	vector,          /* to be multiplied */
                 real1D	result,          /* result of multiply */
                 int  nr,                /* row size */
                 int  nc                 /* column size */
);

#endif /* PARALLEL_H */
