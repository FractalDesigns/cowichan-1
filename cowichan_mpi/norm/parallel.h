/**
 * Parallel implementation of vector norm
 *
 * \file parallel.h
 * \author Andrew Borzenko
 * \date 02-10-09
 */

#pragma once
#ifndef PARALLEL_H
#define PARALLEL_H

void norm_mpi (mpi::communicator world,
               pt1D* vec,      /* points to normalize */
               int  n);       /* length of vector */

#endif /* PARALLEL_H */
