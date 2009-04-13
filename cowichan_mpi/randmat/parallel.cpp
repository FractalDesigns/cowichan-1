/**
 * Parallel implementation of random matrix generation
 *
 * \file parallel.cpp
 * \author Andrew Borzenko
 * \date 02-02-09
 */

#include "../include/main.h"
#include "parallel.h"

// shared data structures

static unsigned int* state;		/* random state vector */
static unsigned int aPrime, cPrime;		/* modified constants */ 

// public

void
randmat_mpi (
  mpi::communicator world,			/* mpi communicator */
  int2D*		matrix,			/* to fill */
  int		nr,			/* row size */
  int		nc,			/* column size */
  unsigned int		limit,			/* value limit */
  unsigned int		seed			/* RNG seed */
){
  int rlo, rhi;
  int r, c;

  int size = nr;

  state = new unsigned int[size];

  // set up
  if (world.rank () == 0) {
    randStateInit(seed, size, state, &aPrime, &cPrime);
  }
  // broadcast set up
  broadcast (world, state, size, 0);
  broadcast (world, aPrime, 0);
  broadcast (world, cPrime, 0);

  // assign rows to processes
  if (get_block_rows_mpi (world, 0, nr, &rlo, &rhi)) {
    for (r = rlo; r < rhi; r++) {
      for (c = 0; c < nc; c++) {
        matrix[r][c] = state[r] % limit;
        state[r] = (aPrime * state[r] + cPrime) % RAND_M;
      }
    }
  }
  
  // broadcast matrix rows
  for (r = 0; r < world.size (); r++) {
    get_block_rows_mpi (world, 0, nr, &rlo, &rhi, r);
    broadcast (world, matrix[rlo], (rhi - rlo) * nc, r);
  }

  delete [] state;

  /* return */
}
