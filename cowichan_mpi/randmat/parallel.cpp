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

#define MAXPAR 100

// TODO: change to use std::vector
static int	state[MAXPAR];		/* random state vector */
static int	aPrime, cPrime;		/* modified constants */ 

// public

void
randmat_mpi (
  mpi::communicator world,			/* mpi communicator */
  int2D		matrix,			/* to fill */
  int		nr,			/* row size */
  int		nc,			/* column size */
  int		limit,			/* value limit */
  int		seed			/* RNG seed */
){
  int		i;			/* loop index */
  int		lo, hi, str;		/* work controls */
#if GRAPHICS
  int		gfxCount = 0;
#endif

  int rank = world.rank ();
  int size = world.size ();

  /* set up */
  if (rank == 0) {
    randStateInit(seed, size, state, &aPrime, &cPrime);
  }
  //thr_bar(tid);
  broadcast (world, state, size, 0);
  for (i = 0; i < size; i++) {
    printf ("state %d is %d\n", i, state[i]);
  }

  /* special scheduling */
  if (get_cyclic_rows_mpi (world, 0, nr*nc, &lo, &hi, &str)) {
    printf ("lo is %d, hi is %d, str is %d\n", lo, hi, str);
    for (i = lo; i < hi; i += str) {
      matrix[i / nr][i % nr] = state[rank] % limit;
      state[rank] = (aPrime * state[rank] + cPrime) % RAND_M;
    }
  }
  //thr_bar(tid);
  // broadcast

#if GRAPHICS
  if (MASTER(tid)){
    gfx_randmat(gfxCount++, matrix, nr, nc);
  }
  thr_bar(tid);
#endif

  /* return */
}
