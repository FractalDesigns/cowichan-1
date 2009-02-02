/**
 * Parallel implementation of conway's game of life
 *
 * \file parallel.cpp
 * \author Andrew Borzenko
 * \date 01-26-09
 */

#if NUMA
EXTERN_ENV
#endif

#include "../include/main.h"
#include "parallel.h"

// public

void
life_mpi(
  mpi::communicator world,
  bool1DX	matrix,			/* world to evolve */
  int		nr,			/* row size */
  int		nc,			/* column size */
  int		iters			/* number of iterations */
){
  int1DX		count;			/* neighborhood counts */
  int		i;			/* iteration index */
  int		r, c;			/* row/column indices */
  int		alive = nr * nc;	/* number alive */
#if GRAPHICS
  int		gfxCount = 0;
#endif

#if GRAPHICS
  gfx_life(gfxCount++, matrix, nr, nc);
#endif

  int		lo, hi, str;		/* work controls */
  bool		work;			/* useful work to do? */ 
  int is_alive = 1;
  int rank;

  // work
  printf ("world size is %d\n", world.size());
  work = get_block_rows_mpi (world, 0, nr, &lo, &hi, &str);
  printf ("lo is %d, hi is %d\n", lo, hi);
  for (i=0; (i<iters) && is_alive; i++){
    // fill neighborhood counts
    if (work) {
      for (r = lo; r < hi; r++) {
        life_row_mpi(matrix, count, nr, nc, r,
          (nr + r - 1) % nr, (nr + r + 1) % nr);
      }
    }
    // broadcast counts
    for (r = 0; r < nr; r++) {
      rank = get_block_rank_mpi (world, 0, nr, r);
      broadcast (world, &count[r * nc], nc, rank);
    }
    // update cells
    alive = 1;
    if (work) {
      for (r=lo; r<hi; r+=str) {
        for (c=0; c<nc; c++) {
          if ((count[r * nc + c] == 3) || ((count[r * nc + c] == 2) && matrix[r * nc + c])) {
            matrix[r * nc + c] = TRUE;
            alive++;
          }
          else {
            matrix[r * nc + c] = FALSE;
          }
        }
      }
    }
    // broadcast matrix
    for (r = 0; r < nr; r++) {
      rank = get_block_rank_mpi (world, 0, nr, r);
      broadcast (world, &matrix[r * nc], nc, rank);
    }
    // is_alive is maximum of local alive's
    if (world.rank () == 0) {
      reduce (world, alive, is_alive, mpi::maximum<int>(), 0);
    }
    else {
      reduce (world, alive, mpi::maximum<int>(), 0);
    }
    broadcast (world, is_alive, 0);

#if GRAPHICS
    gfx_life(gfxCount++, matrix, nr, nc);
#endif
  }

  /* check */
  if (is_alive == 0){
    fail("life", "no cells left alive", "iteration", "%d", i, NULL);
  }

  /* return */
}

// private

static void
life_one_mpi(
  bool1DX	matrix,			/* world to evolve */
  int1DX		count,			/* neighborhood counts */
  int		r,			/* this row */
  int		r_lo,			/* lower row */
  int		r_hi,			/* higher row */
  int		c,			/* this column */
  int		c_lo,			/* lower column */
  int		c_hi,			/* higher column */
  int   width
){
  count[r * width + c] = matrix[r_lo * width + c_lo] + matrix[r_lo * width + c] + matrix[r_lo * width + c_hi]
	      + matrix[r * width + c_lo]            +          matrix[r * width + c_hi]
	      + matrix[r_hi * width + c_lo] + matrix[r_hi * width + c] + matrix[r_hi * width + c_hi];
  /* return */
}

static void
life_row_mpi(
  bool1DX	matrix,			/* world to evolve */
  int1DX		count,			/* neighborhood counts */
  int		nr,			/* row size */
  int		nc,			/* column size */
  int		r,			/* this row */
  int		r_lo,			/* lower row */
  int		r_hi			/* higher row */
){
  int		c;			/* column index */

  life_one_mpi(matrix, count, r, r_lo, r_hi, 0, nc-1, 1, nc);
  for (c=1; c<(nc-1); c++){
    life_one_mpi(matrix, count, r, r_lo, r_hi, c, c-1, c+1, nc);
  }
  life_one_mpi(matrix, count, r, r_lo, r_hi, nc-1, nc-2, 0, nc);

  /* return */
}
