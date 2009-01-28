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
  int1DX		count_gathered;			/* gathered neighborhood counts */
  bool1DX		matrix_gathered;			/* gathered neighborhood counts */

  /* work */
  printf ("world size is %d\n", world.size());
  work = sch_block (world.size (), world.rank (), 0, nr, &lo, &hi, &str);
  printf ("lo is %d, hi is %d, stride is %d\n", lo, hi, str);
  for (i=0; (i<iters) && alive; i++){
    /* fill neighborhood counts */
    if (work) {
      for (r=lo; r<hi; r+=str) {
        life_row_mpi(matrix, count, nr, nc, r, (nr+r-1)%nr, (nr+r+1)%nr);
      }
    }
    // all_gather
    if (work) {
      all_gather (world, &count[r * nc], nc * ((hi - lo) / str), count_gathered);
    }
    print_matrix (count_gathered, nr, nc);
    /* update cells */
    alive = 0;
    if (work) {
      for (r=lo; r<hi; r+=str) {
        for (c=0; c<nc; c++) {
          if ((count_gathered[r * nc + c] == 3) || ((count_gathered[r * nc + c] == 2) && matrix[r * nc + c])) {
            matrix[r * nc + c] = TRUE;
            alive++;
          }
          else {
            matrix[r * nc + c] = FALSE;
          }
        }
      }
    }
    // all_gather
    if (work) {
      all_gather (world, &matrix[r * nc], nc * ((hi - lo) / str), matrix_gathered);
    }
    for (r=0; r<nr; r++) {
      for (c=0; c<nc; c++) {
        matrix[r * nc + c] = matrix_gathered[r * nc + c];
      }
    }

#if GRAPHICS
    gfx_life(gfxCount++, matrix, nr, nc);
#endif
  }

  /* check */
  if (alive == 0){
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
