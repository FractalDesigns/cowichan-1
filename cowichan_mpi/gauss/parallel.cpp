/**
 * Parallel implementation of generic gauss
 *
 * \file parallel.cpp
 * \author Andrew Borzenko
 * \date 02-03-09
 */

#include "../include/main.h"
#include "parallel.h"

void gauss_mpi (mpi::communicator world,
                real2D	matrix,			/* to solve */
                real1D	vector,			/* target vector */
                real1D	answer,			/* solution found */
                int		n)
{
  bool		work;			/* work control */
  int		lo, hi, str;		/* work controls */
  int		r, c, k;		/* indices */
#if GRAPHICS
  int		gfxCount = 0;
#endif
  int rank;

  /* forward elimination */
  for (k=0; k<n; k++){
#if GRAPHICS
    if (MASTER(tid)){
      gfx_gauss(gfxCount++, matrix, vector, answer, n);
    }
    thr_bar(tid);
#endif
    /* calculate pivots in k'th column */
    if ((work = get_block_rows_mpi (world, k + 1, n, &lo, &hi, &str))){
      for (r = lo; r < hi; r += str) {
        matrix[r][k] = matrix[r][k] / matrix[k][k];
      }
    }
    //thr_bar(tid);
    for (r = 0; r < n; r += str) {
      //rank = get_block_rank_mpi (world, r + 1, n, r);
      //broadcast ();
    }
    /* update elements below k'th row */
    if (work){
      for (r=lo; r<hi; r+=str){
	for (c=k+1; c<n; c++){
	  matrix[r][c] = matrix[r][c] - (matrix[r][k] * matrix[k][c]);
	}
      }
    }
    thr_bar(tid);
    /* update element of solution vector */
    if (work){
      for (r=lo; r<hi; r+=str){
	vector[r] = vector[r] - (matrix[r][k] * vector[k]);
      }
    }
    thr_bar(tid);
  }

  /* back substitution */
  for (k=(n-1); k>=0; k--){
    /* set this element */
    if (MASTER(tid)){
      answer[k] = vector[k]/matrix[k][k];
    }
    thr_bar(tid);
    /* update other elements */
    if (sch_work(ParWidth, tid, 0, k, &lo, &hi, &str)){
      for (r=lo; r<hi; r+=str){
	vector[r] = vector[r] - (matrix[r][k] * answer[k]);
      }
    }
    thr_bar(tid);
  }

  /* return */
}
