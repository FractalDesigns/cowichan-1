/**
 * Serial implementation of outer product matrix
 *
 * \file serial.cpp
 * \author Andrew Borzenko
 * \date 02-27-09
 */

#include "../include/main.h"
#include "serial.h"

// public

/*
 * @ outer : calculate outer product matrix
 * > none
 * + fill matrix
 */

void
outer(
  pt1D*		ptVec,			/* vector of points */
  real2D*	matrix,			/* matrix to fill */
  real1D*	realVec,		/* vector to fill */
  int		n			/* size */
){
  real		d;			/* distance between points */
  real		dMax = -1.0;		/* maximum distance */
  int		r, c;			/* loop indices */

  /* all elements except matrix diagonal */
  for (r=0; r<n; r++){
    realVec[r] = ptMag(&(ptVec[r]));
    for (c=0; c<r; c++){
      d = ptDist(&(ptVec[r]), &(ptVec[c]));
      if (d > dMax){
        dMax = d;
      }
      matrix[r][c] = matrix[c][r] = d;
    }
  }

  /* matrix diagonal */
  dMax *= n;
  for (r=0; r<n; r++){
    matrix[r][r] = dMax;
  }

  /* return */
}
