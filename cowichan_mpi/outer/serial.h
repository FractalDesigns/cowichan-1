/**
 * Serial implementation of outer product matrix
 *
 * \file serial.h
 * \author Andrew Borzenko
 * \date 02-27-09
 */

#pragma once
#ifndef SERIAL_H
#define SERIAL_H

/*--------------------------------------------------------------*/
/* public functions						*/
/*--------------------------------------------------------------*/

void
outer(
  pt1D*		ptVec,			/* vector of points */
  real2D*	matrix,			/* matrix to fill */
  real1D*	realVec,		/* vector to fill */
  int		n			/* size */
);

#endif /* SERIAL_H */
