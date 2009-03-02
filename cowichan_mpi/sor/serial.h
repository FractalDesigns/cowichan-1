/**
 * Serial implementation of successive over-relaxation
 *
 * \file serial.h
 * \author Andrew Borzenko
 * \date 03-02-09
 */

#pragma once
#ifndef SERIAL_H
#define SERIAL_H

/*--------------------------------------------------------------*/
/* public functions						*/
/*--------------------------------------------------------------*/

void
sor(
  real2D*	matrix,			/* to solve */
  real1D*	vector,			/* target vector */
  real1D*	answer,			/* solution found */
  int		n,			/* size */
  real		tol			/* tolerance on answer */
);

#endif /* SERIAL_H */
