/**
 * Serial implementation of vector difference
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
vecdiff(
  real1D*	left,			/* left vector */
  real1D*	right,			/* right vector */
  int		n,			/* vector lengths */
  real	      * diff			/* norm-1 difference */
);

#endif /* SERIAL_H */
