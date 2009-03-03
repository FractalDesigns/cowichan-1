/**
 * Serial implementation of histogram thresholding
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
thresh(
  int2D*		matrix,			/* to threshold */
  bool2D*	mask,			/* threshold mask */
  int		nr,			/* row size */
  int		nc,			/* column size */
  real		fraction		/* how much to keep */
);

#endif /* SERIAL_H */
