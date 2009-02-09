/**
 * Serial implementation of Mandelbrot set
 *
 * \file serial.h
 * \author Andrew Borzenko
 * \date 02-09-09
 */

#pragma once
#ifndef SERIAL_H
#define SERIAL_H

/*--------------------------------------------------------------*/
/* public functions						*/
/*--------------------------------------------------------------*/

/*
 * @ mandel : calculate Mandelbrot Set
 * > none
 * + fill matrix
 */

void
mandel(
  int2D		matrix,			/* to fill */
  int		nr,			/* row size */
  int		nc,			/* column size */
  real		base_x,			/* lower left corner */
  real		base_y,			/* lower left corner */
  real		ext_x,			/* extent */
  real		ext_y			/* extent */
);


/*--------------------------------------------------------------*/
/* private functions						*/
/*--------------------------------------------------------------*/

int mandel_calc(
  real		x,			/* x coordinate */
  real		y			/* y coordinate */
);

#endif /* SERIAL_H */
