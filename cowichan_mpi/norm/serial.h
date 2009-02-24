/**
 * Serial implementation of vector norm
 *
 * \file serial.h
 * \author Andrew Borzenko
 * \date 02-10-09
 */

#pragma once
#ifndef SERIAL_H
#define SERIAL_H

/*
 * @ norm : do coordinate normalization
 * > none
 * + normalize point coordinates
 */

void
norm(
  pt1D*		vec,			/* points to normalize */
  int		n			/* length of vector */
);

#endif /* SERIAL_H */
