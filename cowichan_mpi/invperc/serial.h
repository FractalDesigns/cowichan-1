/**
 * Serial implementation of invasion percolation
 *
 * \file serial.h
 * \author Andrew Borzenko
 * \date 02-28-09
 */

#pragma once
#ifndef SERIAL_H
#define SERIAL_H

/*--------------------------------------------------------------*/
/* private types						*/
/*--------------------------------------------------------------*/

typedef struct node_struct  node_t;
typedef struct node_struct* node_p;

struct node_struct {
  int		val;			/* matrix value */
  int		r, c;			/* location indices */
  node_p	next;			/* link */
}; 

/*--------------------------------------------------------------*/
/* public functions						*/
/*--------------------------------------------------------------*/

void
invperc(
  int2D*		matrix,			/* matrix to invade */
  bool2D*	mask,			/* mask to fill */
  int		nr,			/* row size */
  int		nc,			/* column size */
  real		fraction		/* how much to fill */
);

/*--------------------------------------------------------------*/
/* private functions 					*/
/*--------------------------------------------------------------*/

node_p
inv_deq(
  node_p	queue,			/* priority queue */
  int	      * r,			/* row index */
  int	      * c			/* column index */
);
node_p
inv_enq(
  node_p	queue,			/* where to enqueue */
  node_p	node			/* what to enqueue */
);
node_p
inv_node(
  int		val,			/* location value */
  int		r,			/* row index */
  int		c			/* column index */
);
node_p
inv_enqPt(
  int2D*		matrix,			/* matrix of values */
  bool2D*	mask,			/* mask to be filled */
  int		nr,			/* number of rows */
  int		nc,			/* number of columns */
  int		r,			/* point row */
  int		c,			/* point column */
  node_p	queue			/* priority queue */
);

#endif /* SERIAL_H */
