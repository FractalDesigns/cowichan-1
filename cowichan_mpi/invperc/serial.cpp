/**
 * Serial implementation of invasion percolation
 *
 * \file serial.cpp
 * \author Andrew Borzenko
 * \date 02-28-09
 */

#include "../include/main.h"
#include "serial.h"

// public

static int MaxChain;

/*
 * @ invperc : do invasion percolation
 * > none
 * + fill mask
 */

void
invperc(
  int2D*		matrix,			/* matrix to invade */
  bool2D*	mask,			/* mask to fill */
  int		nr,			/* row size */
  int		nc,			/* column size */
  real		fraction		/* how much to fill */
){
  node_p	queue = NULL;		/* priority queue */
  int		r, c;			/* row and column indices */
  int		num, i;			/* filling index */
#if GRAPHICS
  int		gfxCount = 0;
#endif

  /* initialize */
  num = (int)(fraction * nr * nc);
  queue = inv_node(matrix[nr/2][nc/2], nr/2, nc/2);
  MaxChain = 0;

  /* fill */
  for (i=0; i<num; i++){
    queue = inv_deq(queue, &r, &c);
    mask[r][c] = TRUE;
    queue = inv_enqPt(matrix, mask, nr, nc, r, c, queue);
#if GRAPHICS
    gfx_invperc(gfxCount++, matrix, mask, nr, nc, r, c);
#endif
  }

  printf ("MaxChain is %d\n", MaxChain);

  /* return */
}

// private

/*
 * @ inv_deq : take item out of priority queue
 * > queue after dequeueing
 * + fill row and column indices
 */

node_p inv_deq(
  node_p	queue,			/* priority queue */
  int	      * r,			/* row index */
  int	      * c			/* column index */
){
  node_p	result;			/* queue after dequeue */
  node_p	node;			/* node dequeued */

  node = queue;
  *r = node->r;
  *c = node->c;
  result = queue->next;
  //FREE(node);
  delete node;

  return result;
}

/*
 * @ inv_enq : add item to priority queue
 * > queue after dequeueing
 * + enqueue item in given tree
 */

node_p inv_enq(
  node_p	queue,			/* where to enqueue */
  node_p	node			/* what to enqueue */
){
  node_p	ptr;			/* for list traversal */
  node_p	result;			/* queue returned */
  int chain; // chain size (assessing performance)

  node->next = NULL;
  if (queue == NULL){
    result = node;
  } else if (node->val <= queue->val){
    node->next = queue;
    result = node;
  } else {
    result = queue;
    ptr = queue;
    chain = 1;
    while ((ptr->next != NULL) && (node->val > ptr->next->val)){
      ptr = ptr->next;
      chain++;
    }
    if (chain > MaxChain) {
      MaxChain = chain;
    }
    node->next = ptr->next;
    ptr->next = node;
  }

  return result;
}

/*
 * @ inv_node : allocate and fill queue node
 * > new node
 */

node_p inv_node(
  int		val,			/* location value */
  int		r,			/* row index */
  int		c			/* column index */
){
  node_p	result;

  //ALLOC(result, node_t, 1, "inv_node", "new node");
  result = new node_t;
  result->val  = val;
  result->r    = r;
  result->c    = c;
  result->next = NULL;

  return result;
}

/*
 * @ inv_enqPt : possibly add point to priority queue
 * > new queue
 * + possibly add point to priority queue
 */

node_p inv_enqPt(
  int2D*		matrix,			/* matrix of values */
  bool2D*	mask,			/* mask to be filled */
  int		nr,			/* number of rows */
  int		nc,			/* number of columns */
  int		r,			/* point row */
  int		c,			/* point column */
  node_p	queue			/* priority queue */
){
  node_p	result = queue;		/* new queue */
  bool		e[8];			/* empty neighbors */
  bool		r_lo = r > 0,
		r_hi = r < (nr-1),
		c_lo = c > 0,
		c_hi = c < (nc-1);

  /*   0   */
  /*  1A2  */
  /* 3BXC4 */
  /*  5D6  */
  /*   7   */
  if (r_lo){
    e[0] = ((r > 1) && !mask[r-2][c]) || (r - 2 < 0);
    e[1] = (c_lo && !mask[r-1][c-1]) || (r - 1 < 0) || (c - 1 < 0);
    e[2] = (c_hi && !mask[r-1][c+1]) || (r - 1 < 0) || (c + 1 > nc - 1);
  }
  e[3] = ((c > 1) && !mask[r][c-2]) || (c - 2 < 0);
  e[4] = ((c < (nc-2)) && !mask[r][c+2]) || (c + 2 > nc - 1);
  if (r_hi){
    e[5] = (c_lo && !mask[r+1][c-1]) || (r + 1 > nr - 1) || (c - 1 < 0);
    e[6] = (c_hi && !mask[r+1][c+1]) || (r + 1 > nr - 1) || (c + 1 > nc - 1);
    e[7] = ((r < (nr-2)) && !mask[r+2][c]) || (r + 2 > nr - 2);
  }

  if (r_lo && (!mask[r-1][c]) && e[0] && e[1] && e[2]){	/* A */
    result = inv_enq(result, inv_node(matrix[r-1][c], r-1, c));
  }
  if (c_lo && (!mask[r][c-1]) && e[1] && e[3] && e[5]){	/* B */
    result = inv_enq(result, inv_node(matrix[r][c-1], r, c-1));
  }
  if (c_hi && (!mask[r][c+1]) && e[2] && e[4] && e[6]){	/* C */
    result = inv_enq(result, inv_node(matrix[r][c+1], r, c+1));
  }
  if (r_hi && (!mask[r+1][c]) && e[5] && e[6] && e[7]){	/* D */
    result = inv_enq(result, inv_node(matrix[r+1][c], r+1, c));
  }

  return result;
}
