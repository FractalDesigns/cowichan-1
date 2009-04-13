/**
 * Parallel implementation of vector norm
 *
 * \file parallel.h
 * \author Andrew Borzenko
 * \date 02-10-09
 */

#pragma once
#ifndef NORM_PARALLEL_H
#define NORM_PARALLEL_H

struct minimum_pt {
  pt operator()(pt a, pt b)
  {
    if (b.x < a.x) {
      a.x = b.x;
    }
    if (b.y < a.y) {
      a.y = b.y;
    }
    return a;
  }
};

struct maximum_pt {
  pt operator()(pt a, pt b)
  {
    if (b.x > a.x) {
      a.x = b.x;
    }
    if (b.y > a.y) {
      a.y = b.y;
    }
    return a;
  }
};

void norm_mpi (mpi::communicator world,
               pt1D* vec,      /* points to normalize */
               int  n);       /* length of vector */

#endif /* NORM_PARALLEL_H */
