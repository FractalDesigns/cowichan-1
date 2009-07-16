/**
 * Datatypes and common routines for Cowichan programs.
 * Intel Thread Building Blocks implementation.
 */
#ifndef __cowichan_tbb_hpp__
#define __cowichan_tbb_hpp__

#include "../cowichan/cowichan.hpp"

// THREADING BUILDING BLOCKS ================================================// 

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range2d.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
#include "tbb/parallel_sort.h"
using namespace tbb;

typedef blocked_range<index_t> Range;
typedef blocked_range2d<index_t, index_t> Range2D;

// COWICHAN DEFINITIONS =====================================================//
// aka. "inputs" to the toys, and chaining functions.
class CowichanTBB : public Cowichan {
protected: // chaining functions

  void mandel(IntMatrix matrix);
  void randmat(IntMatrix matrix);
  void half(IntMatrix matrixIn, IntMatrix matrixOut);
  void invperc(IntMatrix matrix, BoolMatrix mask);
  void thresh(IntMatrix matrix, BoolMatrix mask);
  void life(BoolMatrix matrixIn, BoolMatrix matrixOut);
  void winnow(IntMatrix matrix, BoolMatrix mask, PointVector points);
  void norm(PointVector pointsIn, PointVector pointsOut);
  void hull(PointVector pointsIn, PointVector pointsOut);
  void outer(PointVector points, Matrix matrix, Vector vector);
  void gauss(Matrix matrix, Vector target, Vector solution);
  void sor(Matrix matrix, Vector target, Vector solution);
  void product(Matrix matrix, Vector candidate, Vector solution);
  real vecdiff(Vector actual, Vector computed);

};

#endif

