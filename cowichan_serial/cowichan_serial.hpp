/**
 * Datatypes and common routines for Cowichan programs.
 * Serial implementation.
 */
#ifndef __cowichan_serial_hpp__
#define __cowichan_serial_hpp__

#include "../cowichan/cowichan.hpp"

// COWICHAN DEFINITIONS =====================================================//
// aka. "inputs" to the toys, and chaining functions.
class CowichanSerial : public Cowichan {
protected: // chaining functions

  void mandel(IntMatrix matrix);
  void randmat(IntMatrix matrix);
  void half(IntMatrix matrixIn, IntMatrix matrixOut);
  void invperc(IntMatrix matrix, BoolMatrix mask);
  void thresh(IntMatrix matrix, BoolMatrix mask);
  void life(BoolMatrix matrixIn, BoolMatrix matrixOut);
  void winnow(IntMatrix matrix, BoolMatrix mask, PointList** points);
  void norm(PointList* pointsIn, PointList** pointsOut);
  void hull(PointList* pointsIn, PointList** pointsOut);
  void outer(PointList* points, Matrix* matrix, Vector* vector);
  void gauss(Matrix matrix, Vector target, Vector* solution);
  void sor(Matrix matrix, Vector target, Vector* solution);
  void product(Matrix matrix, Vector actual, Vector* solution);
  void vecdiff(Vector actual, Vector computed, real* e);

protected:

  /**
   * Runs the cowichan problem set, chained together.
   * @param use_randmat  true: generate a random matrix.
   *             false: use a window of the mandelbrot set.
   * @param use_thresh  true: use image thresholding for int->bool.
   *            false: use invasion percolation for int->bool.
   */
  void chain(bool use_randmat, bool use_thresh);

protected:

  // specific to life implementation

  /**
   * For ping-pong approach.
   */
  BoolMatrix first, second;

  /**
   * Calculate number of neighbours.
   */
  int sumNeighbours(int r, int c);

};

#endif

