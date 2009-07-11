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

