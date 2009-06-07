/**
 * Datatypes and common routines for Cowichan programs.
 */
#ifndef __cowichan_hpp__
#define __cowichan_hpp__

#include "cowichan_defaults.hpp"

// BASIC HEADERS ============================================================//
#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <algorithm>
#include <iomanip>
#include <vector>
#include <climits>
#include <limits>
#include <string>
using std::numeric_limits;

// TIMING ===================================================================//

#define TEST_TIME

#if defined(WIN32)   // Windows
  #include <windows.h>
#else                // Linux
  #include <sys/times.h>
  typedef uint64_t INT64;
#endif

INT64 get_ticks ();
INT64 get_freq ();
void print_elapsed_time (INT64 start, INT64 end);

/**
 * Does a sort of swap-out, printing progress.
 */
void timeInfo(INT64 *start, INT64 *end, std::string message);

// BASIC TYPES ==============================================================//
#ifndef REAL_TYPE
  // use IEEE single floating-point by default
  #define REAL_TYPE float
#endif
typedef REAL_TYPE  real;
typedef unsigned int uint;

typedef uint*    IntMatrix;
typedef bool*    BoolMatrix;
typedef real*    RealMatrix;

typedef uint*    IntVector;
typedef bool*    BoolVector;
typedef real*    RealVector;

typedef RealMatrix  Matrix;
typedef RealVector  Vector;

typedef std::vector<real>  RealList;

#ifdef max
#undef max
#endif

#ifdef min
#undef min
#endif

/*
 * It is worth explicitly pointing out that IntMatrix/IntVector use uint.
 */
// STATIC AND USEFUL DEFINITIONS ============================================//
// as well as values needed for the toys that are not "inputs"
#define MAXIMUM_INT    numeric_limits<int>::max()
#define MINIMUM_INT    numeric_limits<int>::min()
#define MAXIMUM_REAL  numeric_limits<real>::min()
#define MINIMUM_REAL  numeric_limits<real>::min()
#define INFINITY_REAL  numeric_limits<real>::infinity()

// POINT TYPE ===============================================================//
class Point {
public:

  real x, y;
  Point(real x, real y): x(x), y(y) { }
  Point(): x(0.0), y(0.0) { }
  Point(const Point& other): x(other.x), y(other.y) {}
  
  /**
   * Calculates euclidean distance between two points.
   * @return the distance between p1 and p2
   */
  static inline real distance(const Point& p1, const Point& p2) {
    return sqrt((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y));
  }

};

typedef std::vector<Point>  PointList;
typedef Point*        PointVector;
 
// WEIGHTED POINT TYPE (FOR WINNOW) =========================================//
class WeightedPoint {
public:

  Point point;
  uint weight;
  
  WeightedPoint(Point point, uint weight): point(point), weight(weight) { }
  WeightedPoint(real x, real y, uint weight): point(x, y), weight(weight) { }  

  inline bool operator<(const WeightedPoint& rhs) const {
    return (weight < rhs.weight);
  }

};

typedef std::vector<WeightedPoint>  WeightedPointList;

// COWICHAN DEFINITIONS =====================================================//
// aka. "inputs" to the toys, and chaining functions.
class Cowichan {
protected:

  // constants
  static const char* CHAIN;
  static const char* MANDEL;
  static const char* RANDMAT;
  static const char* HALF;
  static const char* INVPERC;
  static const char* THRESH;
  static const char* LIFE;
  static const char* WINNOW;
  static const char* NORM;
  static const char* HULL;
  static const char* OUTER;
  static const char* GAUSS;
  static const char* SOR;
  static const char* PRODUCT;
  static const char* VECDIFF;

protected:

  // common
  int nr;
  int nc;
  int n;
  // game of life
  int lifeIterations;
  // mandelbrot
  real mandelX0, mandelY0, mandelDx, mandelDy;
  // threshold
  real threshPercent;
  // percolation
  int invpercNFill;
  // seed value for simple random number generator
  uint seed;

protected: // individual problems

  virtual void mandel(IntMatrix matrix) = 0;
  virtual void randmat(IntMatrix matrix) = 0;
  virtual void half(IntMatrix matrixIn, IntMatrix matrixOut) = 0;
  virtual void invperc(IntMatrix matrix, BoolMatrix mask) = 0;
  virtual void thresh(IntMatrix matrix, BoolMatrix mask) = 0;
  virtual void life(BoolMatrix matrixIn, BoolMatrix matrixOut) = 0;
  virtual void winnow(IntMatrix matrix, BoolMatrix mask, PointList** points) = 0;
  virtual void norm(PointList* pointsIn, PointList** pointsOut) = 0;
  virtual void hull(PointList* pointsIn, PointList** pointsOut) = 0;
  virtual void outer(PointList* points, Matrix* matrix, Vector* vector) = 0;
  virtual void gauss(Matrix matrix, Vector target, Vector* solution) = 0;
  virtual void sor(Matrix matrix, Vector target, Vector* solution) = 0;
  virtual void product(Matrix matrix, Vector actual, Vector* solution) = 0;
  virtual void vecdiff(Vector actual, Vector computed, real* e) = 0;

protected:

  /**
   * Runs the cowichan problem set, chained together.
   * @param use_randmat true: generate a random matrix.
   *                    false: use a window of the mandelbrot set.
   * @param use_thresh true: use image thresholding for int->bool.
   *                   false: use invasion percolation for int->bool.
   */
  virtual void chain(bool use_randmat, bool use_thresh) = 0;

protected:

  /**
   * DEBUGGING FUNCTION: show a matrix result
   */
  void printAxb(Matrix matrix, Vector answer, Vector vector);

public:

  /**
   * Runs cowichan problems based on command line input.
   * @param argc number of command line arguments.
   * @param argv command line arguments.
   * @param use_randmat true: generate a random matrix.
   *                    false: use a window of the mandelbrot set.
   * @param use_thresh true: use image thresholding for int->bool.
   *                   false: use invasion percolation for int->bool.
   */
  void main(int argc, char* argv[], bool use_randmat, bool use_thresh);

};

// UTILITY FUNCTIONS ========================================================//

#define MATRIX_RECT(mtrx,row,col)  (mtrx)[(row)*this->nc + col]
#define MATRIX_RECT_NC(mtrx,row,col,nc)  (mtrx)[(row)*(nc) + col]
#define MATRIX_SQUARE(mtrx,row,col)  (mtrx)[(row)*this->n + col]
#define MATRIX_SQUARE_N(mtrx,row,col,n)  (mtrx)[(row)*(n) + col]
#define MATRIX            MATRIX_SQUARE
#define VECTOR(vect,row)      (vect)[row]
#define DIAG(mtrx,v)        (mtrx)[v*this->n + v]

#define NEW_MATRIX_SQUARE(__type)  (new __type[this->n * this->n])
#define NEW_MATRIX_RECT(__type)    (new __type[this->nr * this->nc])
#define NEW_VECTOR_SZ(__type,__num)  (new __type[__num])
#define NEW_VECTOR(__type)      NEW_VECTOR_SZ(__type, this->n)

/**
 * Returns a pseudorandom number ~ U[mean - range, mean + range].
 */
real uniform(real mean, real range);

// DEBUGGING FUNCTIONS ======================================================//

/**
 * "pretty-print" a list of points.
 */
#define PRINT_BREAK 4
void print(PointList& points);

/**
 * Prints out of memory message and exits.
 */
void out_of_memory();

#endif

