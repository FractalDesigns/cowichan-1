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
#include <climits>
#include <limits>
#include <string>
#include <cstring>
using std::numeric_limits;

// TIMING ===================================================================//

/**
 * Enables printing of test time.
 */
#define TEST_TIME

/**
 * Enables printing of sort time (in winnow).
 */
#define SORT_TIME

#if defined(WIN64) || defined(WIN32)   // Windows
  #include <windows.h>
#else                // Linux
  #include <sys/times.h>
  #include <stdint.h>
  #include <string.h>
  typedef uint64_t UINT64;
  typedef uint32_t UINT32;
  typedef int64_t INT64;
  typedef int32_t INT32;
#endif

INT64 get_ticks ();
INT64 get_freq ();
void print_elapsed_time (INT64 start, INT64 end);

/**
 * Does a sort of swap-out, printing progress.
 */
void timeInfo(INT64 *start, INT64 *end, std::string message);

// DEBUGGING FUNCTIONS ======================================================//

/**
 * Enables printing of output data.
 */
//#define OUTPUT_DATA

/**
 * Indicates that winnow weights should be printed.
 */
//#define WINNOW_OUTPUT

/**
 * Prints out of memory message and exits.
 */
void out_of_memory();

/**
 * Prints not enough points message and exits.
 */
void not_enough_points();

// BASIC TYPES ==============================================================//
#ifndef REAL_TYPE
  // use IEEE single floating-point by default
  #define REAL_TYPE float
#endif
typedef REAL_TYPE real;

typedef UINT32 INT_TYPE;

typedef INT_TYPE* IntMatrix;
typedef bool* BoolMatrix;
typedef real* RealMatrix;

typedef INT_TYPE* IntVector;
typedef bool* BoolVector;
typedef real* RealVector;

typedef RealMatrix Matrix;
typedef RealVector Vector;

#ifdef max
#undef max
#endif

#ifdef min
#undef min
#endif

// defined index type to be signed
typedef ptrdiff_t index_t;

/*
 * It is worth explicitly pointing out that IntMatrix/IntVector use INT_TYPE.
 */
// STATIC AND USEFUL DEFINITIONS ============================================//
// as well as values needed for the toys that are not "inputs"
#define MAXIMUM_INT numeric_limits<INT_TYPE>::max()
#define MINIMUM_INT numeric_limits<INT_TYPE>::min()
#define MAXIMUM_REAL numeric_limits<real>::min()
#define MINIMUM_REAL numeric_limits<real>::min()
#define INFINITY_REAL numeric_limits<real>::infinity()

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

  /**
   * Computes the cross product of the vectors (l1,l2) and (l1,p).
   */
  static inline real cross (const Point& l1, const Point& l2, const Point& p) {
    return (l1.x - p.x) * (l2.y - p.y) - (l1.y - p.y) * (l2.x - p.x);
  }
};

typedef Point* PointVector;
 
// WEIGHTED POINT TYPE (FOR WINNOW) =========================================//
class WeightedPoint {
public:

  Point point;
  INT_TYPE weight;
  
  WeightedPoint(Point point, INT_TYPE weight): point(point), weight(weight) { }
  WeightedPoint(): point(0.0, 0.0), weight(0) { }
  WeightedPoint(real x, real y, INT_TYPE weight): point(x, y), weight(weight) { }  

  inline bool operator<(const WeightedPoint& rhs) const {
    return (weight < rhs.weight);
  }

  inline bool operator<=(const WeightedPoint& rhs) const {
    return (weight <= rhs.weight);
  }

};

typedef WeightedPoint* WeightedPointVector;

// UTILITY FUNCTIONS ========================================================//

#define MATRIX_RECT(mtrx,row,col)  (mtrx)[(row)*this->nc + col]
#define MATRIX_RECT_NC(mtrx,row,col,nc)  (mtrx)[(row)*(nc) + col]
#define MATRIX_SQUARE(mtrx,row,col)  (mtrx)[(row)*this->n + col]
#define MATRIX_SQUARE_N(mtrx,row,col,n)  (mtrx)[(row)*(n) + col]
#define MATRIX MATRIX_SQUARE
#define VECTOR(vect,row) (vect)[row]
#define DIAG(mtrx,v) (mtrx)[v*this->n + v]

#define NEW_MATRIX_SQUARE(__type) (new __type[this->n * this->n])
#define NEW_MATRIX_RECT(__type) (new __type[this->nr * this->nc])
#define NEW_VECTOR_SZ(__type,__num) (new __type[__num])
#define NEW_VECTOR(__type) NEW_VECTOR_SZ(__type, this->n)

/**
 * Returns a pseudorandom number ~ U[mean - range, mean + range].
 */
real uniform(real mean, real range);

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
  index_t nr;
  index_t nc;
  index_t n;
  // game of life
  index_t lifeIterations;
  // mandelbrot
  real mandelX0, mandelY0, mandelDx, mandelDy;
  // threshold
  real threshPercent;
  // percolation
  index_t invpercNFill;
  // seed value for simple random number generator
  INT_TYPE seed;

protected: // individual problems

  virtual void mandel(IntMatrix matrix) = 0;
  virtual void randmat(IntMatrix matrix) = 0;
  virtual void half(IntMatrix matrixIn, IntMatrix matrixOut) = 0;
  virtual void invperc(IntMatrix matrix, BoolMatrix mask) = 0;
  virtual void thresh(IntMatrix matrix, BoolMatrix mask) = 0;
  virtual void life(BoolMatrix matrixIn, BoolMatrix matrixOut) = 0;
  virtual void winnow(IntMatrix matrix, BoolMatrix mask, PointVector points) = 0;
  virtual void norm(PointVector pointsIn, PointVector pointsOut) = 0;
  virtual void hull(PointVector pointsIn, PointVector pointsOut) = 0;
  virtual void outer(PointVector points, Matrix matrix, Vector vector) = 0;
  virtual void gauss(Matrix matrix, Vector target, Vector solution) = 0;
  virtual void sor(Matrix matrix, Vector target, Vector solution) = 0;
  virtual void product(Matrix matrix, Vector candidate, Vector solution) = 0;
  virtual real vecdiff(Vector actual, Vector computed) = 0;

private:

  /**
   * Runs the cowichan problem set, chained together.
   * @param use_randmat true: generate a random matrix.
   *                    false: use a window of the mandelbrot set.
   * @param use_thresh true: use image thresholding for int->bool.
   *                   false: use invasion percolation for int->bool.
   */
  void chain(bool use_randmat, bool use_thresh);

public:

  /**
   * DEBUGGING FUNCTION: Print a rectangular matrix.
   */
#ifdef OUTPUT_DATA
  template <typename T>
  void print_rect_matrix(T* matrix)
  {
    index_t r, c;

    for (r = 0; r < nr; r++) {
      for (c = 0; c < nc; c++) {
        std::cout << MATRIX_RECT(matrix, r, c) << "\t";
      }
      std::cout << "\n";
    }
    std::cout << "\n";
  }
#else
  template <typename T>
  void print_rect_matrix(T* /* matrix */) { }
#endif


  /**
   * DEBUGGING FUNCTION: Print a square matrix.
   */
#ifdef OUTPUT_DATA
  template <typename T>
  void print_square_matrix(T* matrix)
  {
    index_t r, c;

    for (r = 0; r < n; r++) {
      for (c = 0; c < n; c++) {
        std::cout << MATRIX_SQUARE(matrix, r, c) << "\t";
      }
      std::cout << "\n";
    }
    std::cout << "\n";
  }
#else
  template <typename T>
  void print_square_matrix(T* /* matrix */) { }
#endif

  /**
   * DEBUGGING FUNCTION: Print a vector.
   */
#ifdef OUTPUT_DATA
  template <typename T>
  void print_vector(T* vector)
  {
    index_t r;

    for (r = 0; r < n; r++) {
      std::cout << VECTOR(vector, r) << "\n";
    }
    std::cout << "\n";
  }
#else
  template <typename T>
  void print_vector(T* /* vector */) { }
#endif

  /**
   * DEBUGGING FUNCTION: Print a point vector.
   */
  void print_vector(PointVector points);

public:

  /**
   * Runs cowichan problems based on command line input.
   * @param argc number of command line arguments.
   * @param argv command line arguments.
   * @param use_randmat true: generate a random matrix.
   *                    false: use a window of the mandelbrot set.
   * @param use_thresh  true: use image thresholding for int->bool.
   *                    false: use invasion percolation for int->bool.
   */
  void main(int argc, char* argv[], bool use_randmat, bool use_thresh);

};

#endif

