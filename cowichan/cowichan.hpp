/**
 * \mainpage Parallel Programming and Cowichan Problems
 *
 * \section intro_sec Introduction
 *
 * The Cowichan problems are implemented by various parallel programming
 * systems. Serial C++ implementation is available for comparison.
 * The Cowichan class is the base class for all C++ implementations.
 *
 * \section problems_sec The Problems
 *
 * Currently, there are 14 Cowichan problems. They are described below.
 * Cowichan class. Problems can be run separately by
 * passing the problem name as an argument on the command line. The inputs to
 * the problems are defined in cowichan_defaults.hpp. There is currently no way
 * to override the inputs without recompiling.
 *
 * TODO: add links to problems in Cowichan class.
 *
 * \subsection mandel_sec 1. mandel: Mandelbrot Set Generation
 * This module generates the Mandelbrot Set for a specified region of the
 * complex plane.
 *
 * \subsection randmat_sec 2. randmat: Random Number Generation
 * This module fills a matrix with pseudo-random integers.
 * Note that, as in all problems, the output is required to be independent of
 * the number of processors used.
 *
 * \subsection half_sec 3. half: Two-Dimensional Shuffle
 * This module divides the values in a rectangular two-dimensional integer
 * matrix into two halves along one axis, shuffles them, and then repeats this
 * operation along the other axis. Values in odd-numbered locations are
 * collected at the low end of each row or column, while values in
 * even-numbered locations are moved to the high end.
 *
 * \subsection invperc_sec 4. invperc: Invasion Percolation
 * Invasion percolation models the displacement of one fluid (such as oil) by
 * another (such as water) in fractured rock. In two dimensions, this can be
 * simulated by generating an NxN grid of random numbers in the range
 * [1. . .R], and then marking the center cell of the grid as filled. In each
 * iteration, one examines the four orthogonal neighbors of all filled cells,
 * chooses the one with the lowest value (i.e. the one with the least
 * resistance to filling), and fills it in. The simulation continues until some
 * fixed percentage of cells have been filled, or until some other condition
 * (such as the presence of trapped regions) is achieved. The fractal structure
 * of the filled and unfilled regions is then examined to determine how much
 * oil could be recovered. The naive way to implement this is to repeatedly
 * scan the array; a more sophisticated, and much faster, sequential technique
 * is to maintain a priority queue of unfilled cells which are neighbors of
 * filled cells. This latter technique is similar to the list-based methods
 * used in some cellular automaton programs, and is very difficult to
 * parallelize effectively. Filling begins at the central cell of the matrix
 * (rounding down for even-sized axes).
 *
 * \subsection thresh_sec 5. thresh: Histogram Thresholding
 * This module performs histogram thresholding on an image. Given an integer
 * image I and a target percentage p, it constructs a binary image B such that
 * B[i,j] is set if no more than p percent of the pixels in I are brighter than
 * I[i,j]. The general idea is that an image's histogram should have 2 peaks,
 * one centered around the average foreground intensity, and one centered
 * around the average background intensity. This program attempts to set a
 * threshold between the two peaks in the histogram and select the pixels above
 * the threshold.
 *
 * \subsection life_sec 6. life: Game of Life
 * This module simulates the evolution of Conway's Game of Life, a
 * two-dimensional cellular automaton.
 * At each time step, this module must count the number of live (true)
 * neighbors of each cell, using both orthogonal and diagonal connectivity and
 * circular boundary conditions. The update rule is simple: if a cell has 3
 * live neighbors, or has 2 live neighbors and is already alive, it is alive in
 * the next generation. In any other situation, the cell becomes, or stays,
 * dead.
 *
 * \subsection winnow_sec 7. winnow: Weighted Point Selection
 * This module converts a matrix of integer values to a vector of points,
 * represented as x and y coordinates.
 * Each location where mask is true becomes a candidate point, with a weight
 * equal to the integer value in matrix at that location and x and y
 * coordinates equal to its row and column indices. These candidate points
 * are then \b sorted into increasing order by weight, and n evenly-spaced 
 * points selected to create the result vector.
 *
 * \subsection norm_sec 8. norm: Point Location Normalization
 * This module normalizes point coordinates so that all points lie within
 * the unit square [0. . .1]x[0. . .1].
 *
 * \subsection hull_sec 9. hull: Convex Hull
 * This module takes a list of two-dimensional points and reorders them by
 * doing multiple convex hull computations. Convex hull is the boundary of the
 * minimal convex set containing a given non-empty finite set of points in the
 * plane. In other words, all points not in the convex hull are enclosed in the
 * convex hull polygon. At each step the convex hull points are taken out of
 * the input list and are put into the output list. The computation terminates
 * when there are no more points left in the input list.
 *
 * \subsection outer_sec 10. outer: Outer Product
 * This module turns a vector containing point positions into a dense,
 * symmetric, diagonally dominant matrix by calculating the distances between
 * each pair of points. It also constructs a real vector whose values are the
 * distance of each point from the origin.
 *
 * \subsection gauss_sec 11. gauss: Gaussian Elimination
 * This module solves a matrix equation AX = V for a dense, symmetric,
 * diagonally dominant matrix A and an arbitrary vector non-zero V using
 * explicit reduction (matrices are required to be symmetric and diagonally
 * dominant in order to guarantee that there is a well-formed solution to the
 * equation).
 *
 * \subsection sor_sec 12. sor: Successive Over-Relaxation
 * This module solves a matrix equation AX = V for a dense, symmetric,
 * diagonally dominant matrix A and an arbitrary vector non-zero V using
 * successive over-relaxation.
 *
 * \subsection product_sec 13. product: Matrix-Vector Product
 * This module calculates V in AX = V where A is a matrix and X is a vector.
 *
 * \subsection vecdiff_sec 14. vecdiff: 1-Norm Vector Difference
 * This module calculates the 1-norm of the difference between two vectors. In
 * case the vectors are actual and computed values of some calculation the
 * result represents the magnitude of the error.
 */

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

