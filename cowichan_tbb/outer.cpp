/**
  This module turns a vector containing point positions into a dense, symmet-
  ric, diagonally dominant matrix by calculating the distances between each
  pair of points. It also constructs a real vector whose values are the distance
  of each point from the origin. Inputs are:

  points: a vector of (x, y) points, where x and y are the point’s position.
  nelts: the number of points in the vector, and the size of the matrix along
       each axis.

  Its outputs are:

  matrix: a real matrix, whose values are filled with inter-point distances.
  vector: a real vector, whose values are filled with origin-to-point distances.

  Each matrix element Mi,j such that i != j is given the value di,j, the Eu-
  clidean distance between point i and point j. The diagonal values Mi,i are
  then set to nelts times the maximum off-diagonal value to ensure that the
  matrix is diagonally dominant. The value of the vector element vi is set to
  the distance of point i from the origin, which is given by sqrt(xi^2 + yi^2).
 */
#include "cowichan_tbb.hpp"
 
class PointDistances {
  
  PointVector _points;
  Matrix _matrix;
  Vector _vector;
  index_t n;
  real _max;
  
public:

  PointDistances(PointVector points, Matrix matrix, Vector vector, index_t n)
      : _points(points), _matrix(matrix), _vector(vector), n(n), _max(-1) { }

  real getMaximum() const {
    return _max;
  }

  /**
   * Calculates inter-point distances on the given range.
   */
  void operator()(const Range& rows) {
    
    PointVector points = _points;
    Matrix matrix = _matrix;
    Vector vector = _vector;
    
    for (index_t y = rows.begin(); y != rows.end(); ++y) {
      
      // compute distances from points to origin
      VECTOR(vector, y) = Point::distance(points[y], Point(0.0, 0.0));
    
      // compute distances between points
      for (index_t x = 0; x < y; ++x) {
        real d = Point::distance(points[x], points[y]);
        if (d > _max) {
          _max = d;
        }
        MATRIX(matrix, y, x) = MATRIX(matrix, x, y) = d;
      }
    }
  }
  
  /**
   * Splitting (TBB) constructor
   */
  PointDistances(PointDistances& other, split) : _points(other._points),
      _matrix(other._matrix), _vector(other._vector), n(other.n), _max(-1) { }

  /**
   * Joiner (TBB).
   */
  void join(const PointDistances& other) {
    if (_max < other._max) {
      _max = other._max;
    }
  }
  
};


/**
 * Makes a given matrix diagonally dominant by modifying its diagonal elements.
 */
class MakeDominant {

  Matrix _matrix;
  index_t n;
  const real value;
  
public:

  MakeDominant(Matrix matrix, index_t n, real value):
    _matrix(matrix), n(n), value(value) { }
  
  /**
   * Sets diagonal elements to a given constant.
   */  
  void operator()(const Range& rows) const {
    Matrix matrix = _matrix;
    
    for (index_t i = rows.begin(); i != rows.end(); ++i) {
      DIAG(matrix, i) = value;
    }
  }
  
};

/*****************************************************************************/

void CowichanTBB::outer(PointVector points, Matrix matrix, Vector vector) {
  
  // figure out the matrix and vector
  PointDistances dist(points, matrix, vector, n);
  parallel_reduce(Range(0, n), dist, auto_partitioner());

  // fix up the diagonal
  MakeDominant dom(matrix, n, dist.getMaximum() * n);
  parallel_for(Range(0, n), dom, auto_partitioner());
  
}

