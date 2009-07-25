#include "cowichan_tbb.hpp"

/**
 * This class multiplies a matrix by a vector, producing a vector, a la:
 *  [x1 x2 x3]   [y1]   [(x1*y1 + x2*y2 + x3*y3)]
 *  [x4 x5 x6] * [y2] = [(x4*y1 + x5*y2 + x6*y3)]
 *  [x7 x8 x9]   [y3]   [(x7*y1 + x8*y2 + x9*y3)]
 */
class Product {
  
  Matrix _matrix;
  Vector _vector, _result;
  index_t n;

public:

  Product(Matrix matrix, Vector vector, Vector result, index_t n):
    _matrix(matrix), _vector(vector), _result(result), n(n) { }

  /**
   * Performs matrix-vector multiplication on the given row range.
   */
  void operator()(const Range& rows) const {
    
    Matrix matrix = _matrix;
    Vector vector = _vector;
    Vector result = _result;
    
    for (index_t row = rows.begin(); row != rows.end(); ++row) {
      
      VECTOR(result, row) = 0.0;
      for (index_t col = 0; col < n; ++col) {
        VECTOR(result, row) += MATRIX(matrix, row,col) * VECTOR(vector, col);
      }
      
    }
    
  }
};

/*****************************************************************************/

void CowichanTBB::product(Matrix matrix, Vector candidate, Vector solution)
{
  Product product(matrix, candidate, solution, n);
  parallel_for(Range(0, n), product, auto_partitioner());
}

