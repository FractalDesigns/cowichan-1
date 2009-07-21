#include "cowichan_tbb.hpp"

/** 
 * Performs row elimination, i.e. eliminate i-th column in j-th row.
 */
class RowElimination {
private:

  Matrix _matrix;
  Vector _target;
  index_t n;
  index_t i;
  real column_i;

public:

  RowElimination(Matrix matrix, Vector target, index_t n) : _matrix(matrix),
      _target(target), n(n) { };

  void setI(index_t i) {
    this->i = i;
    column_i = MATRIX(_matrix, i, i);
  }

  void operator()(const Range& rows) const {
    
    // Get pointers locally.
    Matrix matrix = _matrix;
    Vector target = _target;
    
    for (index_t j = rows.begin(); j != rows.end(); ++j) {
      real factor = -(MATRIX(matrix, j, i) / column_i);
      for (index_t k = n - 1; k >= i; k--) {
        MATRIX(matrix, j, k) += MATRIX(matrix, i, k) * factor;
      }
      target[j] += target[i] * factor;
    }
  }
  
};

/**
 * Matrices are required to be symmetric and diagonally dominant in order to
 * guarantee that there is a well-formed solution to the equation.
 */
void CowichanTBB::gauss (Matrix matrix, Vector target, Vector solution)
{
  index_t i, j, k;

  RowElimination rowElimination(matrix, target, n);

  // forward elimination
  for (i = 0; i < n; i++) {
    // get row with maximum column i
    index_t max = i;
    for (j = i + 1; j < n; j++) {
      if (fabs(MATRIX(matrix, j, i)) > fabs(MATRIX(matrix, max, i))) {
        max = j;
      }
    }

    real tmp;
    // swap max row with row i
    for (j = i; j < n; j++) {
      tmp = MATRIX(matrix, i, j);
      MATRIX(matrix, i, j) = MATRIX(matrix, max, j);
      MATRIX(matrix, max, j) = tmp;
    }
    tmp = target[i];
    target[i] = target[max];
    target[max] = tmp;

    // eliminate i-th column in rows (i + 1, n)
    rowElimination.setI(i);
    parallel_for(Range(i + 1, n), rowElimination, auto_partitioner());
  }

  // back substitution
  for (k = (n - 1); k >= 0; k--) {
    solution[k] = target[k] / MATRIX_SQUARE(matrix, k, k);
    for (i = k - 1; i >= 0; i--) {
      target[i] = target[i] - (MATRIX_SQUARE(matrix, i, k) * solution[k]);
    }
  }
}

