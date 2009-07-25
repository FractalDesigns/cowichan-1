#include "cowichan_tbb.hpp"

/**
 * Performs one iteration of relaxation.
 */
class Relaxer {
private:

  Matrix _matrix;
  Vector _target;
  Vector _solution;
  index_t n;
  real maxDiff;

public:

  Relaxer(Matrix matrix, Vector target, Vector solution, index_t n)
      : _matrix(matrix), _target(target), _solution(solution), n(n) { }

  /**
   * Get maximum difference.
   */
  real getMaxDiff() const {
    return maxDiff;
  }

  /**
   * Performs one iteration of relaxation.
   */
  void operator()(const Range& range) {

    // get pointers locally.
    Matrix matrix = _matrix;
    Vector target = _target;
    Vector solution = _solution;

    index_t c;
    real diff;
    real sum;
    real oldSolution;

    maxDiff = 0.0;

    for (index_t r = range.begin(); r != range.end(); r++) {
      // compute sum
      sum = 0.0;
      for (c = 0; c < r; c++) {
        sum += MATRIX_SQUARE(matrix, r, c) * solution[c];
      }
      for (c = r + 1; c < n; c++) {
        sum += MATRIX_SQUARE(matrix, r, c) * solution[c];
      }
    
      // calculate new solution
      oldSolution = solution[r];
      solution[r] = (real)((1.0 - SOR_OMEGA) * oldSolution + SOR_OMEGA *
          (target[r] - sum) / MATRIX_SQUARE(matrix, r, r));

      // compute difference
      diff = (real)fabs((double)(oldSolution - solution[r]));
      if (diff > maxDiff){
        maxDiff = diff;
      }
    }
    
  }

  /**
   * Splitting (TBB) constructor
   */
  Relaxer(Relaxer& other, split) : _matrix(other._matrix),
      _target(other._target), _solution(other._solution), n(other.n) { }

  /**
   * Joiner (TBB).
   */
  void join(const Relaxer& other) {
    if (maxDiff < other.maxDiff) {
      maxDiff = other.maxDiff;
    }
  }
  
};

/*****************************************************************************/

void CowichanTBB::sor(Matrix matrix, Vector target, Vector solution)
{
  index_t r, t;
  real maxDiff;

  // initialize
  for (r = 0; r < n; r++) {
    solution[r] = 1.0;
  }
  maxDiff = (real)(2 * SOR_TOLERANCE); // to forestall early exit

  Relaxer relaxer(matrix, target, solution, n);

  for (t = 0; (t < SOR_MAX_ITERS) && (maxDiff >= SOR_TOLERANCE); t++) {
    parallel_reduce(Range(0, n), relaxer, auto_partitioner()); 
    maxDiff = relaxer.getMaxDiff();
  }
}

