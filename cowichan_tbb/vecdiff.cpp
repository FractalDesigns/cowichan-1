#include "cowichan_tbb.hpp"

/**
 * This class takes the 1-norm of the difference between two vectors.
 */
class NormDiff {

  Vector _actual;
  Vector _computed;
  real normDiff;

public:
  
  real getNormDiff() const {
    return normDiff;
  }
  
  NormDiff(Vector actual, Vector computed) : _actual(actual),
      _computed(computed), normDiff(0.0) { }

  void operator()(const Range& range) {
    Vector actual = _actual;
    Vector computed = _computed;

    for (index_t i = range.begin(); i != range.end(); ++i) {
    
      // get the element-wise difference; store the maximum
      real diff = (real)fabs((double)(actual[i] - computed[i]));

      if (diff > normDiff) {
        normDiff = diff;
      }
      
    }
  }
  
  /**
   * Splitting (TBB) constructor
   */
  NormDiff(NormDiff& other, split):
    _actual(other._actual), _computed(other._computed), normDiff(0.0) { }
  
  /**
   * Joiner (TBB).
   */
  void join(const NormDiff& other) {
    if (normDiff < other.normDiff) {
      normDiff = other.normDiff;
    }
  }

};

/*****************************************************************************/

real CowichanTBB::vecdiff (Vector actual, Vector computed)
{
  NormDiff norm(actual, computed);
  parallel_reduce(Range(0, n), norm, auto_partitioner());

  return norm.getNormDiff();
}

