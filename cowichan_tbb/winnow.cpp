/*
  This module converts a matrix of integer values to a vector of points, rep-
  resented as x and y coordinates. Its inputs are:

  matrix: an integer matrix, whose values are used as masses.
  mask: a Boolean matrix showing which points are eligible for consideration.
  nr, nc: the number of rows and columns in the matrix.
  n: the number of points to select.

  Its output is:

  points: a vector of (x, y) points.

  Each location where mask is true becomes a candidate point, with a weight
  equal to the integer value in matrix at that location and x and y coordinates
  equal to its row and column indices. These candidate points are then sorted
  into increasing order by weight, and nelts evenly-spaced points selected to
  create the result vector.
*/
#include "cowichan_tbb.hpp"

class ValueCount {
private:

  IntMatrix _candidates;
  BoolMatrix _mask;
  index_t nc;
  index_t count;

public:

  /**
   * Return value count.
   */
  index_t getCount()
  {
    return count;
  }

  /**
   * Standard constructor
   */
  ValueCount(IntMatrix candidates, BoolMatrix mask, index_t nc):
    _candidates(candidates), _mask(mask), nc(nc), count(0) { }

  /**
   * Add candidate values to the value list based on mask (TBB).
   */
  void operator()(const Range2D& range) {
    
    // bring pointers into cache
    const BoolMatrix mask = _mask;
    const IntMatrix candidates = _candidates;
    const Range& rows = range.rows();
    const Range& cols = range.cols();

    // count values marked as good by the mask.
    for (index_t y = rows.begin(); y != rows.end(); ++y) {
      for (index_t x = cols.begin(); x != cols.end(); ++x) {
        
        if (MATRIX_RECT(mask, y, x)) {
          count++;
        }
        
      }
    }
    
  }
  
  /**
   * Splitting (TBB) constructor
   */
  ValueCount(ValueCount& other, split) : _candidates(other._candidates),
      _mask(other._mask), nc(other.nc), count(0) { }

  /**
   * Joiner (TBB).
   */
  void join(const ValueCount& other) {
    count += other.count;
  }
  
};

/*****************************************************************************/

void CowichanTBB::winnow(IntMatrix matrix, BoolMatrix mask,
    PointVector points) {

  // count candidates in the matrix
  ValueCount vc(matrix, mask, nc);
  parallel_reduce(Range2D(0, nr, 1000, 0, nc, 1000), vc);

  index_t len = vc.getCount();

  WeightedPointVector weightedPoints = NULL;
  try {
    weightedPoints = NEW_VECTOR_SZ(WeightedPoint, len);
  }
  catch (...) {out_of_memory();}

  index_t i = 0;

  // fill in weighted points
  for (index_t y = 0; y != nr; ++y) {
    for (index_t x = 0; x != nc; ++x) {
      if (MATRIX_RECT(mask, y, x)) {
        weightedPoints[i++] = WeightedPoint((real)x, (real)y,
            MATRIX_RECT(matrix, y, x));
      }
    }
  }

#ifdef SORT_TIME
  INT64 start, end;
  start = get_ticks ();
#endif

  // sort the extracted points
  parallel_sort(weightedPoints, &weightedPoints[len]);

#ifdef SORT_TIME
  end = get_ticks ();
#endif

  index_t stride, j;

  // copy over points
  stride = len / n;

  for (i = n - 1, j = len - 1; i >= 0; i--, j -= stride) {
#ifdef WINNOW_OUTPUT
    std::cout << weightedPoints[j].weight << "\n";
#endif
    points[i] = weightedPoints[j].point;
  }
  
#ifdef SORT_TIME
  std::cout << "winnow sort: ";
  print_elapsed_time(start, end);
  std::cout << std::endl;
#endif

}

