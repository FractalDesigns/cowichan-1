#include "cowichan_tbb.hpp"

/**
 * This class does a halving shuffle.
 */
class Shuffle {
private:

  index_t xBreak, yBreak;
  index_t nr, nc;

public:
  
  IntMatrix _first, _second;

  Shuffle(IntMatrix input, IntMatrix output, index_t nr, index_t nc):
      _first(input), _second(output),
      nr(nr), nc(nc),
      xBreak((nc + 1) / 2),
      yBreak((nr + 1) / 2) {}

  /**
   * Performs the halving shuffle over the given range.
   */
  void operator()(const Range2D& range) const {
    
    IntMatrix first = _first;
    IntMatrix second = _second;
    const Range& rows = range.rows();
    const Range& cols = range.cols();
    
    index_t xSrc, ySrc;
    for (index_t y = rows.begin(); y != rows.end(); ++y) {
      for (index_t x = cols.begin(); x != cols.end(); ++x) {
        
        // calculate unswapped x co-ordinate.
        if (x < xBreak) {
          // odd columns
          xSrc = x * 2;
        } else {
          // even columns
          xSrc = (x - xBreak) * 2 + 1;
        }
        
        // calculate unswapped y co-ordinate.
        if (y < yBreak) {
          // odd rows
          ySrc = y * 2;
        } else {
          // even columns
          ySrc = (y - yBreak) * 2 + 1;
        }
              
        // assign new values in the output matrix.
        MATRIX_RECT(second, y, x) = MATRIX_RECT(first, ySrc, xSrc);
        
      }
    }
    
  }
};

/*****************************************************************************/

void CowichanTBB::half(IntMatrix matrixIn, IntMatrix matrixOut) {

  // perform the halving shuffle.
  Shuffle shuffle(matrixIn, matrixOut, nr, nc);
  parallel_for(Range2D(0, nr, 0, nc), shuffle,
      auto_partitioner());
    
}

