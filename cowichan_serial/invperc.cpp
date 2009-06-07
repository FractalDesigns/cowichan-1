#include "cowichan_serial.hpp"
#include <queue>

class PercPoint {
public:

  Point point;

public:

  PercPoint(Point point): point(point) { }

  // we want to extract lowest values.
  bool operator<(const PercPoint &other) const {
    return value() > other.value();
  }

  uint value() const {
    return MATRIX_RECT(matrix, (int)point.y, (int)point.x);
  }

  static IntMatrix matrix;
  static int nc;

};

IntMatrix PercPoint::matrix = NULL;
int PercPoint::nc = 0;

/*****************************************************************************/

void CowichanSerial::invperc(IntMatrix matrix, BoolMatrix mask) {
  
  PercPoint pp(Point(0, 0));
  
  // fill mask with false.
  for (int i = 0; i < nr * nc; i++) {
    VECTOR(mask, i) = false;
  }
  
  // set the matrix we are working with and number of columns
	PercPoint::matrix = matrix;
  PercPoint::nc = nc;

  // "seed" with the middle value; start a priority queue.
  std::vector<PercPoint> points;

  points.push_back(Point((real) (nr / 2), (real) (nc / 2))); 
  std::make_heap(points.begin(), points.end());

  // perform invasion percolation nfill times.
  int r, c;
  for (int it = 0; it < invpercNFill; ++it) {

    // get the highest-priority point that hasn't already
    // been filled.
    do {
      std::pop_heap(points.begin(), points.end());
      pp = points.back();
      points.pop_back();
      r = (int)pp.point.y;
      c = (int)pp.point.x;
    } while (MATRIX_RECT(mask, r, c)); // find a free one

    // fill it.
    MATRIX_RECT(mask, r, c) = true;

    // add all of its neighbours to the party...
    
    // top neighbour
    if (r > 0) {
      points.push_back(Point((real)c, (real)r - 1));
      push_heap(points.begin(), points.end());
    }
    
    // bottom neighbour
    if (r < (nr - 1)) {
      points.push_back(Point((real)c, (real)r + 1));
      push_heap(points.begin(), points.end());
    }
    
    // left neighbour
    if (c > 0) {
      points.push_back(Point((real)c - 1, (real)r));
      push_heap(points.begin(), points.end());
    }
    
    // right neighbour
    if (c < (nc - 1)) {
      points.push_back(Point((real)c + 1, (real)r));
      push_heap(points.begin(), points.end());
    }
    
  }
  
}

