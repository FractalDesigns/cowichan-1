#include "cowichan_tbb.hpp"

/**
 * Performs the minimum and maximum computations.
 */
class MinMaxReducer {
private:

  PointVector pointsIn; // input points
  Point minPoint, maxPoint; // min/max points

public:

  MinMaxReducer(PointVector pointsIn) : pointsIn(pointsIn) { }

  Point getMinimum() const {
    return minPoint;
  }

  Point getMaximum() const {
    return maxPoint;
  }

  /**
   * Calculates the minimum and maximum co-ordinates over the given array
   * range.
   */
  void operator()(const Range& range) {

    // get pointers locally.
    Point min = pointsIn[0];
    Point max = pointsIn[0];

    // calculate the minimum and maximum co-ordinates over the range.
    for (index_t i = range.begin(); i != range.end(); ++i) {
      if (pointsIn[i].x < min.x) {
        min.x = pointsIn[i].x;
      }
      if (pointsIn[i].y < min.y) {
        min.y = pointsIn[i].y;
      }
      if (pointsIn[i].x > max.x) {
        max.x = pointsIn[i].x;
      }
      if (pointsIn[i].y > max.y) {
        max.y = pointsIn[i].y;
      }
    }
    
    // refresh member variables.
    minPoint = min;
    maxPoint = max;
    
  }

  /**
   * Splitting (TBB) constructor
   */
  MinMaxReducer(MinMaxReducer& other, split)
      : pointsIn(other.pointsIn) { }

  /**
   * Joiner (TBB).
   */
  void join(const MinMaxReducer& other) {
    if (other.minPoint.x < minPoint.x) {
      minPoint.x = other.minPoint.x;
    }
    if (other.minPoint.y < minPoint.y) {
      minPoint.y = other.minPoint.y;
    }
    if (other.maxPoint.x > maxPoint.x) {
      maxPoint.x = other.maxPoint.x;
    }
    if (other.maxPoint.y > maxPoint.y) {
      maxPoint.y = other.maxPoint.y;
    }
  }
  
};


/**
 * This class performs point normalization -- points are put onto the unit
 * square. 
 */
class Normalizer {
public:
  
  PointVector pointsIn; // input points
  PointVector pointsOut; // output points
  real minX, minY; // minimum x/y coordinates
  real xfactor, yfactor; // scaling factors

public:

  Normalizer(PointVector pointsIn, PointVector pointsOut, real minX, real minY,
      real xfactor, real yfactor) : pointsIn(pointsIn), pointsOut(pointsOut),
      minX(minX), minY(minY), xfactor(xfactor), yfactor(yfactor) { }

  void operator()(const Range& range) const {

    // normalize the points that lie in the given range.
    for (index_t i = range.begin(); i != range.end(); ++i) {

      pointsOut[i].x = xfactor * (pointsIn[i].x - minX);
      pointsOut[i].y = yfactor * (pointsIn[i].y - minY);

    }
    
  }
  
};

/*****************************************************************************/

void CowichanTBB::norm(PointVector pointsIn, PointVector pointsOut) {

  MinMaxReducer minmax(pointsIn);

  // find min/max coordinates
  parallel_reduce(Range(0, n), minmax, auto_partitioner());

  Point minPoint = minmax.getMinimum();
  Point maxPoint = minmax.getMaximum();

  // compute scaling factors
  real xfactor = (real)((maxPoint.x == minPoint.x) ?
      0.0 : 1.0 / (maxPoint.x - minPoint.x));
  real yfactor = (real)((maxPoint.y == minPoint.y) ?
      0.0 : 1.0 / (maxPoint.y - minPoint.y));

  Normalizer normalizer(pointsIn, pointsOut, minPoint.x, minPoint.y, xfactor,
      yfactor);

  // normalize the vector
  parallel_for(Range(0, n), normalizer, auto_partitioner());

}

