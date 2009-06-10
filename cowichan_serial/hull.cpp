#include "cowichan_serial.hpp"

void split(PointVector pointsIn, int n, PointVector pointsOut, int* hn,
    Point minPoint, Point maxPoint);

void CowichanSerial::hull (PointVector pointsIn, PointVector pointsOut)
{
  Point minPoint;
  Point maxPoint;

  minPoint = pointsIn[0];
  maxPoint = pointsIn[0];

  // figure out the points with minimum and maximum x values
  int i;
  int hn = 0;
  for (i = 1; i < n; i++) {
    if (minPoint.x > pointsIn[i].x) {
      minPoint = pointsIn[i];
    }
    if (maxPoint.x < pointsIn[i].x) {
      maxPoint = pointsIn[i];
    }
  }

  // use these as initial pivots
  split (pointsIn, n, pointsOut, &hn, minPoint, maxPoint);
  split (pointsIn, n, pointsOut, &hn, maxPoint, minPoint);

  // set the new number of points for chained runs
  n = hn;
}

/**
 * Compute hull on one side of the splitting line.
 * @param pointsIn hull input points.
 * @param n number of input points.
 * @param pointsOut hull output points.
 * @param hn pointer to the number of output points.
 * @param p1 boundary point #1
 * @param p2 boundary point #2
 */
void split (PointVector pointsIn, int n, PointVector pointsOut, int* hn,
    Point p1, Point p2) {

  Point* maxPoint = NULL;
  real maxCross = -numeric_limits<real>::infinity ();

  // compute the signed distances from the line for each point
  for (int i = 0; i < n; i++) {
    real currentCross = Point::cross (p1, p2, pointsIn[i]);
    if (currentCross > maxCross) {
      maxPoint = &pointsIn[i];
      maxCross = currentCross;
    }
  }

  // is there a point in the positive half-space?
  // if so, it has maximal distance, and we must recurse based on that point.
  if (maxCross > 0.0) {
    // recurse on the new set with the given far point
    split (pointsIn, n, pointsOut, hn, p1, *maxPoint);
    split (pointsIn, n, pointsOut, hn, *maxPoint, p2);
    return;
  }

  // otherwise, it's not on the right side; we don't need to split anymore.
  // this is because all points are inside the hull when we use this half-space.
  // add the first point and return.
  pointsOut[(*hn)++] = p1;
}

