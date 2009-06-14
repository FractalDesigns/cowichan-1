#include "cowichan_serial.hpp"

void findMinMax(PointVector points, INT64 n, Point* minPoint, Point* maxPoint);

void CowichanSerial::norm (PointVector pointsIn, PointVector pointsOut)
{
  Point minPoint, maxPoint;
  real sclX, sclY; // scaling factors
  INT64 i;

  // compute scaling factors
  findMinMax(pointsIn, n, &minPoint, &maxPoint);

  sclX = (real)((maxPoint.x == minPoint.x) ?
      0.0 : 1 / (maxPoint.x - minPoint.x));
  sclY = (real)((maxPoint.y == minPoint.y) ?
      0.0 : 1 / (maxPoint.y - minPoint.y));

  // scale
  for (i = 0; i < n; i++) {
    pointsOut[i].x = sclX * (pointsIn[i].x - minPoint.x);
    pointsOut[i].y = sclY * (pointsIn[i].y - minPoint.y);
  }

}

/**
 * Finds min/max x/y coordinates.
 * @param points point vector.
 * @param n number of points.
 * @param minPoint min x/y values.
 * @param maxPoint max x/y values.
 */
void findMinMax(PointVector points, INT64 n, Point* minPoint,
    Point* maxPoint) {

  minPoint->x = points[0].x;
  minPoint->y = points[0].y;
  maxPoint->x = points[0].x;
  maxPoint->y = points[0].y;

  for (INT64 i = 1; i < n; i++) {
    minPoint->x = std::min (points[i].x, minPoint->x);
    minPoint->y = std::min (points[i].y, minPoint->y);
    maxPoint->x = std::max (points[i].x, maxPoint->x);
    maxPoint->y = std::max (points[i].y, maxPoint->y);
  }

}

