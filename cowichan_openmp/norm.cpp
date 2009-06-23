#include "cowichan_openmp.hpp"

void findMinMax(PointVector points, INT64 n, Point* minPoint, Point* maxPoint);

void CowichanOpenMP::norm (PointVector pointsIn, PointVector pointsOut)
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
#pragma omp parallel for schedule(static)
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

  PointVector minPoints = NULL;
  PointVector maxPoints = NULL;
  INT64 num_threads = omp_get_max_threads();

  try {
    minPoints = NEW_VECTOR_SZ(Point, num_threads);
    maxPoints = NEW_VECTOR_SZ(Point, num_threads);
  }
  catch (...) {out_of_memory();}

#pragma omp parallel
  {
    INT64 thread_num = omp_get_thread_num();
    minPoints[thread_num].x = points[0].x;
    minPoints[thread_num].y = points[0].y;
    maxPoints[thread_num].x = points[0].x;
    maxPoints[thread_num].y = points[0].y;
#pragma omp for schedule(static)
    for (INT64 i = 1; i < n; i++) {
      if (points[i].x < minPoints[thread_num].x) {
        minPoints[thread_num].x = points[i].x;
      }
      if (points[i].y < minPoints[thread_num].y) {
        minPoints[thread_num].y = points[i].y;
      }
      if (points[i].x > maxPoints[thread_num].x) {
        maxPoints[thread_num].x = points[i].x;
      }
      if (points[i].y > maxPoints[thread_num].y) {
        maxPoints[thread_num].y = points[i].y;
      }
    }
  }

  minPoint->x = minPoints[0].x;
  minPoint->y = minPoints[0].y;
  maxPoint->x = maxPoints[0].x;
  maxPoint->y = maxPoints[0].y;

  for (INT64 i = 0; i < num_threads; i++) {
    if (minPoint->x > minPoints[i].x) {
      minPoint->x = minPoints[i].x;
    }
    if (minPoint->y > minPoints[i].y) {
      minPoint->y = minPoints[i].y;
    }
    if (maxPoint->x < maxPoints[i].y) {
      maxPoint->x = maxPoints[i].x;
    }
    if (maxPoint->y < maxPoints[i].y) {
      maxPoint->y = maxPoints[i].y;
    }
  }

  delete [] minPoints;
  delete [] maxPoints;

}

