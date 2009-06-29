#include "cowichan_openmp.hpp"

void quickhull(PointVector pointsIn, INT64 n, PointVector pointsOut,
    INT64* hn);

void split(PointVector pointsIn, INT64 n, PointVector pointsOut, INT64* hn,
    Point* p1, Point* p2);

/**
 * Runs quickhull algorithm until all points have been used up from the
 * original vector. At each step the hull points are marked as used and a new
 * convex hull is computed on the rest of points.
 * The points that have been used up are in the range (n - hn, n), i.e. at
 * the end of pointsIn vector.
 * NOTE: pointsIn vector gets modified by the algorithm.
 */
void CowichanOpenMP::hull (PointVector pointsIn, PointVector pointsOut)
{
  INT64 hn = 0;
  INT64 previous_hn = 0;

  // while not all points are used up then run quickhull on the rest of points
  while (n != hn) {
    // exclude added points from pointsIn by swapping them with points from the
    // end of pointsIn vector in range (0, n - nused)
    INT64 added_i;
#pragma omp parallel for schedule(static)
    for (added_i = previous_hn; added_i < hn; added_i++) {
      // search for the added point
      for (INT64 i = 0; i < n - previous_hn; i++) {
        if ((pointsIn[i].x == pointsOut[added_i].x)
            && (pointsIn[i].y == pointsOut[added_i].y)) {
          Point tmp = pointsIn[i];
          pointsIn[i] = pointsIn[n - added_i - 1];
          pointsIn[n - added_i - 1] = tmp;
          break;
        }
      }
    }
    
    previous_hn = hn;
    quickhull (pointsIn, n - hn, pointsOut, &hn);
  }
}

void quickhull(PointVector pointsIn, INT64 n, PointVector pointsOut,
    INT64* hn)
{
  // base case
  if (n == 1) {
    pointsOut[(*hn)++] = pointsIn[0];
    return;
  }

  Point* minPoint;
  Point* maxPoint;

  // checking cutoff value here prevents allocating unnecessary memory
  // for the reduction
  if(n > CowichanOpenMP::HULL_CUTOFF) {
    INT64 num_threads = omp_get_max_threads();

    Point** minPoints = NULL;
    Point** maxPoints = NULL;

    try {
      minPoints = NEW_VECTOR_SZ(Point*, num_threads);
      maxPoints = NEW_VECTOR_SZ(Point*, num_threads);
    }
    catch (...) {out_of_memory();}

    // figure out the points with minimum and maximum x values
  #pragma omp parallel
    {
      INT64 thread_num = omp_get_thread_num();
      minPoints[thread_num] = &pointsIn[0];
      maxPoints[thread_num] = &pointsIn[0];
      INT64 i;
  #pragma omp for schedule(static)
      for (i = 1; i < n; i++) {
        if (minPoints[thread_num]->x > pointsIn[i].x) {
          minPoints[thread_num] = &pointsIn[i];
        }
        if (maxPoints[thread_num]->x < pointsIn[i].x) {
          maxPoints[thread_num] = &pointsIn[i];
        }
      }
    }

    minPoint = minPoints[0];
    maxPoint = maxPoints[0];

    for (INT64 i = 1; i < num_threads; i++) {
      if (minPoint->x > minPoints[i]->x) {
        minPoint = minPoints[i];
      }
      if (maxPoint->x < maxPoints[i]->x) {
        maxPoint = maxPoints[i];
      }
    }

    delete [] minPoints;
    delete [] maxPoints;
  }
  else {
    minPoint = &pointsIn[0];
    maxPoint = &pointsIn[0];

    // figure out the points with minimum and maximum x values
    INT64 i;
    for (i = 1; i < n; i++) {
      if (minPoint->x > pointsIn[i].x) {
        minPoint = &pointsIn[i];
      }
      if (maxPoint->x < pointsIn[i].x) {
        maxPoint = &pointsIn[i];
      }
    }
  }

  // use these as initial pivots
  split (pointsIn, n, pointsOut, hn, minPoint, maxPoint);
  split (pointsIn, n, pointsOut, hn, maxPoint, minPoint);
}

/**
 * Compute hull on one side of the splitting line.
 * @param pointsIn hull input points.
 * @param n number of input points.
 * @param pointsOut hull output points.
 * @param hn pointer to the number of output points.
 * @param p1 boundary point #1.
 * @param p2 boundary point #2.
 */
void split (PointVector pointsIn, INT64 n, PointVector pointsOut, INT64* hn,
    Point* p1, Point* p2) {

  Point* maxPoint;
  real maxCross;

  // checking cutoff value here prevents allocating unnecessary memory
  // for the reduction
  if (n > CowichanOpenMP::HULL_CUTOFF) {
    INT64 num_threads = omp_get_max_threads();

    Point** maxPoints = NULL;
    Vector maxCrosses = NULL;

    try {
      maxPoints = NEW_VECTOR_SZ(Point*, num_threads);
      maxCrosses = NEW_VECTOR_SZ(real, num_threads);
    }
    catch (...) {out_of_memory();}

    // compute the signed distances from the line for each point
#pragma omp parallel
    {
      INT64 thread_num = omp_get_thread_num();
      maxPoints[thread_num] = &pointsIn[0];
      maxCrosses[thread_num] = Point::cross (*p1, *p2, pointsIn[0]);
#pragma omp for schedule(static)
      for (INT64 i = 1; i < n; i++) {
        real currentCross = Point::cross (*p1, *p2, pointsIn[i]);
        if (currentCross > maxCrosses[thread_num]) {
          maxPoints[thread_num] = &pointsIn[i];
          maxCrosses[thread_num] = currentCross;
        }
      }
    }

    maxPoint = maxPoints[0];
    maxCross = maxCrosses[0];

    for (INT64 i = 0; i < num_threads; i++) {
      if (maxCross < maxCrosses[i]) {
        maxPoint = maxPoints[i];
        maxCross = maxCrosses[i];
      }
    }

    delete [] maxPoints;
    delete [] maxCrosses;
  }
  else
  {
    maxPoint = &pointsIn[0];
    maxCross = Point::cross (*p1, *p2, pointsIn[0]);

    // compute the signed distances from the line for each point
    for (INT64 i = 1; i < n; i++) {
      real currentCross = Point::cross (*p1, *p2, pointsIn[i]);
      if (currentCross > maxCross) {
        maxPoint = &pointsIn[i];
        maxCross = currentCross;
      }
    }
  }

  // is there a point in the positive half-space?
  // if so, it has maximal distance, and we must recurse based on that point.
  if (maxCross > 0.0) {
    // recurse on the new set with the given far point
    split (pointsIn, n, pointsOut, hn, p1, maxPoint);
    split (pointsIn, n, pointsOut, hn, maxPoint, p2);
    return;
  }

  // otherwise, it's not on the right side; we don't need to split anymore.
  // this is because all points are inside the hull when we use this half-space.
  // add the first point and return.
  pointsOut[(*hn)++] = *p1;

}
