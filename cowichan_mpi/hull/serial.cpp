/**
 * Serial implementation of convex hull
 *
 * \file serial.cpp
 * \author Andrew Borzenko
 * \date 03-06-09
 */

#include "../include/main.h"
#include "serial.h"

// public

void hull (pt1D* points,     // list of points
           int n,            // number of points
           pt1D* hullPoints, // list of points in convex hull
           int* hn)          // number of points in convex hull
{
  int i;
  pt min_p;
  pt max_p;

  min_p = points[0];
  max_p = points[0];

  // figure out the points with minimum and maximum x values
  for (i = 1; i < n; i++) {
    if (min_p.x > points[i].x) {
      min_p = points[i];
    }
    if (max_p.x < points[i].x) {
      maxPoint = points[i];
    }
  }

  // use these as initial pivots
  split (points, n, hullPoints, hn, min_p, max_p);
  split (points, n, hullPoints, hn, max_p, min_p);
}

// private

void split (pt1D* points,    // list of points
           int n,            // number of points
           pt1D* hullPoints, // list of points in convex hull
           int* hn,          // number of points in convex hull
           pt min_p,         // point with minimum x
           pt max_p)         // point with maximum x
{
  const Point* maxPoint = NULL;
  real maxCross = -numeric_limits<real>::infinity();
	
	// compute the signed distances from the line for each point
	for (int i = 0; i < points.size(); ++i) {
		real currentCross = cross(p1, p2, points[i]);
		if (currentCross > maxCross) {
			maxPoint = &(points[i]);
			maxCross = currentCross;
		}
	}
	
	// is there a point in the positive half-space?
	// if so, it has maximal distance, and we must recurse based on that point.
	if (maxCross > 0.0) {
		
		// for recusion, remove all points inside of the triangle
		// defined by (points[p1], points[p2], points[maxIndex])
//		PointList newPoints;
		
		// FIXME this hurts performance by a factor of 20.
/*		for (PointList::iterator it = points.begin(); it != points.end();) {
			if (cross(p1, p2, *it) > 0 &&
				cross(p2, *maxPoint, *it) > 0 &&
				cross(*maxPoint, p1, *it) > 0)
			{
				// point in triangle; remove it.
				it = points.erase(it);
			} else {
				// continue on to next point
				it++;
			}
		}  */
				
		// recurse on the new set with the given far point
		split(hull, points, p1, *maxPoint);
		split(hull, points, *maxPoint, p2);
		return;
		
	} 
	
	// otherwise, it's not on the right side; we don't need to split anymore.
	// this is because all points are inside the hull when we use this half-space.
	// add the first point and return.
	hull.push_back(p1);
}
