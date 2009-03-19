#include <iostream>
#include <cstdlib>
#include <ctime>
#include <algorithm>
#include <vector>
#include <limits>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

using std::numeric_limits;

class Point;

typedef float				real;
typedef std::vector<Point>	PointList;

class Point {
public:

	real x, y;
	Point(real x, real y): x(x), y(y) { }
	Point(): x(0.0), y(0.0) { }
	Point(const Point& other): x(other.x), y(other.y) {}
	
};

/**
 * Computes the maximum signed distance from a point to a line, given
 * the line and list of points.
 */
class MaximumDistance {
public:

	/**
	 * Computes the cross product of the vectors (l1,l2) and (l1,p).
	 */
	static inline real cross(const Point& l1, const Point& l2, const Point& p) {
		return (l1.x - p.x) * (l2.y - p.y) -
			   (l1.y - p.y) * (l2.x - p.x);
	}

private:

	const PointList& points;
	const Point &p1, &p2;
	Point maxPoint;
	real maxCross;

public:

	MaximumDistance(const PointList& points, const Point& p1, const Point& p2):
		points(points), p1(p1), p2(p2), maxCross(-numeric_limits<real>::infinity())
		{ }

	/**
	 * Gets the point with maximum signed distance (if it has already been
	 * calculated).
	 */	
	Point getPoint() const {
		return maxPoint;
	}
	
	/**
	 * Gets the signed distance to that point.
	 */
	real getDistance() const {
		return maxCross;
	}
	
	/**
	 * Calculates the Point with maximum signed distance.
	 */
	void operator()(const blocked_range<size_t>& range) {

		// compute the signed distances from the line for each point in the range.
		for (int i = range.begin(); i < range.end(); ++i) {
			real currentCross = cross(p1, p2, points[i]);
			if (currentCross > maxCross) {
				maxPoint = points[i];
				maxCross = currentCross;
			}
		}
		
	}

	/**
	 * Splitting (TBB) constructor
	 */
	MaximumDistance(MaximumDistance& other, split):
		points(other.points), maxCross(other.maxCross), p1(other.p1), p2(other.p2)
		{ }

	/**
	 * Joiner (TBB).
	 */
	void join(const MaximumDistance& other) {
		if (other.maxCross > maxCross) {
			maxPoint = other.maxPoint;
			maxCross = other.maxCross;
		}
	}

};

/**
 * Computes the convex hull of a set of points, returning a set of points in
 * counter-clockwise order. The method used is known as parallel quickhull, and
 * it is described at:
 * 
 * http://www.cs.cmu.edu/~scandal/cacm/node10.html
 * http://www.cs.princeton.edu/~ah/alg_anim/version1/QuickHull.html
 *
 * The code used in this program is based on the ideas presented there.
 */
class QuickHull {
private:

	PointList hull;
	const PointList& points;

public:

	QuickHull(const PointList& points):
		points(points)
		{ }

	/**
	 * Splits the recursion space to find members of the convex hull.
	 */
	void split(const PointList& points, const Point p1, const Point p2) {

		// use TBB to find the point with maximal signed distance from the line (p1,p2)
		MaximumDistance md(points, p1, p2);
		parallel_reduce(blocked_range<size_t>(0, points.size()), md, auto_partitioner());

		// is there a point in the positive half-space?
		// if so, it has maximal distance, and we must recurse based on that point.
		if (md.getDistance() > 0.0) {
				
			// recurse on the new set with the given far point
			split(points, p1, md.getPoint());
			split(points, md.getPoint(), p2);
			return;
		
		} 
		
		// otherwise, it's not on the right side; we don't need to split anymore.
		// this is because all points are inside the hull when we use this half-space.
		// add the first point and return.
		hull.push_back(p1);
	
	}

	/**
	 * Calculate the convex hull of points using the QuickHull method.
	 * @return PointList whose members are the points of the convex hull.
	 */
	void convexHull() {
	
		PointList::const_iterator minPoint = points.begin();
		PointList::const_iterator maxPoint = points.begin();	
	
		// figure out the points with minimum and maximum x values
		for (PointList::const_iterator it = points.begin(); it != points.end(); ++it) {
		
			if (minPoint->x > it->x) minPoint = it;
			if (maxPoint->x < it->x) maxPoint = it;
		
		}
	
		// use these as initial pivots
		split(points, *minPoint, *maxPoint);
		split(points, *maxPoint, *minPoint);
	
	}

public:

	/**
	 * Computes and returns the convex hull for the given set of points (TBB).
	 */
	static PointList perform(const PointList& points) {
		QuickHull qh(points);
		qh.convexHull();
		return qh.hull;
	}

};

/*****************************************************************************/

/**
 * Returns a pseudorandom number ~ U[mean - range, mean + range].
 */
real uniform(real mean, real range) {
	return (rand() / (real)RAND_MAX) * (2.0f * range) - range + mean;
}

/**
 * "pretty-print" a list of points.
 */
#define PRINT_BREAK 4
void print(PointList& points) {
	int b = 0;
	for (PointList::const_iterator it = points.begin(); it != points.end(); ++it) {
		if (b == 0) std::cout << std::endl << "\t";
		std::cout << "(" << it->x << "," << it->y << ")\t";
		b = (b + 1) % PRINT_BREAK;
	}
}

/**
 * The entry point of the program.
 */
int main(int argc, char** argv) {
	
	// seed the random number generator.
	srand(time(0));
	
	// create a set of points in [-100,100]x[-100,100].
	PointList points;
	for (int i = 0; i < 50000; ++i) {
		points.push_back(Point(
			uniform(0.0, 100.0),
			uniform(0.0, 100.0)
		));
	}
	
	// start up TBB
	task_scheduler_init init;
	
	// compute the convex hull of the points.
	PointList hull = QuickHull::perform(points);
	
	// now print out the points, and the hull.
//	std::cout << "Points: "; print(points); std::cout << std::endl;
//	std::cout << "Hull:   "; print(hull); std::cout << std::endl;

	return 0;

}

