#include <iostream>
#include <cstdlib>
#include <ctime>
#include <algorithm>
#include <vector>
#include <limits>

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
 * Computes the convex hull of a set of points, returning a set of points in
 * counter-clockwise order. The method used is known as parallel quickhull, and
 * it is described at:
 * 
 * http://www.cs.cmu.edu/~scandal/cacm/node10.html
 *
 * The code used in this program is based on the ideas presented there.
 */

/**
 * Computes the cross product of the vectors (l1,l2) and (l1,p).
 */
inline real cross(const Point& l1, const Point& l2, const Point& p) {
	return (l1.x - p.x) * (l2.y - p.y) -
		   (l1.y - p.y) * (l2.x - p.x);
}

/**
 * Splits the recursion space to find members of the convex hull.
 */
void split(PointList& hull, const PointList& points, const Point p1, const Point p2) {

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

/**
 * Calculate the convex hull of points using the QuickHull method.
 * @return PointList whose members are the points of the convex hull.
 */
PointList convexHull(PointList& points) {
	
	PointList hull;
	PointList::const_iterator minPoint = points.begin();
	PointList::const_iterator maxPoint = points.begin();	
	
	// figure out the points with minimum and maximum x values
	for (PointList::const_iterator it = points.begin(); it != points.end(); ++it) {
		
		if (minPoint->x > it->x) minPoint = it;
		if (maxPoint->x < it->x) maxPoint = it;
		
	}
	
	// use these as initial pivots
	split(hull, points, *minPoint, *maxPoint);
	split(hull, points, *maxPoint, *minPoint);
	
	// return the points that we figured out
	return hull;
	
}

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
	
	// compute the convex hull of the points.
	PointList hull = convexHull(points);
	
	// now print out the points, and the hull.
//	std::cout << "Points: "; print(points); std::cout << std::endl;
//	std::cout << "Hull:   "; print(hull); std::cout << std::endl;

	return 0;

}

