#include <iostream>
#include <cstdlib>
#include <ctime>
#include <algorithm>
#include <vector>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range2d.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

/*
	This module converts a matrix of integer values to a vector of points, rep-
	resented as x and y coordinates. Its inputs are:

	matrix: an integer matrix, whose values are used as masses.
	mask: a Boolean matrix showing which points are eligible for consideration.
	nrows, ncols: the number of rows and columns in the matrix.
	nelts: the number of points to select.

	Its output is:

	points: a vector of (x, y) points.

	Each location where mask is true becomes a candidate point, with a weight
	equal to the integer value in matrix at that location and x and y coordinates
	equal to its row and column indices. These candidate points are then sorted
	into increasing order by weight, and nelts evenly-spaced points selected to
	create the result vector.
*/

#define NELTS 100
#define NROWS 50
#define NCOLS 50

class Point;

typedef float				real;

typedef std::vector<Point>	PointList;
typedef std::vector<real>	RealList;

typedef bool				BoolMatrix[NROWS][NCOLS];
typedef real				RealMatrix[NROWS][NCOLS];

class Point {
public:

	real x, y;
	Point(real x, real y): x(x), y(y) { }
	Point(): x(0.0), y(0.0) { }
	
};

/*****************************************************************************/

class ValueSelector {
private:

	RealMatrix* _candidates;
	BoolMatrix* _mask;
	
	RealList values;

public:

	/**
	 * Standard constructor
	 */
	ValueSelector(RealMatrix* candidates, BoolMatrix* mask):
		_candidates(candidates), _mask(mask) { }

	/**
	 * Add candidate values to the value list based on mask (TBB).
	 */
	void operator()(const blocked_range2d<size_t,size_t>& range) {
		
		// bring pointers into cache
		const BoolMatrix& mask = *_mask;
		const RealMatrix& candidates = *_candidates;
		const blocked_range<size_t>& rows = range.rows();
		const blocked_range<size_t>& cols = range.cols();
		
		// add candidate values marked as good by the mask.
		for (size_t y = rows.begin(); y != rows.end(); ++y) {
			for (size_t x = cols.begin(); x != cols.end(); ++x) {
				
				if (mask[y][x]) add(candidates[y][x]);
				
			}
		}
		
	}
	
	/**
	 * Splitting (TBB) constructor.
	 */
	ValueSelector(ValueSelector& other, split):
		_candidates(other._candidates), _mask(other._mask) { }
	
	/**
	 * Joiner (TBB).
	 */
	void join(const ValueSelector& other) {
		add(other.values);
	}
	
	/**
	 * Gets the list of selected values.
	 */
	RealList getValues() const {
		return values;
	}
		
private:

	/**
	 * Inserts a value into the values list, in correct order (given that the
	 * list is already sorted).
	 */
	void add(const real& newValue) {
		RealList::iterator pos = std::lower_bound(values.begin(), values.end(), newValue);
		values.insert(pos, newValue);
	}
	
	/**
	 * Merges another already-sorted list with this ValueSelector's list.
	 */
	void add(const RealList& other) {

		RealList replacement;
		RealList::const_iterator i, j;		

		// reserve space in the replacement vector for the current and new values		
		replacement.reserve(values.size() + other.size());

		// iterate over both collections, adding the smaller element to the new
		// real list every time we must make a comparison. In the end, all of
		// the elements are added to replacement.
		for (i = values.begin(), j = other.begin();;) {
		
			if (i == values.end() && j == other.end()) return;
			if (i == values.end()) {
				replacement.push_back(*j); ++j;
			} else if (j == values.end()) {
				replacement.push_back(*i); ++i;
			} else {
				if (*i < *j) {
					replacement.push_back(*i); ++i;
				} else {
					replacement.push_back(*j); ++j;
				}
			}
		
		}
		
		// replace the old list with the new one.
		values = replacement;
		
	}
	
};

/*****************************************************************************/

class PointCreator {
private:

	RealList* values;
	PointList points;

public:

	/**
	 * Perform weighted point selection.
	 * @return a list of numPoints points.
	 */
	static PointList perform(int numPoints, RealMatrix* candidates, BoolMatrix* mask) {
	
		// extract candidates from the matrix
		ValueSelector vc(candidates, mask);
		parallel_reduce(blocked_range2d<size_t,size_t>(0, NROWS, 0, NCOLS),
			vc, auto_partitioner());
		
		// we can only create as many points as we have pairs of values.
		if (numPoints < vc.getValues().size()) {
			numPoints = vc.getValues().size();
		}
		
		// pair them together to create points
		PointCreator pc(vc.getValues());
		parallel_reduce(blocked_range<size_t>(0, numPoints),
			pc, auto_partitioner());
			
		// return those points
		return pc.getPoints();
		
	}

public:

	/**
	 * Standard constructor.
	 */
	PointCreator(RealList sv) {
		values = new RealList(sv);
	}

	/**
	 * Combine values pairwise to create a list of points (TBB).
	 */
	void operator()(const blocked_range<size_t>& range) {

		const RealList& v = *values;
		for (size_t i = range.begin(); i != range.end(); ++i) {
			points.push_back(Point(v[i*2], v[(i*2)+1]));
		}

	}

	/**
	 * Splitting (TBB) constructor.
	 */
	PointCreator(PointCreator& other, split):
		values(other.values) { }
	
	/**
	 * Joiner (TBB).
	 */	
	void join(const PointCreator& other) {
		points.insert(points.end(), other.points.begin(), other.points.end());
	}
	
	/**
	 * Gets the list of points.
	 */
	PointList getPoints() const {
		return points;
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
 * Entry point of the program.
 */
int main(int argc, char** argv) {

	RealMatrix candidates;
	BoolMatrix mask;
	
	// seed the random number generator.
	srand(time(0));
	
	// SERIAL: generate random values for the matrices.
	for (int row = 0; row < NROWS; ++row) {
		for (int col = 0; col < NCOLS; ++col) {
			candidates[row][col] = uniform(0.0, 100.0);
			mask[row][col] = (uniform(0.0, 100.0) > 50.0); // 25% will be true
		}
	}
	
	// start up TBB
	task_scheduler_init init;

	// get a list of points!
	PointList points = PointCreator::perform(NELTS, &candidates, &mask);
	
}

