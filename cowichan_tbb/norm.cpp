#include <iostream>
#include <cstdlib>
#include <ctime>
#include <algorithm>
#include <limits>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

using std::numeric_limits;

typedef float real;

class Point {
public:

	real x, y;
	Point(real x, real y): x(x), y(y) { }
	Point(): x(0.0), y(0.0) { }
	
	static Point minimum;
	static Point maximum;

};

Point Point::minimum = Point(numeric_limits<real>::min(), numeric_limits<real>::min());
Point Point::maximum = Point(numeric_limits<real>::max(), numeric_limits<real>::max());

/*****************************************************************************/

const int SIZE = 100;

/**
 * This class performs point normalization -- points are put onto the unit
 * square. 
 */
class Normalizer {
public:
	
	Point (*points)[SIZE];

public:
	
	/**
	 * Performs the minimum and maximum computations.
	 */
	class MinMaxReducer {
	private:

		Normalizer* parent;
		Point _min, _max;

	public:

		/**
		 * Initialise min and max with max and min, respectively, so that we
		 * don't have to "special-case" the first iteration.
		 */
		MinMaxReducer(Normalizer* parent):
			parent(parent), _min(Point::maximum), _max(Point::minimum) { }
	
		Point getMinimum() const {
			return _min;
		}
	
		Point getMaximum() const {
			return _max;
		}
	
		/**
		 * Calculates the minimum and maximum co-ordinates over the given array
		 * range.
		 */
		void operator()(const blocked_range<size_t>& range) {

			// get pointers locally.
			Point (&points)[SIZE] = *(parent->points);
			Point min = _min;
			Point max = _max;

			// calculate the minimum and maximum co-ordinates over the range.
			for (size_t i = range.begin(); i != range.end(); ++i) {
				min.x = std::min(points[i].x, min.x);
				min.y = std::min(points[i].y, min.y);
				max.x = std::max(points[i].x, max.x);
				max.y = std::max(points[i].y, max.y);
			}
			
			// refresh member variables.
			_min = min;
			_max = max;
			
		}
	
		/**
		 * Splitting (TBB) constructor
		 */
		MinMaxReducer(MinMaxReducer& other, split):
			parent(other.parent), _min(Point::maximum), _max(Point::minimum) { }
	
		/**
		 * Joiner (TBB).
		 */
		void join(const MinMaxReducer& other) {
			_min.x = std::min(other._min.x, _min.x);
			_min.y = std::min(other._min.y, _min.y);
			_max.x = std::max(other._max.x, _max.x);
			_max.y = std::max(other._max.y, _max.y);
		}
		
	};

	/** 
	 * Performs the re-normalization procedure.
	 */
	class Computer {
	public:
		
		Normalizer* parent;
		Point min, max;

		Computer(Normalizer* parent, Point min, Point max):
			parent(parent), min(min), max(max) { }

		void operator()(const blocked_range<size_t>& range) const {
			
			// get pointers locally.
			Point (&points)[SIZE] = *(parent->points);

			// normalize the points that lie in the given range.
			real xfactor = 1.0 / (max.x - min.x);
			real yfactor = 1.0 / (max.y - min.y);
			for (size_t i = range.begin(); i != range.end(); ++i) {

				points[i].x = (points[i].x - min.x) * xfactor;
				points[i].y = (points[i].y - min.y) * yfactor;

			}
			
		}
		
	};
	
	MinMaxReducer minmax;
	Computer* computer;

public:

	Normalizer(Point (*points)[SIZE]):
		minmax(this), computer(NULL), points(points)
		{ }

	~Normalizer() {
		if (computer != NULL) {
			delete computer;
		}
	}
		
	/**
	 * In-place normalizes the incoming co-ordinates onto the unit square.
	 */
	static void perform(Point (*points)[SIZE]) {
		
		Normalizer norm(points);
				
		// first compute the statistics, and then use the min/max to compute the new points.
		norm.computeStats();
		parallel_for(blocked_range<size_t>(0, SIZE), (*norm.computer), auto_partitioner());
		
		// the points and transformed in-place, so don't return anything.
		return;
		
	}

private:

	/**
	 * Calls the stats-gathering reducer and creates a normalizer based on it.
	 */
	void computeStats() {
		
		parallel_reduce(blocked_range<size_t>(0, SIZE), minmax, auto_partitioner());
		computer = new Computer(this, minmax.getMinimum(), minmax.getMaximum());
		
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

	Point points[SIZE];
	
	// seed the random number generator.
	srand(time(0));
	
	// SERIAL: generate random values for the vector and matrix
	for (int i = 0; i < SIZE; ++i) {
		points[i] = Point(uniform(-50.0, 50.0), uniform(-50.0, 50.0));
	}
	
	// start up TBB
	task_scheduler_init init;

	// multiply the matrix by the vector (NB. answer == vector).
	Normalizer::perform(&points);
	
}

