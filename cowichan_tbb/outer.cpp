#include <iostream>
#include <cstdlib>
#include <ctime>
#include <algorithm>
#include <cmath>
#include <vector>
#include <limits>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range2d.h"
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
	static Point origin;

	/**
	 * Calculates euclidean distance between two points.
	 * @return the distance between p1 and p2
	 */
	static inline real distance(const Point& p1, const Point& p2) {
		return sqrt((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y));
	}

};

Point Point::minimum = Point(numeric_limits<real>::min(), numeric_limits<real>::min());
Point Point::maximum = Point(numeric_limits<real>::max(), numeric_limits<real>::max());
Point Point::origin  = Point(0.0, 0.0);

/*****************************************************************************/

const int NELTS = 5000;

typedef real				Vector[NELTS];
typedef real				Matrix[NELTS][NELTS];
typedef std::vector<Point>	PointList;

/*****************************************************************************/

/**
	This module turns a vector containing point positions into a dense, symmet-
	ric, diagonally dominant matrix by calculating the distances between each
	pair of points. It also constructs a real vector whose values are the distance
	of each point from the origin. Inputs are:

	points: a vector of (x, y) points, where x and y are the point’s position.
	nelts: the number of points in the vector, and the size of the matrix along
		   each axis.

	Its outputs are:

	matrix: a real matrix, whose values are filled with inter-point distances.
	vector: a real vector, whose values are filled with origin-to-point distances.

	Each matrix element Mi,j such that i != j is given the value di,j, the Eu-
	clidean distance between point i and point j. The diagonal values Mi,i are
	then set to nelts times the maximum off-diagonal value to ensure that the
	matrix is diagonally dominant. The value of the vector element vi is set to
	the distance of point i from the origin, which is given by sqrt(xi^2 + yi^2).
 */
 
class PointDistances {
	
	Matrix *_matrix;
	Vector *_vector;
	PointList *_points;
	
public:

	static void perform(PointList& points, Matrix* matrix, Vector* vector) {
		PointDistances dist(&points, matrix, vector);
		parallel_for(blocked_range<size_t>(0, NELTS),
			dist, auto_partitioner());
	}
	
public:

	PointDistances(PointList *points, Matrix* matrix, Vector* vector):
		_points(points), _matrix(matrix), _vector(vector) { }

	/**
	 * Calculates inter-point distances on the given range.
	 */
	void operator()(const blocked_range<size_t>& rows) const {
		
		PointList& points = *_points;
		Matrix& matrix = *_matrix;
		Vector& vector = *_vector;
		
		for (size_t y = rows.begin(); y != rows.end(); ++y) {
			
			// compute distances from points to origin
			vector[y] = Point::distance(points[y], Point::origin);
		
			// compute distances between points,
			for (size_t x = 0; x < NELTS; ++x) {
				if (x != y) {
					// and only for the non-diagonal elements.
					matrix[y][x] = Point::distance(points[x], points[y]);
				} else {
					matrix[y][x] = 0.0;
				}
			}			
		}
	}
	
};


/**
 * Performs the maximum computation.
 */
class MaxReducer {
private:

	Matrix* _image;
	real _max;
	
public:

	static real perform(Matrix& matrix) {
		MaxReducer reducer(&matrix);
		parallel_reduce(blocked_range2d<size_t,size_t>(0, NELTS, 0, NELTS),
			reducer, auto_partitioner());
		return reducer.getMaximum();
	}
	
public:

	/**
	 * Initialise max with the lowest possible value.
	 */
	MaxReducer(Matrix* image):
		_image(image), _max(numeric_limits<real>::min()) { }

	real getMaximum() const {
		return _max;
	}

	/**
	 * Calculates the maximum value over the given range.
	 */
	void operator()(const blocked_range2d<size_t,size_t>& range) {

		Matrix& image = *_image;
		real max = _max;
		
		const blocked_range<size_t>& rows = range.rows();
		const blocked_range<size_t>& cols = range.cols();
		
		for (size_t y = rows.begin(); y != rows.end(); ++y) {
			for (size_t x = cols.begin(); x != cols.end(); ++x) {
				max = std::max(max, image[y][x]);
			}
		}
		
		_max = max;
		
	}
	
	/**
	 * Splitting (TBB) constructor
	 */
	MaxReducer(MaxReducer& other, split):
		_image(other._image), _max(numeric_limits<real>::min()) { }

	/**
	 * Joiner (TBB).
	 */
	void join(const MaxReducer& other) {
		_max = std::max(other._max, _max);
	}
	
};

/**
 * Makes a given matrix diagonally dominant by modifying its diagonal elements.
 */
class MakeDominant {

	Matrix* _matrix;
	const real value;
	
public:

	static void perform(Matrix* matrix, real value) {
		MakeDominant dom(matrix, value);
		parallel_for(blocked_range<size_t>(0, NELTS), dom, auto_partitioner());
	}

public:

	MakeDominant(Matrix* matrix, real value):
		_matrix(matrix), value(value) { }
	
	/**
	 * Sets diagonal elements to a given constant.
	 */	
	void operator()(const blocked_range<size_t>& rows) const {
		Matrix& matrix = *_matrix;
		
		for (size_t i = rows.begin(); i != rows.end(); ++i) {
			matrix[i][i] = value;
		}
	}
	
};

/*****************************************************************************/

real uniform(real mean, real range) {
	return (rand() / (real)RAND_MAX) * (2.0f * range) - range + mean;
}

int main(int argc, char** argv) {

	PointList points;
	Vector vector;
	Matrix matrix;
	
	// seed the random number generator.
	srand(time(0));
	
	// SERIAL: generate random values for the vector and matrix
	for (int i = 0; i < NELTS; ++i) {
		points.push_back(Point(uniform(-50.0, 50.0), uniform(-50.0, 50.0)));
	}
	
	// start up TBB
	task_scheduler_init init;
	
	// create the matrix and vector; fix up the diagonal.
	PointDistances::perform(points, &matrix, &vector);
	real maxValue = MaxReducer::perform(matrix);
	MakeDominant::perform(&matrix, maxValue);
	
	return 0;
	
}

