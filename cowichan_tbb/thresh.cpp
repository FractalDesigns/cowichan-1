#include <iostream>
#include <cstdlib>
#include <ctime>
#include <limits>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range2d.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

const size_t NROWS = 10000;
const size_t NCOLS = 10000;

typedef float	real;
typedef int		IntMatrix[SIZE][SIZE];
typedef bool	BoolMatrix[SIZE][SIZE];

#define MINIMUM_INT numeric_limits<int>::min()

/**
 * Performs the maximum computation.
 */
class MaxReducer {
private:

	IntMatrix* _image;
	int _max;

public:

	/**
	 * Initialise max with the lowest possible value.
	 */
	MaxReducer(IntMatrix* image):
		_image(image), _max(MINIMUM_INT) { }

	Point getMaximum() const {
		return _max;
	}

	/**
	 * Calculates the maximum value over the given range.
	 */
	void operator()(const blocked_range2d<size_t,size_t>& range) {

		IntMatrix& image = *_image;
		int max = _max;
		
		const blocked_range<size_t>& rows = range.rows();
		const blocked_range<size_t>& cols = range.cols();
		
		for (size_t y = rows.begin(); y != rows.end(); ++y) {
			for (size_t x = cols.begin(); x != cols.end(); ++x) {
				if (image[y][x] > max) max = image[y][x];
			}
		}
		
		_max = max;
		
	}
	
	/**
	 * Splitting (TBB) constructor
	 */
	MaxReducer(MaxReducer& other, split):
		_image(other._image), _max(MINIMUM_INT) { }

	/**
	 * Joiner (TBB).
	 */
	void join(const MaxReducer& other) {
		_max = std::max(other._max, _max);
	}
	
};

/*
 * This class calculates the histogram of a matrix.
 */
class Histogram {
	
	IntMatrix* _image;
	int* histogram;
	const int bins;
	
public:

	static int calculate(Matrix* image, real cutoff) {
		
		// get the maximum value in the matrix (need 0-that number of bins)
		MaxReducer reducer(image);
		parallel_reduce(
			blocked_range2d<size_t,size_t>(0, NROWS, 0, NCOLS),
			reducer, auto_partitioner());
		int max = reducer.getMaximum();
		
		// compute the histogram to get a thresholding value
		Histogram ranker(hist, max);
		parallel_for(
			blocked_range2d<size_t,size_t>(0, NROWS, 0, NCOLS),
			hist, auto_partitioner());
		
		// return that value
		return hist.getValue(cutoff);
		
	}
	
public:

	Histogram(Matrix* image, int maxValue): _image(image), maxValue(maxValue) {
		histogram = new int[maxValue + 1]{0};
	}

	int getValue(real cutoff) const {
		int i;
		int retain = (NROWS * NCOLS) * cutoff;
		for (i = maxValue; (i >= 0) && (retain > 0); --i) {
			retain -= (*histogram)[i];
		}
		return i;
	}
	
	/**
 	 * Histogram calculation.
 	 */
	void operator()(const blocked_range2d<size_t,size_t>& range) {
		IntMatrix& image = *_image;
		concurrent_vector<int>& hist = *histogram;
		
		const blocked_range<size_t>& rows = range.rows();
		const blocked_range<size_t>& cols = range.cols();
		
		for (size_t y = rows.begin(); y != rows.end(); ++y) {
			for (size_t x = cols.begin(); x != cols.end(); ++x) {
				hist[image[y][x]]++;
			}
		}
	}
	
	/**
	 * Splitting (TBB) constructor
	 */
	Histogram(Histogram& other, split): _image(other._image), maxValue(other.maxValue) {
		histogram = new int[maxValue + 1]{0};
	}
	
	/**
	 * Joiner (TBB).
	 */
	void join(const Histogram& other) {
		// SERIAL... XXX can we speed up by reducing here, too?
		for (int i = 0; i < maxValue; ++i) {
			(*histogram)[i] += (*other.histogram)[i];
		}
	}
	
};

/**
 * This class takes an Integer array to a boolean array based on a cut-off value.
 */
class Threshold {

	IntMatrix *_image;
	BoolMatrix *_result;
	const int retain;

public: 

	static BoolMatrix* exec(IntMatrix* image, int retain) {
		BoolMatrix* result = new BoolMatrix();

		Threshold thresh(image, retain, result);
		parallel_for(blocked_range<size_t,size_t>(0, NROWS, 0, NCOLS), thresh, auto_partitioner());

		return result;
	}
	
public:

	float _sum;
	
	float getSum() const {
		return _sum;
	}
	
	void operator()(const blocked_range2d<size_t,size_t>& range) {
		IntMatrix& image = *_image;
		BoolMatrix& result = *_result;
		
		const blocked_range<size_t>& rows = range.rows();
		const blocked_range<size_t>& cols = range.cols();
		
		for (size_t y = rows.begin(); y != rows.end(); ++y) {
			for (size_t x = cols.begin(); x != cols.end(); ++x) {
				result[y][x] = image[y][x] > retain;
			}
		}
	}
	
	Threshold(IntMatrix* image, int retain, BoolMatrix* result):
		_image(image), _result(result), retain(retain) { }

};

/*****************************************************************************/

/**
 * Returns a pseudorandom number ~ U[mean - range, mean + range].
 */
real uniform(real mean, real range) {
	return (rand() / (real)RAND_MAX) * (2.0f * range) - range + mean;
}

int main(int argc, char** argv) {
	
	IntMatrix* matrix = new IntMatrix();
	
	// seed the random number generator.
	srand(time(0));
	
	// SERIAL: generate random values for the matrix
	for (int row = 0; row < NROWS; ++row) {
		for (int col = 0; col < NCOLS; ++col) {
			matrix[row][col] = uniform(0.0f, 20.0f);
		}
	}
	
	// random value for retain
	float cutoff = uniform(0.5f, 0.5f);
	
	// start up TBB
	task_scheduler_init init;
	
	// do everything.
	int retain = Histogram::calculate(matrix, cutoff);
	BoolMatrix* result = Threshold::exec(matrix, retain);

	// exit the program.
	return 0;

}

