#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cmath>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

const int SIZE = 10000;

typedef float	real;
typedef real	Vector[SIZE];

/**
 * This classs takes the difference between two similarly sized vectors.
 */
class VectorDifference {
	
	Vector *_result;
	Vector *_v1, *_v2;

public:

	VectorDifference(Vector *v1, Vector *v2, Vector *result):
		_v1(v1), _v2(v2), _result(result)
		{ }

	/**
	 * Performs matrix-vector multiplication on the given row range.
	 */
	void operator()(const blocked_range<size_t>& range) const {
		
		Vector& v1 = *_v1;
		Vector& v2 = *_v2;
		Vector& result = *_result;
		
		for (size_t row = range.begin(); row != range.end(); ++row) {

			// compute the vector difference at this result element.
			result[row] = v1[row] - v2[row];

		}		
	}
	
};

/*****************************************************************************/

real uniform(real mean, real range) {
	return (rand() / (real)RAND_MAX) * (2.0f * range) - range + mean;
}

int main(int argc, char** argv) {

	Vector v1, v2;
	Vector result;
	
	// seed the random number generator.
	srand(time(0));
	
	// SERIAL: generate random values for the vectors
	for (int row = 0; row < SIZE; ++row) {
		v1[row] = uniform(0.0f, 100.0f);
		v2[row] = uniform(0.0f, 100.0f);
	}
	
	// start up TBB
	task_scheduler_init init;
	
	// get the outer product.
	VectorDifference diff(&v1, &v2, &result);
	parallel_for(
		blocked_range<size_t>(0, SIZE),
		diff, auto_partitioner());
	
}

