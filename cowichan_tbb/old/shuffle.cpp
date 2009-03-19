#include <iostream>
#include <cstdlib>
#include <ctime>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range2d.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

const int SIZE = 10000;

typedef float	real;
typedef real	Vector[SIZE];
typedef real	Matrix[SIZE][SIZE];

/**
 * This class does a halving shuffle.
 */
class Shuffle {

public:
	
	Matrix *_first, *_second;

	Shuffle(Matrix *input, Matrix *output):
		_first(input), _second(output) { }

	/**
	 * Performs the halving shuffle over the given range.
	 */
	void operator()(const blocked_range2d<size_t,size_t>& range) const {
		
		Matrix& first = *_first;
		Matrix& second = *_second;
		const blocked_range<size_t>& rows = range.rows();
		const blocked_range<size_t>& cols = range.cols();
		
		for (size_t y = rows.begin(); y != rows.end(); ++y) {
			for (size_t x = cols.begin(); x != cols.end(); ++x) {
				
				// assign from swapped co-ordinates.
				second[y][x] = first[y ^ 0x01][x ^ 0x01];
				
			}
		}
		
	}
};

/*****************************************************************************/

real uniform(real mean, real range) {
	return (rand() / (real)RAND_MAX) * (2.0f * range) - range + mean;
}

int main(int argc, char** argv) {
	
	Matrix input, output;
	
	// seed the random number generator.
	srand(time(0));
	
	// SERIAL: generate random values for game of life
	for (int y = 0; y < SIZE; ++y) {
		for (int x = 0; x < SIZE; ++x) {
			input[y][x] = uniform(0.0, 100.0);
		}
	}
	
	// start up TBB
	task_scheduler_init init;
	
	// do the halving shuffle
	Shuffle shuffle(&input, &output);
	parallel_for(
		blocked_range2d<size_t,size_t>(0, SIZE, 0, SIZE),
		shuffle, auto_partitioner());	

	return 0;

}

