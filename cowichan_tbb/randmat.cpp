#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <climits>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

const int NROWS = 55;		// the number of rows in the produced matrix
const int NCOLS = 1024;		// the number of columns in the produced matrix
const uint S = 681304;		// seed value for simple random number generator

/*****************************************************************************/

typedef unsigned int uint;

/****************************************************************************/

/**
 * This class generates random integers in a matrix, using a simple linear
 * congruential number generator with a given seed value, i.e. using the
 * recurrence:
 * 
 * 		X_i+1 = (a*X_i + c) mod m
 *
 * The class also provides a parallel implementation with parallel_for.
 */
class RandomGenerator {

	const static uint a = 1103515245; // these two constants are values for the
	const static uint c = 12345;	  // linear congruential RNG algorithm
	const static uint m = UINT_MAX;	  // the modulus to use
	uint s;							  // the initial, seed value.

private:

	uint (*_matrix)[NROWS][NCOLS];
	uint state[NROWS];
	uint aPrime, cPrime;

	/**
	 * Generates the next random number.
	 * Convenience method for next(current, a, c);
	 */
	inline uint next(uint& current) const {
		return (a * current + c) % m;
	}
	
	/**
	 * Generates the k-th next random number, using an identity,
	 * 		where A = a^k mod m
	 *        and C = c * sum[j=0..k-1](a^j mod m).
	 */
	inline uint nextK(uint& current) const {
		return (aPrime * current + cPrime) % m;
	}
	
	/**
	 * Initialises the random number generator to operate on
	 * NROWS different rows completely independently.
	 */
	void initialise() {
	
		// generate first column values
		state[0] = s % m;
		for (int row = 1; row < NROWS; ++row) {
			state[row] = next(state[row - 1]);
		}
		
		// generate the A and C values for the next(k) method.
		aPrime = a;
		cPrime = 1;
		for (int i = 1; i < NROWS; ++i) {
			cPrime = (cPrime + aPrime) % m;
			aPrime = (aPrime * a) % m;
		}
		cPrime = (cPrime * c) % m;
	
	}

public:

	/**
	 * Provides NROWS-seperated linear congruential RNG on the specified range
	 * of rows.
	 */
	void operator()(const blocked_range<size_t>& rows) const {
		
		uint (&matrix)[NROWS][NCOLS] = *_matrix;
		const uint (&init)[NROWS] = state;
		
		for (size_t row = rows.begin(); row != rows.end(); ++row) {
			
			// copy over the seed value for this row
			matrix[row][0] = init[row];
			
			// for every other column, provide NROWS-spaced random numbers
			// using the specialty method RandomGenerator.nextK(current).
			for (int col = 1; col < NCOLS; ++col) {
				matrix[row][col] = nextK(matrix[row][col - 1]);
			}
			
		}
		
	}

public:

	RandomGenerator(uint seed): s(seed) {
		initialise();
	}

	void executeParallel(uint (*matrix)[NROWS][NCOLS]) {
		_matrix = matrix;
		parallel_for(blocked_range<size_t>(0, NROWS), *this, auto_partitioner());
	}
	
	void executeSerial(uint (*matrix)[NROWS][NCOLS]) {
		_matrix = matrix;
		(*this)(blocked_range<size_t>(0, NROWS));
	}

};

/*****************************************************************************/

int main(int argc, char** argv) {

	uint matrix[NROWS][NCOLS];

	// start up TBB
	task_scheduler_init init;
	
	// run the random number generator.
	RandomGenerator generator(S);
	generator.executeParallel(&matrix);

	// print out the last column of numbers.
	for (int i = 0; i < NROWS; ++i) {
		std::cout << matrix[i][NCOLS - 1] << " ";
	}
	std::cout << std::endl;

}

