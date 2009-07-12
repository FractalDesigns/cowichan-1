#include "cowichan_tbb.hpp"

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

private:

	IntMatrix _matrix;
	IntVector state;
	INT_TYPE aPrime, cPrime;
  index_t nr, nc;
	
	/**
	 * Generates the next random number.
	 * Convenience method for next(current, a, c);
	 */
	inline INT_TYPE next(INT_TYPE& current) const {
		return (RANDMAT_A * current + RANDMAT_C) % RAND_M;
	}
	
	/**
	 * Generates the k-th next random number, using an identity,
	 * 		where A = a^k mod m
	 *        and C = c * sum[j=0..k-1](a^j mod m).
	 */
	inline INT_TYPE nextK(INT_TYPE& current) const {
		return (aPrime * current + cPrime) % RAND_M;
	}
	
	/**
	 * Initialises the random number generator to operate on
	 * NROWS different rows completely independently.
	 */
	void initialise() {
	
		state = NEW_VECTOR_SZ(INT_TYPE, nr);
	
		// generate first column values
		VECTOR(state, 0) = RAND_SEED % RAND_M;
		for (index_t row = 1; row < nr; ++row) {
			VECTOR(state, row) = next(VECTOR(state, row - 1));
		}
		
		// generate the A and C values for the next(k) method.
		aPrime = RANDMAT_A;
		cPrime = 1;
		for (index_t i = 1; i < nr; ++i) {
			cPrime = (cPrime + aPrime) % RAND_M;
			aPrime = (aPrime * RANDMAT_A) % RAND_M;
		}
		cPrime = (cPrime * RANDMAT_C) % RAND_M;
	
	}

public:

	/**
	 * Provides NROWS-seperated linear congruential RNG on the specified range
	 * of rows.
	 */
	void operator()(const Range& rows) const {
		
		IntMatrix matrix = _matrix;
		IntVector init = state;
		
		for (index_t row = rows.begin(); row != rows.end(); ++row) {
			
			// copy over the seed value for this row
			MATRIX_RECT(matrix, row, 0) = VECTOR(init, row);
			
			// for each other column, provide NROWS-spaced random numbers
			// using the specialty method RandomGenerator.nextK(current).
			for (index_t col = 1; col < nc; ++col) {
				MATRIX_RECT(matrix, row, col) = nextK(MATRIX_RECT(matrix, row, col - 1));
			}
			
		}
		
	}

public:

  RandomGenerator(index_t nr, index_t nc) : nr(nr), nc(nc) {
		initialise();
	}

	void execute(IntMatrix matrix) {
		_matrix = matrix;
		parallel_for(Range(0, nr), *this, auto_partitioner());
	}
	
};

/*****************************************************************************/

void CowichanTBB::randmat(IntMatrix matrix) {
	
	// run the random number generator.
	RandomGenerator generator(nr, nc);
	generator.execute(matrix);

}

