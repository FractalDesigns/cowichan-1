#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <limits>
#include <algorithm>
#include <iomanip>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

/*****************************************************************************/

typedef float 	real;

const real SOR_OMEGA = 0.9;
const real SOR_TOLERANCE = 10e-6;

const int SIZE = 6;

typedef real	Vector[SIZE];
typedef real	Matrix[SIZE][SIZE];

/*****************************************************************************/

class RowSummer {

	Matrix *_matrix;
	Vector *_sums, *_answer;

public:

	static void perform(Matrix& matrix, Vector& answer, Vector* sums) {
		RowSummer sum(&matrix, &answer, sums);
		parallel_for(
			blocked_range<size_t>(0, SIZE),
			sum,
			auto_partitioner());
	}
	
public:

	RowSummer(Matrix* matrix, Vector* answer, Vector* sums):
		_matrix(matrix), _answer(answer), _sums(sums) { }
		
	void operator()(const blocked_range<size_t>& rows) const {

		Matrix& matrix = *_matrix;
		Vector& sums   = *_sums;
		Vector& answer = *_answer;

		for (size_t y = rows.begin(); y != rows.end(); ++y) {
			sums[y] = 0.0;
			for (size_t x = 0; x < SIZE; ++x) {
				if (x != y)
					sums[y] += matrix[y][x] * answer[x];
			}
		}
	
	}

};

/*----------------------------------------------*/

class Relaxer {

	Matrix *_matrix;
	Vector *_vector, *_sums, *_answer;
	real norm;

public:

	/**
	 * Performs the SOR process. Requires that answer already contains a guess.
	 */
	static void performSOR(Matrix& matrix, Vector& vector, Vector* answer, real tolerance) {
				
		int numIterations = 0;		
		
		Relaxer* relaxer = NULL;
		do {

			// gather row sums for the SOR process
			Vector sums;
			RowSummer::perform(matrix, *answer, &sums);
			
			// "relax" the answer vector, while at the same time calculating tolerance
			if (relaxer) delete relaxer;
			relaxer = new Relaxer(&matrix, &vector, answer, &sums);
			parallel_reduce(
				blocked_range<size_t>(0, SIZE),
				*relaxer,
				auto_partitioner());

			// increase the iteration counter
			++numIterations;

		} while (relaxer->getNorm() >= tolerance);
		
		// DEBUG: print the number of iterations.
		std::cout << "SOR in " << numIterations << " iterations." << std::endl;
		
		// make sure we get rid of the final Relaxer instance.
		if (relaxer) delete relaxer;
		
	}

public:

	real getNorm() const {
		return norm;
	}

	void operator()(const blocked_range<size_t>& rows) {

		Matrix& matrix = *_matrix;
		Vector& vector = *_vector;
		Vector& answer = *_answer;
		Vector& sums   = *_sums;

		for (size_t y = rows.begin(); y != rows.end(); ++y) {

			// save the old answer
			real oldAnswer = answer[y];

			// blend between the old answer and the new answer
			answer[y] =
				(1.0 - SOR_OMEGA) * oldAnswer +
				SOR_OMEGA 		  * (vector[y] - sums[y]) / matrix[y][y];

			// store the magnitude of the quickest change (1-norm)
			norm = std::max(norm, (real) fabs(oldAnswer - answer[y]));

		}
		
	}

	Relaxer(Matrix* matrix, Vector* vector, Vector* answer, Vector* sums):
		_matrix(matrix), _vector(vector), _sums(sums), _answer(answer), norm(0.0)
		{ }

	Relaxer(Relaxer& p, split):
		_matrix(p._matrix), _vector(p._vector), _sums(p._sums), _answer(p._answer), norm(0.0)
		{ }

	void join(const Relaxer& other) {
		norm = std::max(norm, other.norm);
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

	Matrix matrix, savedmatrix;
	Vector vector, answer;

	real maxValue = std::numeric_limits<real>::min();
	
	// seed the random number generator.
	srand(time(0));
	
	// SERIAL: generate random values for the vector and matrix;
	// start a guess for the answer (a vector with all ones).
	for (int row = 0; row < SIZE; ++row) {
		vector[row] = uniform(0.0f, 1000.0f);
		answer[row] = 1.0;
		for (int col = 0; col < SIZE; ++col) {
			matrix[row][col] = uniform(0.0f, 20.0f);
			savedmatrix[row][col] = matrix[row][col];
			maxValue = std::max(maxValue, matrix[row][col]);
		}
	}
	
	// SERIAL: ensure that the matrix is diagonally dominant.
	maxValue *= SIZE;
	for (int x = 0; x < SIZE; ++x) {
		matrix[x][x] = maxValue;
		savedmatrix[x][x] = matrix[x][x];
	}
	
	// start up TBB
	task_scheduler_init init; 

	// figure out the answer.
	Relaxer::performSOR(matrix, vector, &answer, SOR_TOLERANCE);
	
	// we've got the answer, now, in answer. Print it out.
	std::cout.precision(5);
	for (int row = 0; row < SIZE; ++row) {

		// print out the (saved) matrix
		std::cout << " [ ";
		for (int col = 0; col < SIZE; ++col) {
			std:: cout << savedmatrix[row][col] << "\t";
		}
		
		// print out the answer
		std::cout << "] [ " << answer[row] << " ]\t";

		// print out the (saved) vector
		if (row == int(SIZE / 2)) {
			std::cout << "= [ ";
		} else {
			std::cout << "  [ ";
		}
		std::cout << vector[row] << " ]" << std::endl;
		
	}

}

