#include <iostream>
#include <cstdlib>
#include <ctime>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

const int SIZE = 100;

/**
 * This class performs Gauss-Jordan elimination on a matrix. It computes the
 * perturbed answer vector and the rank of the matrix.
 */
class GaussJordan {
public:
	
	float (*_matrix)[SIZE][SIZE];
	float (*_vector)[SIZE];
	float (*_answer)[SIZE];

public:
	
	/**
	 * Performs the forward-eliminiation phase (=> row-echelon).
	 * Expects a pivoted matrix as input (but makes no other assumptions).
	 */
	class ForwardElimination {
	
		GaussJordan* parent;
		ForwardElimination(GaussJordan* parent): parent(parent) { };
		
		void operator()(const blocked_range<size_t>& rows) {
			
			// Get pointers locally.
			float (*matrix)[SIZE][SIZE]	= parent->_matrix;
			float (*vector)[SIZE]		= parent->_vector;
			float (*answer)[SIZE]		= parent->_answer;
			
			for (size_t row = rows.begin(); row != rows.end(); ++row) {
				
				// for each preceding row...
				for (int previous = 0; previous < row; ++previous) {
					
					// subtract out that row scaled by a factor to introduce a zero
					real factor = matrix[row][previous] / matrix[previous][previous];
					for (int col = 0; col < SIZE; ++col) {
						matrix[row][col] -= factor * matrix[previous][col];
					}
					vector[row] -= factor * vector[previous];
					
					// should be zero anyway, but because of rounding issues...					
					matrix[row][previous] = 0.0;
					
				}
							
				// re-normalize the remaining values
				for (int col = row + 1; col < SIZE; ++col) {
					matrix[row][col] /= matrix[row][row];
				}
				vector[row] /= matrix[row][row];
				
				// rounding issues again.
				matrix[row][row] = 1.0;
				
			}
		}
		
	} forward;

	/** 
	 * Performs the back-substitution phase (=> reduced row-echelon)
	 */
	class BackSubstitution {
		
		GaussJordan* parent;
		ForwardElimination(GaussJordan* parent): parent(parent) { };

		void operator()(const blocked_range<size_t>& rows) {
			
			// Get pointers locally.
			float (*matrix)[SIZE][SIZE]	= parent->_matrix;
			float (*vector)[SIZE]		= parent->_vector;
			float (*answer)[SIZE]		= parent->_answer;

			for (size_t row = rows.begin(); row != rows.end(); ++row) {
	
				// for each following row...
				for (int next = row + 1; next < SIZE; ++next) {

					// subtract out that row scaled by a factor to introduce a zero
					real factor = matrix[row][next];
					for (int col = row + 1; col < SIZE; ++col) {
						matrix[row][col] -= factor * matrix[next][col];
					}
					vector[row] -= factor * vector[next];
					
					// rounding issues again (see ForwardElimination::operator()).
					matrix[row][next] = 0.0;
			
				}
	
			}			
		}
		
	} backward;

public:

	GaussJordan(float (*matrix)[SIZE][SIZE], float (*vector)[SIZE], float (*answer)[SIZE]):
		forward(this), backward(this),
		_matrix(matrix), _vector(vector), _answer(answer)
		{ }
		
	/**
	 * Performs Gauss-Jordan elimination to solve Ax = b, given A and b.
	 * Modifies .
	 * @return x, the "answer" to the "question" Ax = b.
	 *         the calling function should call delete[] on x.
	 */
	(float[SIZE])* GaussJordan::perform(float (*A)[SIZE][SIZE], float (*b)[SIZE][SIZE]) {
		
		GaussJordan gauss(A, b, new float[SIZE]);
		
		// pivoting must be done in series.
		gauss.pivot();
		
		// the forward/backward phases can be split up among the workers.
		parallel_for(gauss.forward, blocked_range<size_t>(0, SIZE), auto_partitioner());
		parallel_for(gauss.backward, blocked_range<size_t>(0, SIZE), auto_partitioner());
		
		// return the answer to the calling function.
		// I'll re-iterate that the calling function owns the pointer.
		return gauss->answer;
		
	}

private:

	/**
	 * Performs sequential partial pivoting of the current matrix
	 * and the associated "b" vector.
	 */
	void pivot() {
		for (int col = 0; col < SIZE; ++col) {
	
			// compute the pivot for this column
			int pivotRow = 0, maxValue = 0;
			for (int row = col; row < SIZE; ++row) {
				if (abs(matrix[row][col]) > maxValue)
					pivotRow = row;
			}
		
			// swap rows (col) and (pivotRow)
			real rowPointer[SIZE] = matrix[col];
			matrix[col] = matrix[pivotRow];
			matrix[pivotRow] = rowPointer;
		
			// swap vector rows in the same fashion
			real savedValue = vector[col];
			vector[col] = vector[pivotRow];
			vector[pivotRow] = savedValue;
		
		}
	}
	
};

/*****************************************************************************/

/**
 * Returns a pseudorandom number ~ U[mean - range, mean + range].
 */
float uniform(float mean, float range) {
	return (rand() / (float)RAND_MAX) * (2.0f * range) - range + mean;
}

/**
 * Entry point of the program.
 */
int main(int argc, char** argv) {

	float matrix[SIZE][SIZE];
	float vector[SIZE];
	float* result[SIZE];
	
	// seed the random number generator.
	srand(time(0));
	
	// SERIAL: generate random values for the vector and matrix
	for (int row = 0; row < SIZE; ++row) {
		vector[row] = uniform(0.0f, 100.0f);
		for (int col = 0; col < SIZE; ++col) {
			matrix[row][col] = uniform(-20.0f, 20.0f);
		}
	}
	
	// start up TBB
	task_scheduler_init init;
	
	// multiply the matrix by the vector; store the result in result.
	result = GaussJordan::perform(&matrix, &vector);
	
	// report the 2-norm of the difference between vector and result.
	std::cout << "Done!?" << std::endl;
	
}

