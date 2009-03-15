#include <iostream>
#include <cstdlib>
#include <ctime>
#include <algorithm>
#include <iomanip>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

const int SIZE = 4;

typedef float real;

/**
 * This class performs Gauss-Jordan elimination on a matrix. It computes the
 * modified answer vector and the rank of the matrix.
 */
class GaussJordan {
public:
	
	real (*_matrix)[SIZE][SIZE];
	real (*_vector)[SIZE];

public:
	
	/**
	 * Performs the forward-eliminiation phase (=> row-echelon).
	 * Expects a pivoted matrix as input (but makes no other assumptions).
	 */
	class ForwardElimination {
	public:
	
		GaussJordan* parent;
		ForwardElimination(GaussJordan* parent): parent(parent) { };
		
		void operator()(const blocked_range<size_t>& rows) const {
			
			// Get pointers locally.
			real (&matrix)[SIZE][SIZE]	= *(parent->_matrix);
			real (&vector)[SIZE]		= *(parent->_vector);
			
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
		
	};

	/** 
	 * Performs the back-substitution phase (=> reduced row-echelon)
	 */
	class BackSubstitution {
	public:
		
		GaussJordan* parent;
		BackSubstitution(GaussJordan* parent): parent(parent) { };

		void operator()(const blocked_range<size_t>& rows) const {
			
			// Get pointers locally.
			real (&matrix)[SIZE][SIZE]	= *(parent->_matrix);
			real (&vector)[SIZE]		= *(parent->_vector);
			
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
		
	};
	
	ForwardElimination forward;
	BackSubstitution backward;

public:

	GaussJordan(real (*matrix)[SIZE][SIZE], real (*vector)[SIZE]):
		forward(this), backward(this),
		_matrix(matrix), _vector(vector)
		{ }
		
	/**
	 * Performs Gauss-Jordan elimination to solve Ax = b, given A and b.
	 * @param A the matrix. This matrix is modified to become the identity matrix,
	 *          and may be rank-deficient (of course, based on A!)
	 * @param b the vector. This vector is modified to become the answer.
	 */
	static void perform(real (*A)[SIZE][SIZE], real (*b)[SIZE]) {
		
		GaussJordan gauss(A, b);
		
		// pivoting must be done in series.
		gauss.pivot();
		
		// the forward/backward phases can be split up among the workers.
		parallel_for(blocked_range<size_t>(0, SIZE), gauss.forward, auto_partitioner());
		parallel_for(blocked_range<size_t>(0, SIZE), gauss.backward, auto_partitioner());
		
		// b is transformed to become the answer, so we needn't return anything.
		return;
		
	}

private:

	/**
	 * Performs sequential partial pivoting of the current matrix
	 * and the associated "b" vector.
	 */
	void pivot() {
		
		// Get pointers locally.
		real (&matrix)[SIZE][SIZE]	= *(_matrix);
		real (&vector)[SIZE]		= *(_vector);
			
		for (int col = 0; col < SIZE; ++col) {
	
			// compute the pivot for this column
			int pivotRow = 0, maxValue = 0;
			for (int row = col; row < SIZE; ++row) {
				if (abs(matrix[row][col]) > maxValue)
					pivotRow = row;
			}
		
			// swap rows (col) and (pivotRow)
			std::swap_ranges(matrix[col], matrix[col] + (SIZE - 1), matrix[pivotRow]);
			
			// swap vector rows in the same fashion
			std::swap(vector[col], vector[pivotRow]);
		
		}
	}
	
};

/*****************************************************************************/

/**
 * Entry point of the program.
 */
int main(int argc, char** argv) {

	real matrix[SIZE][SIZE];
	real savedmatrix[SIZE][SIZE];
	real vector[SIZE];
	real answer[SIZE];
	
	// seed the random number generator.
	srand(time(0));
	
	// SERIAL: generate random values for the vector and matrix
	for (int row = 0; row < SIZE; ++row) {
		vector[row] = uniform(0.0f, 100.0f);
		answer[row] = vector[row];
		for (int col = 0; col < SIZE; ++col) {
			matrix[row][col] = uniform(-20.0f, 20.0f);
			savedmatrix[row][col] = matrix[row][col];
		}
	}
	
	// start up TBB
	task_scheduler_init init;

	// multiply the matrix by the vector (NB. answer == vector).
	GaussJordan::perform(&matrix, &answer);
	
	// we've got the answer, now, in answer. Print it out.
	std::cout.precision(3);
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

