#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cmath>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

const int SIZE = 100;

/**
 * This class multiplies a matrix by a vector, producing a vector, a la:
 *	[x1 x2 x3]   [y1]   [(x1*y1 + x2*y2 + x3*y3)]
 *	[x4 x5 x6] * [y2] = [(x4*y1 + x5*y2 + x6*y3)]
 *	[x7 x8 x9]   [y3]   [(x7*y1 + x8*y2 + x9*y3)]
 */
class Product {
	
	float (*_matrix)[SIZE][SIZE];
	float (*_vector)[SIZE];
	float (*_result)[SIZE];

public:

	Product(float (*matrix)[SIZE][SIZE], float (*vector)[SIZE], float (*result)[SIZE]):
		_matrix(matrix), _vector(vector), _result(result) { }

	/**
	 * Performs matrix-vector multiplication on the given row range.
	 */
	void operator()(const blocked_range<size_t>& rows) {
		
		float (&matrix)[SIZE][SIZE] = *_matrix;
		float (&vector)[SIZE] = *_vector;
		float (&result)[SIZE] = *_result;
		
		for (size_t row = rows.begin(); row != rows.end(); ++row) {
			
			result[row] = 0.0f;
			for (int col = 0; col < SIZE; ++col) {
				result[row] += matrix[row][col] * vector[col];
			}
			
		}
		
	}
};

/*****************************************************************************/

/**
 * This class takes the 2-norm of the difference of two vectors.
 */
class NormDiff {

	float *const _vec1;
	float *const _vec2;

public: 

	static float exec(float *vec1, float *vec2, size_t n) {
		NormDiff norm(vec1, vec2);
		parallel_reduce(blocked_range<size_t>(0, n), norm, auto_partitioner());
		return norm.getNorm();
	}
	
public:

	float _norm;
	
	float getNorm() const {
		return sqrt(_norm);
	}
	
	void operator()(const blocked_range<size_t>& range) {
		float *vec1 = _vec1;
		float *vec2 = _vec2;
		float norm = _norm;
		for (size_t i = range.begin(); i != range.end(); ++i) {
			float diff = vec1[i] - vec2[i];
			norm += diff * diff;
		}
		_norm = norm;
	}
	
	/**
	 * Splitting (TBB) constructor
	 */
	NormDiff(NormDiff& other, split):
		_vec1(other._vec1), _vec2(other._vec2), _norm(0) { }
	
	/**
	 * Joiner (TBB).
	 */
	void join(const NormDiff& other) {
		_norm += other.getNorm();
	}
	
	NormDiff(float *vec1, float *vec2): _vec1(vec1), _vec2(vec2), _norm(0) { }

};

/*****************************************************************************/

float uniform(float mean, float range) {
	return (rand() / (float)RAND_MAX) * (2.0f * range) - range + mean;
}

int main(int argc, char** argv) {

	float matrix[SIZE][SIZE];
	float vector[SIZE];
	float result[SIZE];
	
	// seed the random number generator.
	srand(time(0));
	
	// SERIAL: generate random values for the vector; create an identity matrix
	for (int row = 0; row < SIZE; ++row) {
		vector[row] = uniform(0.0f, 100.0f);
		for (int col = 0; col < SIZE; ++col) {
			// set up matrix
			if (row == col) {
				matrix[row][col] = 1.0f;
			} else {
				matrix[row][col] = 0.0f;
			}
		}
	}
	
	// start up TBB
	task_scheduler_init init;
	
	// multiply the matrix by the vector; store the result in result.
	Product product(&matrix, &vector, &result);
	parallel_for(blocked_range<size_t>(0, SIZE), product, auto_partitioner());
	
	// report the 2-norm of the difference between vector and result.
	std::cout << "The 2-norm is "
			  << NormDiff::exec((float*)vector, (float*)result, SIZE)
			  << "; should be close to zero." << std::endl;
	
}

