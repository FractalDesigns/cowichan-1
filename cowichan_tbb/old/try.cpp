#include <iostream>
#include <cstdlib>
#include <ctime>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

/**
 * This class multiplies negative numbers in a given array.
 */
class MultiplyNegatives {
	
	float *const _arr;
	const float mult;
	
public:

	static void exec(float arr[], size_t n, float multiplier) {
		parallel_for(
			blocked_range<size_t>(0, n),
			MultiplyNegatives(arr, multiplier),
			auto_partitioner());
	}
	
public:

	MultiplyNegatives(float arr[], float multiplier):
		_arr(arr), mult(multiplier) { }

	/**
	 * Performs the posify operation over the given range.
	 */
	void operator()(const blocked_range<size_t>& range) const {
		float *arr = _arr;
		for (size_t i = range.begin(); i != range.end(); ++i) {
			if (arr[i] < 0) arr[i] *= mult;
		}
	}
};

/**
 * This class sums numbers in an array and returns the result.
 */
class Sum {

	float *const _arr;

public: 

	static float exec(float arr[], size_t n) {
		Sum summer(arr);
		parallel_reduce(blocked_range<size_t>(0, n), summer, auto_partitioner());
		return summer.getSum();
	}
	
public:

	float _sum;
	
	float getSum() const {
		return _sum;
	}
	
	void operator()(const blocked_range<size_t>& range) {
		float *arr = _arr;
		float sum = _sum;
		for (size_t i = range.begin(); i != range.end(); ++i) {
			sum += arr[i];
		}
		_sum = sum;
	}
	
	/**
	 * Splitting (TBB) constructor
	 */
	Sum(Sum& other, split): _arr(other._arr), _sum(0) { }
	
	/**
	 * Joiner (TBB).
	 */
	void join(const Sum& other) {
		_sum += other.getSum();
	}
	
	Sum(float arr[]): _arr(arr), _sum(0) { }

};

/*****************************************************************************/

#define NUM_VALUES 9999999 // The number of array values to test with.
#define FACTOR 0.92f	   // The factor to scale negative numbers by.
#define LOW_BOUND -10.0f   // The lowest random number to possibly produce.
#define HIGH_BOUND 6.0f    // The highest random number to possibly produce.

int main(int argc, char** argv) {
	
	// seed the random number generator.
	srand(time(0));
	
	// SERIAL: generate random values in range [LOW_BOUND, HIGH_BOUND]
	float* values = new float[NUM_VALUES];
	for (int i = 0; i < NUM_VALUES; ++i) {
		float randFloat = rand() / float(RAND_MAX);
		values[i] = randFloat * (HIGH_BOUND - LOW_BOUND) + LOW_BOUND;
	}
	
	// start up TBB
	task_scheduler_init init;
	
	// while the average is still negative (and thus the sum),
	// "posify" the array.
	float sum = Sum::exec(values, NUM_VALUES);
	while (sum < 0) {
		std::cout << "Average value is: " << (sum / (float)NUM_VALUES)
				  << std::endl;
		MultiplyNegatives::exec(values, NUM_VALUES, FACTOR);
		sum = Sum::exec(values, NUM_VALUES);
	}
	
	// print out the final average value.
	std::cout << "Average value is: " << (sum / (float)NUM_VALUES)
			  << std::endl;

	// exit the program.	
	return 0;

}

