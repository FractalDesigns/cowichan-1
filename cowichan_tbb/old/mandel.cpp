#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cmath>
#include <limits>
#include <algorithm>
#include <iomanip>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range2d.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

/*****************************************************************************/

typedef float 	real;

#define MANDEL_INFINITY 2.0
#define MANDEL_MAX_ITER 150

const int NROWS = 5000;
const int NCOLS = 5000;

typedef int		IntMatrix[NROWS][NCOLS];
typedef bool	BoolMatrix[NROWS][NCOLS];

/*****************************************************************************/

class Mandelbrot {

	IntMatrix *_matrix;		// to store the result.

	real dX, dY;			// co-ordinate -> complex plane mapping coeff.
	real baseX, baseY;		// where to start the mandelbrot set

private:

	/**
 	 * Performs the mandelbrot set calculation.
 	 */
	int mandelCalc(real x, real y) const {

		real r = 0.0, i = 0.0;
		real rs = 0.0, is = 0.0;
		int numIterations = 0;		
		do {
		
			// calculate the complex value according to the mandelbrot set specs.
			i = (2.0 * r * i) + x;
			r = (rs - is) + y;
			
			// calculate squared complex value
			rs = r * r;
			is = i * i;			
			
			// "step" the simulation for this co-ordinate.
			++numIterations;			
			
		} while ((numIterations < MANDEL_MAX_ITER) && ((rs + is) < MANDEL_INFINITY));
		
		// we are interested if the series converges or diverges. Return the
		// number of iterations before such an event (divergence).
		return numIterations;
		
	}
	
public:

	/**
 	 * Calculates the given mandelbrot set "window", and stores the result in matrix.
 	 */
	static void perform(IntMatrix* matrix, real x, real y, real width, real height) {
		
		Mandelbrot mandel(matrix, x, y, width, height);
		parallel_for(
			blocked_range2d<size_t,size_t>(0, NROWS, 0, NCOLS),
			mandel,
			auto_partitioner());
		
	}
	
	
public:

	Mandelbrot(IntMatrix* matrix, real x, real y, real width, real height):
		_matrix(matrix), baseX(x), baseY(y) {
		
		dX = width / (NCOLS - 1);
		dY = height / (NROWS - 1);
			
	}

	/**
 	 * Calculates a given portion of the current mandelbrot set "window".
 	 */
	void operator()(const blocked_range2d<size_t,size_t>& range) const {

		IntMatrix& matrix = *_matrix;

		const blocked_range<size_t>& rows = range.rows();
		const blocked_range<size_t>& cols = range.cols();
		
		for (size_t y = rows.begin(); y != rows.end(); ++y) {
			for (size_t x = cols.begin(); x != cols.end(); ++x) {
				matrix[y][x] = mandelCalc(baseX + (x * dX), baseY + (y * dY));
			}
		}
		
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

	IntMatrix matrix;
	
	// start up TBB
	task_scheduler_init init;

	// perform the mandelbrot set calculation
	Mandelbrot::perform(&matrix, -2.0, -1.0, 4.0, 2.0);

}

