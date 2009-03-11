#include <iostream>
#include <cstdlib>
#include <ctime>
#include <limits>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range2d.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

using std::numeric_limits;

const size_t NROWS = 10000;
const size_t NCOLS = 10000;

typedef float	real;
typedef int		IntMatrix[NROWS][NCOLS];
typedef bool	BoolMatrix[NROWS][NCOLS];

#define MAXIMUM_INT numeric_limits<int>::max()

/*
 * This class finds the next invasion percolation position.
 */
class FluidPercolation {
	
	IntMatrix* _image;
	BoolMatrix* _fluid;
	int x, y;
	int minimum;
	
private:

	bool hasNeighbours(int x, int y) const {

		int peers = 0;

		// calculate possible neighbour positions
		bool l = (x > 0);
		bool r = (x < (NCOLS - 1));
		bool u = (y > 0);
		bool d = (y < (NROWS - 1));		

		// calculate no. of neighbours
		if (l && (*_fluid)[y][x-1]) 		++peers;
		if (l && u && (*_fluid)[y-1][x-1]) 	++peers;
		if (u && (*_fluid)[y-1][x]) 		++peers;
		if (r && u && (*_fluid)[y-1][x+1]) 	++peers;
		if (r && (*_fluid)[y][x+1])			++peers;
		if (r && d && (*_fluid)[y+1][x+1])	++peers;
		if (d && (*_fluid)[y+1][x]) 		++peers;
		if (l && d && (*_fluid)[y+1][x-1]) 	++peers;		
		
		return (peers > 0);
		
	}
	
public:

	static void perform(IntMatrix& image, BoolMatrix& fluid) {
		
		// compute the histogram to get a thresholding value
		FluidPercolation perc(&image, &fluid);
		parallel_reduce(
			blocked_range2d<size_t,size_t>(0, NROWS, 0, NCOLS),
			perc, auto_partitioner());
		
		// percolate into that position
		fluid[perc.getY()][perc.getX()] = true;
		std::cout << "(" << perc.getX() << "," << perc.getY() << ")" << std::endl;
		
	}
	
public:

	FluidPercolation(IntMatrix* image, BoolMatrix* fluid):
		_image(image), _fluid(fluid), minimum(MAXIMUM_INT) { }

	int getX() const {
		return x;
	}

	int getY() const {
		return y;
	}
	
	/**
 	 * Fluid percolation finder.
 	 */
	void operator()(const blocked_range2d<size_t,size_t>& range) {
		IntMatrix& image = *_image;
		BoolMatrix& fluid = *_fluid;
		
		const blocked_range<size_t>& rows = range.rows();
		const blocked_range<size_t>& cols = range.cols();
		
		for (size_t r = rows.begin(); r != rows.end(); ++r) {
			for (size_t c = cols.begin(); c != cols.end(); ++c) {

				if (image[r][c] <= minimum && fluid[r][c] == false && hasNeighbours(c, r)) {
					minimum = image[r][c];
					y = r;
					x = c;
				}

			}
		}
	}
	
	/**
	 * Splitting (TBB) constructor
	 */
	FluidPercolation(FluidPercolation& other, split):
		_image(other._image), _fluid(other._fluid), minimum(MAXIMUM_INT) { }
	
	/**
	 * Joiner (TBB).
	 */
	void join(const FluidPercolation& other) {
		if (other.minimum < minimum) {
			minimum = other.minimum;
			x = other.x;
			y = other.y;
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

int main(int argc, char** argv) {
	
	IntMatrix matrix;
	BoolMatrix result;
	
	// seed the random number generator.
	srand(time(0));
	
	// SERIAL: generate random values for the matrix, and
	// false for all values of the bool matrix.
	for (int row = 0; row < NROWS; ++row) {
		for (int col = 0; col < NCOLS; ++col) {
			matrix[row][col] = uniform(0.0f, 20.0f);
			result[row][col] = false;
		}
	}
	
	// random value for retain
	float cutoff = uniform(0.5f, 0.5f);
	
	// start up TBB
	task_scheduler_init init;
	
	// The fluid starts in the middle...
	result[NROWS >> 1][NCOLS >> 1] = true;
	
	// continually find the smallest element connected to the fluid
	// that is dry (false) and mark it as fluid (true).
	for (int it = 0; it < (cutoff * NROWS * NCOLS); ++it) {
		FluidPercolation::perform(matrix, result);
	}
	
	// exit the program.
	return 0;

}

