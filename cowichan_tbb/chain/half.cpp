#include "cowichan.hpp"

/**
 * This class does a halving shuffle.
 */
class Shuffle {
private:

	size_t xBreak, yBreak;

public:
	
	IntMatrix _first, _second;

	Shuffle(IntMatrix input, IntMatrix output):
		_first(input), _second(output),
		xBreak((Cowichan::NCOLS+1) / 2),
		yBreak((Cowichan::NROWS+1) / 2)
		{ }

	/**
	 * Performs the halving shuffle over the given range.
	 */
	void operator()(const Range2D& range) const {
		
		IntMatrix first = _first;
		IntMatrix second = _second;
		const Range& rows = range.rows();
		const Range& cols = range.cols();
		
		size_t xSrc, ySrc;
		for (size_t y = rows.begin(); y != rows.end(); ++y) {
			for (size_t x = cols.begin(); x != cols.end(); ++x) {
				
				// calculate unswapped x co-ordinate.
				if (x < xBreak) {
					// odd columns
					xSrc = x * 2;
				} else {
					// even columns
					xSrc = (x - xBreak) * 2 + 1;
				}
				
				// calculate unswapped y co-ordinate.
				if (y < yBreak) {
					// odd rows
					ySrc = y * 2;
				} else {
					// even columns
					ySrc = (y - yBreak) * 2 + 1;
				}
							
				// assign new values in the output matrix.
				MATRIX_RECT(second, y, x) = MATRIX_RECT(first, ySrc, xSrc);
				
			}
		}
		
	}
};

/*****************************************************************************/

void Cowichan::half(IntMatrix matrixIn, IntMatrix* matrixOut) {

	// allocate the output matrix.
	*matrixOut = NEW_MATRIX_RECT(uint);

	// perform the halving shuffle.
	Shuffle shuffle(matrixIn, *matrixOut);
	parallel_for(Range2D(0, NROWS, 0, NCOLS), shuffle,
		auto_partitioner());
		
}

