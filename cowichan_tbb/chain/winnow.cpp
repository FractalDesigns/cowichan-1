/*
	This module converts a matrix of integer values to a vector of points, rep-
	resented as x and y coordinates. Its inputs are:

	matrix: an integer matrix, whose values are used as masses.
	mask: a Boolean matrix showing which points are eligible for consideration.
	nrows, ncols: the number of rows and columns in the matrix.
	nelts: the number of points to select.

	Its output is:

	points: a vector of (x, y) points.

	Each location where mask is true becomes a candidate point, with a weight
	equal to the integer value in matrix at that location and x and y coordinates
	equal to its row and column indices. These candidate points are then sorted
	into increasing order by weight, and nelts evenly-spaced points selected to
	create the result vector.
*/
#include "cowichan.hpp"

class ValueSelector {
private:

	IntMatrix _candidates;
	BoolMatrix _mask;
	
	WeightedPointList values;

public:

	/**
	 * Accessor for the weighted point list.
	 */
	WeightedPointList& getValues() {
		return values;
	}

	/**
	 * Standard constructor
	 */
	ValueSelector(IntMatrix candidates, BoolMatrix mask):
		_candidates(candidates), _mask(mask) { }

	/**
	 * Add candidate values to the value list based on mask (TBB).
	 */
	void operator()(const Range2D& range) {
		
		// bring pointers into cache
		const BoolMatrix mask = _mask;
		const IntMatrix candidates = _candidates;
		const Range& rows = range.rows();
		const Range& cols = range.cols();
		
		// add candidate values marked as good by the mask.
		for (size_t y = rows.begin(); y != rows.end(); ++y) {
			for (size_t x = cols.begin(); x != cols.end(); ++x) {
				
				if (MATRIX_RECT(mask, y, x)) {
					add(WeightedPoint(x, y, MATRIX_RECT(candidates, y, x)));
				}
				
			}
		}
		
	}
	
	/**
	 * Splitting (TBB) constructor.
	 */
	ValueSelector(ValueSelector& other, split):
		_candidates(other._candidates), _mask(other._mask) { }
	
	/**
	 * Joiner (TBB).
	 */
	void join(const ValueSelector& other) {
		add(other.values);
	}
	
private:

	/**
	 * Inserts a value into the values list, in correct order (given that the
	 * list is already sorted). It's O(log n)!
	 */
	void add(const WeightedPoint& newValue) {
		WeightedPointList::iterator pos = std::lower_bound(values.begin(), values.end(), newValue);
		values.insert(pos, newValue);
	}
	
	/**
	 * Merges another already-sorted list with this ValueSelector's list.
	 */
	void add(const WeightedPointList& other) {

		WeightedPointList replacement;
		WeightedPointList::const_iterator i, j;		

		// reserve space in the replacement vector for the current and new values		
		replacement.reserve(values.size() + other.size());

		// iterate over both collections, adding the smaller element to the new
		// real list every time we must make a comparison. In the end, all of
		// the elements are added to replacement.
		for (i = values.begin(), j = other.begin();;) {
		
			if (i == values.end() && j == other.end()) return;
			if (i == values.end()) {
				replacement.push_back(*j); ++j;
			} else if (j == other.end()) {
				replacement.push_back(*i); ++i;
			} else {
				if (*i < *j) {
					replacement.push_back(*i); ++i;
				} else {
					replacement.push_back(*j); ++j;
				}
			}
		
		}
		
		// replace the old list with the new one.
		values = replacement;
		
	}
	
};

/*****************************************************************************/

class ExtractPoints {
public:

	static void exec(IntMatrix matrix, BoolMatrix mask, PointList* points) {
		
		// extract points from the matrix
		ValueSelector vs(matrix, mask);
		parallel_reduce(
			Range2D(0, Cowichan::NROWS, 0, Cowichan::NCOLS),
			vs, auto_partitioner());
			
		// copy over points until we have NELTS points.
		size_t stride = vs.getValues().size() / Cowichan::NELTS; // implicit floor
		for (size_t idx = 0; points->size() < Cowichan::NELTS; idx += stride) {
			points->push_back(vs.getValues()[idx].point);
		}
		
	}

};

/*****************************************************************************/

void Cowichan::winnow(IntMatrix matrix, BoolMatrix mask, PointList** points) {

	// get a list of points!
	*points = new PointList();
	ExtractPoints::exec(matrix, mask, *points);
	
}

