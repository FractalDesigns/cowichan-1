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

#define NELTS 100
#define NROWS 50
#define NCOLS 50

	void operator()(const blocked_range2d<size_t,size_t>& range) const {
		
		bool (*first)[BOARD_SIZE][BOARD_SIZE] = _first;
		bool (*second)[BOARD_SIZE][BOARD_SIZE] = _second;
		const blocked_range<size_t>& rows = range.rows();
		const blocked_range<size_t>& cols = range.cols();
		
		for (size_t y = rows.begin(); y != rows.end(); ++y) {
			for (size_t x = cols.begin(); x != cols.end(); ++x) {
				
				if (boolmx[y][x]) sortedInsertion(values, intmx[y][x]);
				
			}
		}
		
	}
	
	void join(ValueMasker& other) {
		sortedMerge(values, other.values);
	}


/*************/

	void operator()(const blocked_range<size_t>& range) const {
	
		for (size_t i = range.begin(); i != range.end(); ++i) {
			points.push_back(Point(values[i*2], values[(i*2)+1]));
		}
	
	}
	
	void join(PointSelector& other) {
		add other.points onto the back of points
	}


