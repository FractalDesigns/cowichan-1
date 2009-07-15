#include "cowichan_tbb.hpp"

/**
 * Performs the maximum computation.
 */
class MaxReducer {
private:

  IntMatrix _image;
  INT_TYPE _max;
  const index_t nc;

public:

  /**
   * Initialise max with the lowest possible value.
   */
  MaxReducer(IntMatrix image, index_t nc):
    _image(image), _max(MINIMUM_INT), nc(nc) { }

  INT_TYPE getMaximum() const {
    return _max;
  }

  /**
   * Calculates the maximum value over the given range.
   */
  void operator()(const Range2D& range) {

    IntMatrix image = _image;

    const Range& rows = range.rows();
    const Range& cols = range.cols();
    
    for (index_t y = rows.begin(); y != rows.end(); ++y) {
      for (index_t x = cols.begin(); x != cols.end(); ++x) {
        if (_max < MATRIX_RECT(image, y, x)) {
          _max = MATRIX_RECT(image, y, x);
        }
      }
    }
    
  }
  
  /**
   * Splitting (TBB) constructor
   */
  MaxReducer(MaxReducer& other, split) : _image(other._image),
      _max(MINIMUM_INT), nc(other.nc) { }

  /**
   * Joiner (TBB).
   */
  void join(const MaxReducer& other) {
    if (_max < other._max) {
      _max = other._max;
    }
  }
  
};

/*
 * This class calculates the histogram of a matrix.
 */
class Histogram {
  
  IntMatrix _image;
  index_t* histogram;
  const index_t bins;
  const index_t nr, nc;

public:

  Histogram(IntMatrix image, INT_TYPE maxValue, index_t nr, index_t nc)
      : _image(image), bins(maxValue), nr(nr), nc(nc)
  {
    histogram = new index_t[maxValue + 1];

    for (index_t i = 0; i <= (index_t)maxValue; ++i) {
      histogram[i] = 0;
    }
  }
  
  index_t getValue(real cutoff) const {
    index_t i;
    index_t retain = (index_t)(cutoff * nc * nr);

    for (i = bins; (i >= 0) && (retain > 0); --i) {
      retain -= histogram[i];
    }

    return i;
  }
  
  /**
    * Histogram calculation.
    */
  void operator()(const Range2D& range) const {
    IntMatrix image = _image;
    index_t* hist = histogram;
    
    const Range& rows = range.rows();
    const Range& cols = range.cols();
    
    for (index_t y = rows.begin(); y != rows.end(); ++y) {
      for (index_t x = cols.begin(); x != cols.end(); ++x) {
        hist[MATRIX_RECT(image, y, x)]++;
      }
    }
  }
  
  /**
   * Splitting (TBB) constructor
   */
  Histogram(Histogram& other, split): _image(other._image), bins(other.bins),
      nr(other.nr), nc(other.nc)
  {
    histogram = new index_t[bins + 1];

    for (index_t i = 0; i <= bins; ++i) {
      histogram[i] = 0;
    }    
  }
  
  /**
   * Joiner (TBB).
   */
  void join(const Histogram& other) {
    // SERIAL... can we speed up by parallel_for here, too?
    for (index_t i = 0; i < bins; ++i) {
      histogram[i] += other.histogram[i];
    }
  }
  
};

/**
 * This class takes an Integer array to a boolean array based on a cut-off value.
 */
class Threshold {

  IntMatrix _image;
  BoolMatrix _result;
  const index_t retain;
  const index_t nc;

public:

  void operator()(const Range2D& range) const {
    IntMatrix image = _image;
    BoolMatrix result = _result;
    
    const Range& rows = range.rows();
    const Range& cols = range.cols();
    
    for (index_t y = rows.begin(); y != rows.end(); ++y) {
      for (index_t x = cols.begin(); x != cols.end(); ++x) {
        MATRIX_RECT(result, y, x) =
            ((index_t)MATRIX_RECT(image, y, x)) > retain;
      }
    }
  }
  
  Threshold(IntMatrix image, index_t retain, BoolMatrix result, index_t nc):
    _image(image), _result(result), retain(retain), nc(nc) { }

};

/*****************************************************************************/

void CowichanTBB::thresh(IntMatrix matrix, BoolMatrix mask) {
  
  // get the maximum value in the matrix (need 0-that number of bins)
  MaxReducer reducer(matrix, nc);
  parallel_reduce(Range2D(0, nr, 0, nc), reducer, auto_partitioner());
  INT_TYPE max = reducer.getMaximum();
  
  // compute the histogram to get a thresholding value
  Histogram hist(matrix, max, nr, nc);
  parallel_reduce(Range2D(0, nr, 0, nc), hist, auto_partitioner());
  
  // perform the thresholding opearation
  Threshold thresh(matrix, hist.getValue(THRESH_PERCENT), mask, nc);
  parallel_for(Range2D(0, nr, 0, nc), thresh, auto_partitioner());

}

