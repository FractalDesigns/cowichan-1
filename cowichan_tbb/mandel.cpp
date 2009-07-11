#include "cowichan_tbb.hpp"

class Mandelbrot {

  IntMatrix _matrix;    // to store the result.
  INT64 nr, nc;         // number of rows, columns

  real dX, dY;          // co-ordinate -> complex plane mapping coeff.
  real baseX, baseY;    // where to start the mandelbrot set

private:

  /**
    * Performs the mandelbrot set calculation.
    */
  INT_TYPE mandelCalc(real x, real y) const {

    real r = 0.0, i = 0.0;
    real rs = 0.0, is = 0.0;
    INT_TYPE numIterations = 0;    
    do {
    
      // calculate the complex value according to the mandelbrot set specs.
      i = (((real)2.0) * r * i) + x;
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
  static void exec(IntMatrix matrix, INT64 nr, INT64 nc, real x, real y,
      real width, real height) {
    
    Mandelbrot mandel(matrix, nr, nc, x, y, width, height);
    parallel_for(Range2D(0, (size_t)nr, 0, (size_t)nc), mandel,
      auto_partitioner());
    
  }
  
  
public:

  Mandelbrot(IntMatrix matrix, INT64 nr, INT64 nc, real x, real y, real width,
      real height): _matrix(matrix), nr(nr), nc(nc), baseX(x), baseY(y) {
    
    dX = width / (nc - 1);
    dY = height / (nr - 1);
      
  }

  /**
    * Calculates a given portion of the current mandelbrot set "window".
    */
  void operator()(const Range2D& range) const {

    IntMatrix matrix = _matrix;

    const Range& rows = range.rows();
    const Range& cols = range.cols();
    
    for (INT64 y = rows.begin(); y != rows.end(); ++y) {
      for (INT64 x = cols.begin(); x != cols.end(); ++x) {
        MATRIX_RECT(matrix, y, x) = mandelCalc(baseX + (x * dX), baseY + (y * dY));
      }
    }
    
  }
  
};

/*****************************************************************************/

void CowichanTBB::mandel(IntMatrix matrix)
{
  Mandelbrot::exec(matrix, nr, nc, mandelX0, mandelY0, mandelDx, mandelDy);
}

