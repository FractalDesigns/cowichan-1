#include "cowichan.hpp"

/**
 * Returns a pseudorandom number ~ U[mean - range, mean + range].
 */
real uniform(real mean, real range) {
  return (rand() / (real)RAND_MAX) * (2.0f * range) - range + mean;
}

/*****************************************************************************/

/**
 * Prints out of memory message and exits.
 */
void out_of_memory() {
  std::cout << "--- Out of memory! ---";
  exit(1);
}

/**
 * Prints not enough points message and exits.
 */
void not_enough_points() {
  std::cout << "--- Not enough points! ---";
  exit(1);
}

/**
 * Print a rectangular matrix.
 */
#ifdef OUTPUT_DATA
template <typename T>
void Cowichan::print_rect_matrix(T* matrix)
{
  INT64 r, c;

  for (r = 0; r < nr; r++) {
    for (c = 0; c < nc; c++) {
      std::cout << MATRIX_RECT(matrix, r, c) << "\t";
    }
    std::cout << "\n";
  }
  std::cout << "\n";
}
#else
template <typename T>
void Cowichan::print_rect_matrix(T* /* matrix */) { }
#endif

/**
 * Print a rectangular matrix.
 */
#ifdef OUTPUT_DATA
template <typename T>
void Cowichan::print_square_matrix(T* matrix)
{
  INT64 r, c;

  for (r = 0; r < n; r++) {
    for (c = 0; c < n; c++) {
      std::cout << MATRIX_SQUARE(matrix, r, c) << "\t";
    }
    std::cout << "\n";
  }
  std::cout << "\n";
}
#else
template <typename T>
void Cowichan::print_square_matrix(T* /* matrix */) { }
#endif

/**
 * Print a vector.
 */
#ifdef OUTPUT_DATA
template <typename T>
void Cowichan::print_vector(T* vector)
{
  INT64 r;

  for (r = 0; r < n; r++) {
    std::cout << VECTOR(vector, r) << "\n";
  }
  std::cout << "\n";
}
#else
template <typename T>
void Cowichan::print_vector(T* /* vector */) { }
#endif

/**
 * Print a point vector.
 */
#ifdef OUTPUT_DATA
void Cowichan::print_vector(PointVector points)
{
  INT64 r;

  for (r = 0; r < n; r++) {
    std::cout << "[" << points[r].x << ", " << points[r].y << "]\n";
  }
  std::cout << "\n";
}
#else
void Cowichan::print_vector(PointVector /* points */) { }
#endif

/*****************************************************************************/

INT64 get_ticks ()
{
  INT64 count;
#if defined(WIN32)   // Windows
  if (! QueryPerformanceCounter((LARGE_INTEGER *) &count)) {
    count = GetTickCount (); // ms
  }
#else                // Linux
  tms tm;
  count = times (&tm);
#endif               // end of WIN32/Linux definitions
  return count;
}

INT64 get_freq ()
{
  INT64 freq;
#if defined(WIN32)   // Windows
  if (! QueryPerformanceFrequency((LARGE_INTEGER *) &freq)) {
    freq = 1000; // ms
  }
#else                // Linux
  freq = sysconf (_SC_CLK_TCK);
#endif               // end of WIN32/Linux definitions
  return freq;
}

/**
 * Does a sort of swap-out, printing progress.
 */
void timeInfo(INT64 *start, INT64 *end, std::string message) {
  *start = *end;
  *end = get_ticks();
  #ifdef TEST_TIME
    std::cout << message << ": ";
    print_elapsed_time(*start, *end);
    std::cout << std::endl;
  #endif
}

void print_elapsed_time (INT64 start, INT64 end)
{
  INT64 freq = get_freq ();
  std::cout.precision(5);
  std::cout << (((double) (end - start)) / ((double) freq)) << " seconds";
  std::cout.flush();
}

/*****************************************************************************/

const char* Cowichan::CHAIN = "chain";
const char* Cowichan::MANDEL = "mandel";
const char* Cowichan::RANDMAT = "randmat";
const char* Cowichan::HALF = "half";
const char* Cowichan::INVPERC = "invperc";
const char* Cowichan::THRESH = "thresh";
const char* Cowichan::LIFE = "life";
const char* Cowichan::WINNOW = "winnow";
const char* Cowichan::NORM = "norm";
const char* Cowichan::HULL = "hull";
const char* Cowichan::OUTER = "outer";
const char* Cowichan::GAUSS = "gauss";
const char* Cowichan::SOR = "sor";
const char* Cowichan::PRODUCT = "product";
const char* Cowichan::VECDIFF = "vecdiff";

void Cowichan::main (int argc, char* argv[], bool use_randmat, bool use_thresh)
{
  if ((argc == 1) || (strcmp (argv[1], CHAIN) == 0)) {
    chain (use_randmat, use_thresh);
  }
  else {
    INT64 start, end;

    if (strcmp (argv[1], MANDEL) == 0) {
      // set up
      nr = MANDEL_NR;
      nc = MANDEL_NC;
      mandelX0 = MANDEL_X0;
      mandelY0 = MANDEL_Y0;
      mandelDx = MANDEL_DX;
      mandelDy = MANDEL_DY;

      // initialize
      IntMatrix matrix = NULL;

      try {
        matrix = NEW_MATRIX_RECT(INT_TYPE);
      }
      catch (...) {out_of_memory();}

      // execute
      end = get_ticks ();
      mandel (matrix);
      timeInfo(&start, &end, MANDEL);
      print_rect_matrix<INT_TYPE> (matrix);

      // clean up
      delete [] matrix;
    }
    else if (strcmp (argv[1], RANDMAT) == 0) {
      // set up
      nr = RANDMAT_NR;
      nc = RANDMAT_NC;
      seed = RAND_SEED;

      // initialize
      IntMatrix matrix = NULL;

      try {
        matrix = NEW_MATRIX_RECT(INT_TYPE);
      }
      catch (...) {out_of_memory();}

      // execute
      end = get_ticks ();
      randmat (matrix);
      timeInfo(&start, &end, RANDMAT);
      print_rect_matrix<INT_TYPE> (matrix);

      // clean up
      delete [] matrix;
    }
    else if (strcmp (argv[1], HALF) == 0) {
      // set up
      nr = HALF_NR;
      nc = HALF_NC;
      srand(RAND_SEED);

      // initialize
      IntMatrix matrixIn = NULL;
      IntMatrix matrixOut = NULL;

      try {
        matrixIn = NEW_MATRIX_RECT(INT_TYPE);
        matrixOut = NEW_MATRIX_RECT(INT_TYPE);
      }
      catch (...) {out_of_memory();}

      INT64 r, c;

      for (r = 0; r < nr; r++) {
        for (c = 0; c < nc; c++) {
          MATRIX_RECT(matrixIn, r, c) = rand () % RAND_M;
        }
      }

      // execute
      end = get_ticks ();
      half (matrixIn, matrixOut);
      timeInfo(&start, &end, HALF);
      print_rect_matrix<INT_TYPE> (matrixOut);

      // clean up
      delete [] matrixIn;
      delete [] matrixOut;
    }
    else if (strcmp (argv[1], INVPERC) == 0) {
      // set up
      nr = INVPERC_NR;
      nc = INVPERC_NC;
      invpercNFill = INVPERC_NFILL;
      srand(RAND_SEED);

      // initialize
      IntMatrix matrix = NULL;
      BoolMatrix mask = NULL;

      try {
        matrix = NEW_MATRIX_RECT(INT_TYPE);
        mask = NEW_MATRIX_RECT(bool);
      }
      catch (...) {out_of_memory();}

      INT64 r, c;

      for (r = 0; r < nr; r++) {
        for (c = 0; c < nc; c++) {
          MATRIX_RECT(matrix, r, c) = rand () % RAND_M;
        }
      }
      
      // execute
      end = get_ticks ();
      invperc (matrix, mask);
      timeInfo(&start, &end, INVPERC);
      print_rect_matrix<bool> (mask);

      // clean up
      delete [] matrix;
      delete [] mask;
    }
    else if (strcmp (argv[1], THRESH) == 0) {
      // set up
      nr = THRESH_NR;
      nc = THRESH_NC;
      threshPercent = THRESH_PERCENT;
      srand(RAND_SEED);

      // initialize
      IntMatrix matrix = NULL;
      BoolMatrix mask = NULL;

      try {
        matrix = NEW_MATRIX_RECT(INT_TYPE);
        mask = NEW_MATRIX_RECT(bool);
      }
      catch (...) {out_of_memory();}

      INT64 r, c;

      for (r = 0; r < nr; r++) {
        for (c = 0; c < nc; c++) {
          MATRIX_RECT(matrix, r, c) = rand () % RAND_M;
        }
      }
      
      // execute
      end = get_ticks ();
      thresh (matrix, mask);
      timeInfo(&start, &end, THRESH);
      print_rect_matrix<bool> (mask);

      // clean up
      delete [] matrix;
      delete [] mask;
    }
    else if (strcmp (argv[1], LIFE) == 0) {
      // set up
      nr = LIFE_NR;
      nc = LIFE_NC;
      lifeIterations = LIFE_ITERATIONS;
      srand(RAND_SEED);

      // initialize
      BoolMatrix matrixIn = NULL;
      BoolMatrix matrixOut = NULL;

      try {
        matrixIn = NEW_MATRIX_RECT(bool);
        matrixOut = NEW_MATRIX_RECT(bool);
      }
      catch (...) {out_of_memory();}

      INT64 r, c;

      for (r = 0; r < nr; r++) {
        for (c = 0; c < nc; c++) {
          MATRIX_RECT(matrixIn, r, c) = (rand () % 2) == 0;
        }
      }
      
      // execute
      end = get_ticks ();
      life (matrixIn, matrixOut);
      timeInfo(&start, &end, LIFE);
      print_rect_matrix<bool> (matrixOut);

      // clean up
      delete [] matrixIn;
      delete [] matrixOut;
    }
    else if (strcmp (argv[1], WINNOW) == 0) {
      // set up
      nr = WINNOW_NR;
      nc = WINNOW_NC;
      n = WINNOW_N;
      srand(RAND_SEED);

      // initialize
      IntMatrix matrix = NULL;
      BoolMatrix mask = NULL;
      PointVector points = NULL;

      try {
        matrix = NEW_MATRIX_RECT(INT_TYPE);
        mask = NEW_MATRIX_RECT(bool);
        points = NEW_VECTOR(Point);
      }
      catch (...) {out_of_memory();}

      INT64 r, c;

      for (r = 0; r < nr; r++) {
        for (c = 0; c < nc; c++) {
          MATRIX_RECT(matrix, r, c) = rand () % RAND_M;
          MATRIX_RECT(mask, r, c) = (rand () % 2) == 0;
        }
      }
      
      // execute
      end = get_ticks ();
      winnow (matrix, mask, points);
      timeInfo(&start, &end, WINNOW);
      print_vector(points);

      // clean up
      delete [] matrix;
      delete [] mask;
      delete [] points;
    }
    else if (strcmp (argv[1], NORM) == 0) {
      // set up
      n = NORM_N;
      srand(RAND_SEED);

      // initialize
      PointVector pointsIn = NULL;
      PointVector pointsOut = NULL;

      try {
        pointsIn = NEW_VECTOR(Point);
        pointsOut = NEW_VECTOR(Point);
      }
      catch (...) {out_of_memory();}

      INT64 r;

      for (r = 0; r < n; r++) {
        VECTOR(pointsIn, r).x = uniform ((real)RAND_MEAN, (real)RAND_RANGE);
        VECTOR(pointsIn, r).y = uniform ((real)RAND_MEAN, (real)RAND_RANGE);
      }
      
      // execute
      end = get_ticks ();
      norm (pointsIn, pointsOut);
      timeInfo(&start, &end, NORM);
      print_vector(pointsOut);

      // clean up
      delete [] pointsIn;
      delete [] pointsOut;
    }
    else if (strcmp (argv[1], HULL) == 0) {
      // set up
      n = HULL_N;
      srand(RAND_SEED);

      // initialize
      PointVector pointsIn = NULL;
      PointVector pointsOut = NULL;

      try {
        pointsIn = NEW_VECTOR(Point);
        pointsOut = NEW_VECTOR(Point);
      }
      catch (...) {out_of_memory();}

      INT64 r;

      for (r = 0; r < n; r++) {
        VECTOR(pointsIn, r).x = uniform ((real)RAND_MEAN, (real)RAND_RANGE);
        VECTOR(pointsIn, r).y = uniform ((real)RAND_MEAN, (real)RAND_RANGE);
      }
      
      // execute
      end = get_ticks ();
      hull (pointsIn, pointsOut);
      timeInfo(&start, &end, HULL);
      print_vector(pointsOut);

      // clean up
      delete [] pointsIn;
      delete [] pointsOut;
    }
    else if (strcmp (argv[1], OUTER) == 0) {
      // set up
      n = OUTER_N;
      srand(RAND_SEED);

      // initialize
      PointVector points = NULL;
      Matrix matrix = NULL;
      Vector vector = NULL;

      try {
        points = NEW_VECTOR(Point);
        matrix = NEW_MATRIX_SQUARE(real);
        vector = NEW_VECTOR(real);
      }
      catch (...) {out_of_memory();}

      INT64 r;

      for (r = 0; r < n; r++) {
        VECTOR(points, r).x = uniform ((real)RAND_MEAN, (real)RAND_RANGE);
        VECTOR(points, r).y = uniform ((real)RAND_MEAN, (real)RAND_RANGE);
      }
      
      // execute
      end = get_ticks ();
      outer (points, matrix, vector);
      timeInfo(&start, &end, OUTER);
      print_square_matrix<real> (matrix);
      print_vector<real> (vector);

      // clean up
      delete [] points;
      delete [] matrix;
      delete [] vector;
    }
    else if (strcmp (argv[1], GAUSS) == 0) {
      // set up
      n = GAUSS_N;
      srand(RAND_SEED);

      // initialize
      Matrix matrix = NULL;
      Vector target = NULL;
      Vector solution = NULL;

      try {
        matrix = NEW_MATRIX_SQUARE(real);
        target = NEW_VECTOR(real);
        solution = NEW_VECTOR(real);
      }
      catch (...) {out_of_memory();}

      INT64 r, c;
      real value, maxValue = -1;

      // create symmetric, diagonally dominant matrix
      for (r = 0; r < n; r++) {
        for (c = 0; c < r; c++) {
          value = uniform ((real)RAND_MEAN, (real)RAND_RANGE);
          MATRIX_SQUARE(matrix, r, c) = MATRIX_SQUARE(matrix, c, r) = value;
          if (std::abs(value) > maxValue) {
            maxValue = std::abs(value);
          }
        }
        target[r] = uniform ((real)RAND_MEAN, (real)RAND_RANGE);
      }
      maxValue *= n;
      for (r = 0; r < n; r++) {
        DIAG(matrix, r) = maxValue;
      }
      
      // execute
      end = get_ticks ();
      gauss (matrix, target, solution);
      timeInfo(&start, &end, GAUSS);
      print_vector<real> (solution);

      // clean up
      delete [] matrix;
      delete [] target;
      delete [] solution;
    }
    else if (strcmp (argv[1], SOR) == 0) {
      // set up
      n = SOR_N;
      srand(RAND_SEED);

      // initialize
      Matrix matrix = NULL;
      Vector target = NULL;
      Vector solution = NULL;

      try {
        matrix = NEW_MATRIX_SQUARE(real);
        target = NEW_VECTOR(real);
        solution = NEW_VECTOR(real);
      }
      catch (...) {out_of_memory();}

      INT64 r, c;
      real value, maxValue = -1;

      // create symmetric, diagonally dominant matrix
      for (r = 0; r < n; r++) {
        for (c = 0; c < r; c++) {
          value = uniform ((real)RAND_MEAN, (real)RAND_RANGE);
          MATRIX_SQUARE(matrix, r, c) = MATRIX_SQUARE(matrix, c, r) = value;
          if (std::abs(value) > maxValue) {
            maxValue = std::abs(value);
          }
        }
        target[r] = uniform ((real)RAND_MEAN, (real)RAND_RANGE);
      }
      maxValue *= n;
      for (r = 0; r < n; r++) {
        DIAG(matrix, r) = maxValue;
      }
      
      // execute
      end = get_ticks ();
      sor (matrix, target, solution);
      timeInfo(&start, &end, SOR);
      print_vector<real> (solution);

      // clean up
      delete [] matrix;
      delete [] target;
      delete [] solution;
    }
    else if (strcmp (argv[1], PRODUCT) == 0) {
      // set up
      n = PRODUCT_N;
      srand(RAND_SEED);

      // initialize
      Matrix matrix = NULL;
      Vector candidate = NULL;
      Vector solution = NULL;

      try {
        matrix = NEW_MATRIX_SQUARE(real);
        candidate = NEW_VECTOR(real);
        solution = NEW_VECTOR(real);
      }
      catch (...) {out_of_memory();}

      INT64 r, c;
      for (r = 0; r < n; r++) {
        for (c = 0; c < r; c++) {
          MATRIX_SQUARE(matrix, r, c) = uniform ((real)RAND_MEAN,
              (real)RAND_RANGE);
        }
        candidate[r] = uniform ((real)RAND_MEAN, (real)RAND_RANGE);
      }
      
      // execute
      end = get_ticks ();
      product (matrix, candidate, solution);
      timeInfo(&start, &end, PRODUCT);
      print_vector<real> (solution);

      // clean up
      delete [] matrix;
      delete [] candidate;
      delete [] solution;
    }
    else if (strcmp (argv[1], VECDIFF) == 0) {
      // set up
      n = VECDIFF_N;
      srand(RAND_SEED);

      // initialize
      Vector actual = NULL;
      Vector computed = NULL;

      try {
        actual = NEW_VECTOR(real);
        computed = NEW_VECTOR(real);
      }
      catch (...) {out_of_memory();}

      INT64 r;
      for (r = 0; r < n; r++) {
        actual[r] = uniform ((real)RAND_MEAN, (real)RAND_RANGE);
        computed[r] = uniform ((real)RAND_MEAN, (real)RAND_RANGE);
      }
      
      // execute
      end = get_ticks ();
#ifdef OUTPUT_DATA
      real maxDiff = vecdiff (actual, computed);
#else
      vecdiff (actual, computed);
#endif
      timeInfo(&start, &end, VECDIFF);
#ifdef OUTPUT_DATA
      std::cout << maxDiff;
#endif

      // clean up
      delete [] actual;
      delete [] computed;
    }
  }
}
