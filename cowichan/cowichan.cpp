#include "cowichan.hpp"

/**
 * Returns a pseudorandom number ~ U[mean - range, mean + range].
 */
real uniform(real mean, real range) {
  return (rand() / (real)RAND_MAX) * (2.0f * range) - range + mean;
}

/*****************************************************************************/

/**
 * "pretty-print" a list of points.
 */
#define PRINT_BREAK 4
void print(PointList& points) {
  int b = 0;
  for (PointList::const_iterator it = points.begin(); it != points.end(); ++it) {
    if (b == 0) std::cout << std::endl << "\t";
    std::cout << "(" << it->x << "," << it->y << ")\t";
    b = (b + 1) % PRINT_BREAK;
  }
}

/**
 * Prints out of memory message and exits.
 */
void out_of_memory() {
  std::cout << "--- Out of memory! ---";
  exit(1);
}

/**
 * show a matrix result
 */
void Cowichan::printAxb(Matrix matrix, Vector answer, Vector vector) {
  std::cout.precision(5);
  for (int row = 0; row < n; ++row) {

    // print out the matrix
    std::cout << " [ ";
    for (int col = 0; col < n; ++col) {
      std::cout << MATRIX(matrix, row, col) << "\t";
    }
    
    // print out the answer
    std::cout << "] [ " << VECTOR(answer, row) << " ]\t";

    // print out the vector
    if (row == int(n / 2)) {
      std::cout << "= [ ";
    } else {
      std::cout << "  [ ";
    }
    std::cout << VECTOR(vector, row) << " ]" << std::endl;
    
  }
}

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
        matrix = NEW_MATRIX_RECT(uint);
      }
      catch (...) {out_of_memory();}

      // execute
      end = get_ticks ();
      mandel (matrix);
      timeInfo(&start, &end, MANDEL);

      // clean up
      delete [] matrix;
    }
    else if (strcmp (argv[1], RANDMAT) == 0) {
      // set up
      nr = RANDMAT_NR;
      nc = RANDMAT_NC;
      seed = RANDMAT_SEED;

      // initialize
      IntMatrix matrix = NULL;

      try {
        matrix = NEW_MATRIX_RECT(uint);
      }
      catch (...) {out_of_memory();}

      // execute
      end = get_ticks ();
      randmat (matrix);
      timeInfo(&start, &end, RANDMAT);

      // clean up
      delete [] matrix;
    }
    else if (strcmp (argv[1], HALF) == 0) {
      // set up
      nr = HALF_NR;
      nc = HALF_NC;
      srand(RANDMAT_SEED);

      // initialize
      IntMatrix matrixIn = NULL;
      IntMatrix matrixOut = NULL;

      try {
        matrixIn = NEW_MATRIX_RECT(uint);
        matrixOut = NEW_MATRIX_RECT(uint);
      }
      catch (...) {out_of_memory();}

      int r, c;

      for (r = 0; r < nr; r++) {
        for (c = 0; c < nc; c++) {
          MATRIX_RECT(matrixIn, r, c) = rand () % RANDMAT_M;
        }
      }

      // execute
      end = get_ticks ();
      half (matrixIn, matrixOut);
      timeInfo(&start, &end, HALF);

      // clean up
      delete [] matrixIn;
      delete [] matrixOut;
    }
    else if (strcmp (argv[1], INVPERC) == 0) {
      // set up
      nr = INVPERC_NR;
      nc = INVPERC_NC;
      invpercNFill = INVPERC_NFILL;
      srand(RANDMAT_SEED);

      // initialize
      IntMatrix matrix = NULL;
      BoolMatrix mask = NULL;

      try {
        matrix = NEW_MATRIX_RECT(uint);
        mask = NEW_MATRIX_RECT(bool);
      }
      catch (...) {out_of_memory();}

      int r, c;

      for (r = 0; r < nr; r++) {
        for (c = 0; c < nc; c++) {
          MATRIX_RECT(matrix, r, c) = rand () % RANDMAT_M;
        }
      }
      
      // execute
      end = get_ticks ();
      invperc (matrix, mask);
      timeInfo(&start, &end, INVPERC);

      // clean up
      delete [] matrix;
      delete [] mask;
    }
    else if (strcmp (argv[1], THRESH) == 0) {
      // set up
      nr = THRESH_NR;
      nc = THRESH_NC;
      threshPercent = THRESH_PERCENT;
      srand(RANDMAT_SEED);

      // initialize
      IntMatrix matrix = NULL;
      BoolMatrix mask = NULL;

      try {
        matrix = NEW_MATRIX_RECT(uint);
        mask = NEW_MATRIX_RECT(bool);
      }
      catch (...) {out_of_memory();}

      int r, c;

      for (r = 0; r < nr; r++) {
        for (c = 0; c < nc; c++) {
          MATRIX_RECT(matrix, r, c) = rand () % RANDMAT_M;
        }
      }
      
      // execute
      end = get_ticks ();
      thresh (matrix, mask);
      timeInfo(&start, &end, THRESH);

      // clean up
      delete [] matrix;
      delete [] mask;
    }
    else if (strcmp (argv[1], LIFE) == 0) {
      // set up
      nr = LIFE_NR;
      nc = LIFE_NC;
      lifeIterations = LIFE_ITERATIONS;
      srand(RANDMAT_SEED);

      // initialize
      BoolMatrix matrixIn = NULL;
      BoolMatrix matrixOut = NULL;

      try {
        matrixIn = NEW_MATRIX_RECT(bool);
        matrixOut = NEW_MATRIX_RECT(bool);
      }
      catch (...) {out_of_memory();}

      int r, c;

      for (r = 0; r < nr; r++) {
        for (c = 0; c < nc; c++) {
          MATRIX_RECT(matrixIn, r, c) = (rand () % 2) == 0;
        }
      }
      
      // execute
      end = get_ticks ();
      life (matrixIn, matrixOut);
      timeInfo(&start, &end, LIFE);

      // clean up
      delete [] matrixIn;
      delete [] matrixOut;
    }
    else if (strcmp (argv[1], WINNOW) == 0) {
      // set up
      nr = WINNOW_NR;
      nc = WINNOW_NC;
      n = WINNOW_N;
      srand(RANDMAT_SEED);

      // initialize
      IntMatrix matrix = NULL;
      BoolMatrix mask = NULL;
      PointVector points = NULL;

      try {
        matrix = NEW_MATRIX_RECT(uint);
        mask = NEW_MATRIX_RECT(bool);
        points = NEW_VECTOR(Point);
      }
      catch (...) {out_of_memory();}

      int r, c;

      for (r = 0; r < nr; r++) {
        for (c = 0; c < nc; c++) {
          MATRIX_RECT(matrix, r, c) = rand () % RANDMAT_M;
          MATRIX_RECT(mask, r, c) = (rand () % 2) == 0;
        }
      }
      
      // execute
      end = get_ticks ();
      winnow (matrix, mask, points);
      timeInfo(&start, &end, WINNOW);

      // clean up
      delete [] matrix;
      delete [] mask;
      delete [] points;
    }
    else if (strcmp (argv[1], NORM) == 0) {
      // set up
      n = NORM_N;
      srand(RANDMAT_SEED);

      // initialize
      PointVector pointsIn = NULL;
      PointVector pointsOut = NULL;

      try {
        pointsIn = NEW_VECTOR(Point);
        pointsOut = NEW_VECTOR(Point);
      }
      catch (...) {out_of_memory();}

      int r;

      for (r = 0; r < n; r++) {
        VECTOR(pointsIn, r).x = uniform ((real)0, (real)20);
        VECTOR(pointsIn, r).y = uniform ((real)0, (real)20);
      }
      
      // execute
      end = get_ticks ();
      norm (pointsIn, pointsOut);
      timeInfo(&start, &end, NORM);

      // clean up
      delete [] pointsIn;
      delete [] pointsOut;
    }
    else if (strcmp (argv[1], HULL) == 0) {
      // set up
      n = HULL_N;
      srand(RANDMAT_SEED);

      // initialize
      PointVector pointsIn = NULL;
      PointVector pointsOut = NULL;

      try {
        pointsIn = NEW_VECTOR(Point);
        pointsOut = NEW_VECTOR(Point);
      }
      catch (...) {out_of_memory();}

      int r;

      for (r = 0; r < n; r++) {
        VECTOR(pointsIn, r).x = uniform ((real)0, (real)20);
        VECTOR(pointsIn, r).y = uniform ((real)0, (real)20);
      }
      
      // execute
      end = get_ticks ();
      hull (pointsIn, pointsOut);
      timeInfo(&start, &end, HULL);

      // clean up
      delete [] pointsIn;
      delete [] pointsOut;
    }
    else if (strcmp (argv[1], OUTER) == 0) {
      outer (NULL, NULL, NULL);
    }
    else if (strcmp (argv[1], GAUSS) == 0) {
      gauss (NULL, NULL, NULL);
    }
    else if (strcmp (argv[1], SOR) == 0) {
      sor (NULL, NULL, NULL);
    }
    else if (strcmp (argv[1], PRODUCT) == 0) {
      product (NULL, NULL, NULL);
    }
    else if (strcmp (argv[1], VECDIFF) == 0) {
      vecdiff (NULL, NULL, NULL);
    }
  }
}

