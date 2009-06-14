#include "cowichan_serial.hpp"

void CowichanSerial::chain(bool use_randmat, bool use_thresh)
{
  INT64 start, end;

  // STEP 1: mandel or randmat

  // set up
  nr = CHAIN_NR;
  nc = CHAIN_NC;

  // initialize
  IntMatrix matrix1 = NULL;

  try {
    matrix1 = NEW_MATRIX_RECT(INT_TYPE);
  }
  catch (...) {out_of_memory();}

  if (use_randmat) {
    // set up
    seed = RAND_SEED;

    // execute
    end = get_ticks ();
    randmat (matrix1);
    timeInfo(&start, &end, RANDMAT);
    print_rect_matrix<INT_TYPE> (matrix1);
  }
  else {
    // set up
    mandelX0 = MANDEL_X0;
    mandelY0 = MANDEL_Y0;
    mandelDx = MANDEL_DX;
    mandelDy = MANDEL_DY;

    // execute
    end = get_ticks ();
    mandel (matrix1);
    timeInfo(&start, &end, MANDEL);
    print_rect_matrix<INT_TYPE> (matrix1);
  }

  // STEP 2: half

  // initialize
  IntMatrix matrix2 = NULL;

  try {
    matrix2 = NEW_MATRIX_RECT(INT_TYPE);
  }
  catch (...) {out_of_memory();}

  // execute
  end = get_ticks ();
  half (matrix1, matrix2);
  timeInfo(&start, &end, HALF);
  print_rect_matrix<INT_TYPE> (matrix2);

  // clean up
  delete [] matrix1;

  // STEP 3: invperc or thresh

  // initialize
  BoolMatrix mask1 = NULL;

  try {
    mask1 = NEW_MATRIX_RECT(bool);
  }
  catch (...) {out_of_memory();}

  if (use_thresh) {
    // set up
    threshPercent = THRESH_PERCENT;
    
    // execute
    end = get_ticks ();
    thresh (matrix2, mask1);
    timeInfo(&start, &end, THRESH);
    print_rect_matrix<bool> (mask1);
  }
  else {
    invpercNFill = INVPERC_NFILL;

    // execute
    end = get_ticks ();
    invperc (matrix2, mask1);
    timeInfo(&start, &end, INVPERC);
    print_rect_matrix<bool> (mask1);
  }

  // STEP 4: life

  // set up
  lifeIterations = LIFE_ITERATIONS;

  // initialize
  BoolMatrix mask2 = NULL;

  try {
    mask2 = NEW_MATRIX_RECT(bool);
  }
  catch (...) {out_of_memory();}

  // execute
  end = get_ticks ();
  life (mask1, mask2);
  timeInfo(&start, &end, LIFE);
  print_rect_matrix<bool> (mask2);

  // clean up
  delete [] mask1;

  // STEP 5: winnow

  // set up
  n = CHAIN_N;

  // initialize
  PointVector vector1 = NULL;

  try {
    vector1 = NEW_VECTOR(Point);
  }
  catch (...) {out_of_memory();}

  // execute
  end = get_ticks ();
  winnow (matrix2, mask2, vector1);
  timeInfo(&start, &end, WINNOW);
  print_vector(vector1);

  // clean up
  delete [] matrix2;
  delete [] mask2;

  // STEP 6: norm

  // initialize
  PointVector vector2 = NULL;

  try {
    vector2 = NEW_VECTOR(Point);
  }
  catch (...) {out_of_memory();}

  // execute
  end = get_ticks ();
  norm (vector1, vector2);
  timeInfo(&start, &end, NORM);
  print_vector(vector2);

  // STEP 7: hull

  // execute
  end = get_ticks ();
  hull (vector2, vector1);
  timeInfo(&start, &end, HULL);
  print_vector(vector1);

  // clean up
  delete [] vector2;

  // STEP 8: outer

  // initialize
  Matrix matrix3 = NULL;
  Vector vector3 = NULL;

  try {
    matrix3 = NEW_MATRIX_SQUARE(real);
    vector3 = NEW_VECTOR(real);
  }
  catch (...) {out_of_memory();}

  // execute
  end = get_ticks ();
  outer (vector1, matrix3, vector3);
  timeInfo(&start, &end, OUTER);
  print_square_matrix<real> (matrix3);
  print_vector<real> (vector3);

  // clean up
  delete [] vector1;

  // STEP 9: gauss

  // initialize
  Vector vector4 = NULL;

  try {
    vector4 = NEW_VECTOR(real);
  }
  catch (...) {out_of_memory();}

  // execute
  end = get_ticks ();
  gauss (matrix3, vector3, vector4);
  timeInfo(&start, &end, GAUSS);
  print_vector<real> (vector4);

  // STEP 10: sor

  // initialize
  Vector vector5 = NULL;

  try {
    vector5 = NEW_VECTOR(real);
  }
  catch (...) {out_of_memory();}

  // execute
  end = get_ticks ();
  sor (matrix3, vector3, vector5);
  timeInfo(&start, &end, SOR);
  print_vector<real> (vector5);

  // STEP 11: product (for gauss)

  // execute
  end = get_ticks ();
  product (matrix3, vector4, vector3);
  timeInfo(&start, &end, PRODUCT);
  print_vector<real> (vector3);

  // STEP 12: product (for sor)

  // execute
  end = get_ticks ();
  product (matrix3, vector5, vector4);
  timeInfo(&start, &end, PRODUCT);
  print_vector<real> (vector4);

  // clean up
  delete [] matrix3;
  delete [] vector5;

  // STEP 13: vecdiff

  // execute
  end = get_ticks ();
#ifdef OUTPUT_DATA
  real maxDiff = vecdiff (vector3, vector4);
#else
  vecdiff (vector3, vector4);
#endif
  timeInfo(&start, &end, VECDIFF);
#ifdef OUTPUT_DATA
  std::cout << maxDiff;
#endif

  // clean up
  delete [] vector3;
  delete [] vector4;
}

