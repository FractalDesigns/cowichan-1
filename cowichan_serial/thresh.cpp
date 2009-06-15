#include "cowichan_serial.hpp"

void CowichanSerial::thresh(IntMatrix matrix, BoolMatrix mask) {

  IntVector hist = NULL; // histogram
  INT64 i;
  INT64 r, c;
  INT_TYPE vMax; // max value in matrix
  INT64 retain; // selection

  // find max value in matrix
  vMax = 0;
  for (r = 0; r < nr; r++) {
    for (c = 0; c < nc; c++) {
      if (vMax < MATRIX_RECT(matrix, r, c)) {
        vMax = MATRIX_RECT(matrix, r, c);
      }
    }
  }

  // initialize histogram
  try {
    hist = NEW_VECTOR_SZ(INT_TYPE, vMax + 1);
  }
  catch (...) {out_of_memory();}

  for (i = 0; i <= vMax; i++) {
    hist[i] = 0;
  }

  // count
  for (r = 0; r < nr; r++) {
    for (c = 0; c < nc; c++) {
      hist[MATRIX_RECT(matrix, r, c)]++;
    }
  }

  // include
  retain = (INT64)(threshPercent * nc * nr);
  for (i = vMax; ((i >= 0) && (retain > 0)); i--) {
    retain -= hist[i];
  }
  retain = i;

  delete [] hist;

  // threshold
  for (r = 0; r < nr; r++) {
    for (c = 0; c < nc; c++) {
      MATRIX_RECT(mask, r, c) = MATRIX_RECT(matrix, r, c) > retain;
    }
  }

}

