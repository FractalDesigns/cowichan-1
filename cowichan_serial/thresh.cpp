#include "cowichan_serial.hpp"

void CowichanSerial::thresh(IntMatrix matrix, BoolMatrix mask) {

  uint* hist = NULL; // histogram
  long long i;
  int r, c;
  uint vMax; // max value in matrix
  long long retain; // selection

  // find max value in matrix
  vMax = 0;
  for (r = 0; r < nr; r++) {
    for (c = 0; c < nc; c++) {
      if (vMax < MATRIX_RECT(matrix, r, c)){
        vMax = MATRIX_RECT(matrix, r, c);
      }
    }
  }

  // initialize histogram
  try {
    hist = new uint[vMax + 1];
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
  retain = (int)(threshPercent * nc * nr);
  for (i = vMax; ((i >= 0) && (retain > 0)); i--) {
    retain -= hist[i];
  }
  retain = i;

  // threshold
  for (r = 0; r < nr; r++) {
    for (c = 0; c < nc; c++) {
      MATRIX_RECT(mask, r, c) = MATRIX_RECT(matrix, r, c) > retain;
    }
  }

  delete [] hist;

}

