#include "cowichan_serial.hpp"

void CowichanSerial::randmat (IntMatrix matrix)
{
  INT64 r, c;
  INT_TYPE v = seed % RAND_M;

  for (r = 0; r < nr; r++) {
    for (c = 0; c < nc; c++) {
      MATRIX_RECT(matrix, r, c) = v;
      v = (RANDMAT_A * v + RANDMAT_C) % RAND_M;
    }
  }
}

