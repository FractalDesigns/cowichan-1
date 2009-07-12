#include "cowichan_serial.hpp"

void CowichanSerial::randmat (IntMatrix matrix)
{
  index_t r, c;
  INT_TYPE v = seed % RAND_M;

  for (c = 0; c < nc; c++) {
    for (r = 0; r < nr; r++) {
      MATRIX_RECT(matrix, r, c) = v;
      v = (RANDMAT_A * v + RANDMAT_C) % RAND_M;
    }
  }
}

