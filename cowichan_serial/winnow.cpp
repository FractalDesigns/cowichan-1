#include "cowichan_serial.hpp"

int mask_count(BoolMatrix mask, int nr, int nc);

void not_enough_points();

void CowichanSerial::winnow(IntMatrix matrix, BoolMatrix mask,
    PointVector points) {

  int r, c;
  int len; // number of points
  int stride; // selection stride
  int i, j;

  // fill temporary vector
  len = mask_count (mask, nr, nc);

  if (len < n) {
    not_enough_points();
  }

  WeightedPointVector weightedPoints = NULL;
  try {
    weightedPoints = NEW_VECTOR_SZ(WeightedPoint, len);
  }
  catch (...) {out_of_memory();}

  i = 0;
  for (r = 0; r < nr; r++) {
    for (c = 0; c < nc; c++) {
      if (MATRIX_RECT(mask, r, c)) {
        weightedPoints[i++] = WeightedPoint((real)c, (real)r,
            MATRIX_RECT(matrix, r, c));
      }
    }
  }

  // sort
  std::sort(weightedPoints, &weightedPoints[len]);

  // copy over points
  stride = len / n;

  for (i = n - 1, j = len - 1; i >= 0; i--, j -= stride) {
    points[i] =  weightedPoints[j].point;
  }

}

/**
 * Count the number of set cells in the mask.
 * @param mask boolean mask.
 * @param nr number of rows.
 * @param nc number of columns.
 */
int mask_count(BoolMatrix mask, int nr, int nc) {

  int r, c, sum = 0;

  for (r = 0; r < nr; r++) {
    for (c = 0; c < nc; c++) {
      if (MATRIX_RECT_NC(mask, r, c, nc)) {
        sum++;
      }
    }
  }

  return sum;
}

/**
 * Prints not enough points message and exits.
 */
void not_enough_points() {
  std::cout << "--- Not enough points! ---";
  exit(1);
}

