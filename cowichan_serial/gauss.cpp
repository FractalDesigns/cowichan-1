#include "cowichan_serial.hpp"

/**
 * Matrices are required to be symmetric and diagonally dominant in order to
 * guarantee that there is a well-formed solution to the equation.
 */
void CowichanSerial::gauss (Matrix matrix, Vector target, Vector solution)
{
  int r, c, k;

  // forward elimination
  for (k = 0; k < n; k++) {
    // calculate pivots in k'th column
    for (r = k + 1; r < n; r++) {
      MATRIX_SQUARE(matrix, r, k) = MATRIX_SQUARE(matrix, r, k)
          / MATRIX_SQUARE(matrix, k, k);
    }
    // update elements below k'th row
    for (r = k + 1; r < n; r++) {
      for (c = k + 1; c < n; c++) {
        MATRIX_SQUARE(matrix, r, c) = MATRIX_SQUARE(matrix, r, c)
            - (MATRIX_SQUARE(matrix, r, k) * MATRIX_SQUARE(matrix, k, c));
      }
    }
    // update element of solution vector
    for (r = k + 1; r < n; r++) {
      target[r] = target[r] - (MATRIX_SQUARE(matrix, r, k) * target[k]);
    }
  }

  // back substitution
  for (k = (n - 1); k >= 0; k--) {
    solution[k] = target[k] / MATRIX_SQUARE(matrix, k, k);
    for (r = k - 1; r >= 0; r--) {
      target[r] = target[r] - (MATRIX_SQUARE(matrix, r, k) * solution[k]);
    }
  }

}

