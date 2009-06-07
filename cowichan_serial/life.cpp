#include "cowichan_serial.hpp"

void CowichanSerial::life(BoolMatrix matrixIn, BoolMatrix matrixOut) {

  first = matrixIn;
  second = matrixOut;

  int i, r, c;

	for (i = 0; i < lifeIterations; ++i) {

		// update CA simulation
    for (r = 0; r < nr; r++) {
      for (c = 0; c < nc; c++) {
        
        int peers = sumNeighbours (r, c);
        if (peers < 2 || peers > 3) {
          MATRIX_RECT(second, r, c) = false; // hunger/overcrowding
        } else if (peers == 3) {
          MATRIX_RECT(second, r, c) = true; // breeding
        } else {
          MATRIX_RECT(second, r, c) = MATRIX_RECT(first, r, c); // nothing
        }
        
      }
    }

		// swap arrays (ping-pong approach)
    BoolMatrix temp = first;
    first = second;
    second = temp;

	}

  if (lifeIterations % 2 == 0) {
    // final result is in matrixIn - copy to matrixOut
    for (r = 0; r < nr; r++) {
      for (c = 0; c < nc; c++) {
        MATRIX_RECT(second, r, c) = MATRIX_RECT(first, r, c);
      }
    }
  }

}

/**
 * Calculate number of peers.
 */
int CowichanSerial::sumNeighbours(int r, int c) {

  int peers = 0;

  // calculate possible neighbour positions
  bool ll = (c > 0);
  bool rr = (c < (nc - 1));
  bool uu = (r > 0);
  bool dd = (r < (nr - 1));

  // calculate no. of neighbours
  if (ll &&       MATRIX_RECT(first, r,     c - 1)) ++peers;
  if (ll && uu && MATRIX_RECT(first, r - 1, c - 1)) ++peers;
  if (uu &&       MATRIX_RECT(first, r - 1, c    )) ++peers;
  if (rr && uu && MATRIX_RECT(first, r - 1, c + 1)) ++peers;
  if (rr &&       MATRIX_RECT(first, r,     c + 1)) ++peers;
  if (rr && dd && MATRIX_RECT(first, r + 1, c + 1)) ++peers;
  if (dd &&       MATRIX_RECT(first, r + 1, c    )) ++peers;
  if (ll && dd && MATRIX_RECT(first, r + 1, c - 1)) ++peers;

  return peers;

}

