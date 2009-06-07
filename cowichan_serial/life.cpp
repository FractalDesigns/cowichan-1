#include "cowichan_serial.hpp"

int sumNeighbours(BoolMatrix first, int r, int c, int nr, int nc);

void CowichanSerial::life(BoolMatrix matrixIn, BoolMatrix matrixOut) {

  BoolMatrix first = matrixIn;
  BoolMatrix second = matrixOut;

  int i, r, c;

	for (i = 0; i < lifeIterations; ++i) {

		// update CA simulation
    for (r = 0; r < nr; r++) {
      for (c = 0; c < nc; c++) {
        
        int peers = sumNeighbours (first, r, c, nr, nc);
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
int sumNeighbours(BoolMatrix first, int r, int c, int nr, int nc) {

  int peers = 0;

  // calculate possible neighbour positions
  bool ll = (c > 0);
  bool rr = (c < (nc - 1));
  bool uu = (r > 0);
  bool dd = (r < (nr - 1));

  // calculate no. of neighbours
  if (ll &&       MATRIX_RECT_NC(first, r    , c - 1, nc)) ++peers;
  if (ll && uu && MATRIX_RECT_NC(first, r - 1, c - 1, nc)) ++peers;
  if (uu &&       MATRIX_RECT_NC(first, r - 1, c    , nc)) ++peers;
  if (rr && uu && MATRIX_RECT_NC(first, r - 1, c + 1, nc)) ++peers;
  if (rr &&       MATRIX_RECT_NC(first, r    , c + 1, nc)) ++peers;
  if (rr && dd && MATRIX_RECT_NC(first, r + 1, c + 1, nc)) ++peers;
  if (dd &&       MATRIX_RECT_NC(first, r + 1, c    , nc)) ++peers;
  if (ll && dd && MATRIX_RECT_NC(first, r + 1, c - 1, nc)) ++peers;

  return peers;

}

