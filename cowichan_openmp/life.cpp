#include "cowichan_openmp.hpp"

INT64 sumNeighbours(BoolMatrix first, INT64 r, INT64 c, INT64 nr, INT64 nc);

void no_cells_alive();

void CowichanOpenMP::life(BoolMatrix matrixIn, BoolMatrix matrixOut) {

  BoolMatrix first = matrixIn;
  BoolMatrix second = matrixOut;

  INT64 i, r, c;
  INT64 alive; // number of cells alive

	for (i = 0; i < lifeIterations; ++i) {

    alive = 0;

		// update CA simulation
#pragma omp parallel for schedule(static) reduction(+:alive)
    for (r = 0; r < nr; r++) {
#pragma omp parallel for schedule(static)
      for (c = 0; c < nc; c++) {
        
        INT64 peers = sumNeighbours (first, r, c, nr, nc);
        if (peers < 2 || peers > 3) {
          MATRIX_RECT(second, r, c) = false; // hunger/overcrowding
        } else if (peers == 3) {
          MATRIX_RECT(second, r, c) = true; // breeding
        } else {
          MATRIX_RECT(second, r, c) = MATRIX_RECT(first, r, c); // nothing
        }
        
        if (MATRIX_RECT(second, r, c)) {
          alive++;
        }
      }
    }

    if (alive == 0) {
      no_cells_alive();
    }

		// swap arrays (ping-pong approach)
    BoolMatrix temp = first;
    first = second;
    second = temp;

	}

  if (lifeIterations % 2 == 0) {
    // final result is in matrixIn - copy to matrixOut
#pragma omp parallel for schedule(static)
    for (r = 0; r < nr; r++) {
#pragma omp parallel for schedule(static)
      for (c = 0; c < nc; c++) {
        MATRIX_RECT(second, r, c) = MATRIX_RECT(first, r, c);
      }
    }
  }

}

/**
 * Calculate number of peers.
 */
INT64 sumNeighbours(BoolMatrix first, INT64 r, INT64 c, INT64 nr, INT64 nc) {

  INT64 peers = 0;

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

/**
 * Prints no cells alive message and exits.
 */
void no_cells_alive() {
  std::cout << "--- No cells alive! ---";
  exit(1);
}

