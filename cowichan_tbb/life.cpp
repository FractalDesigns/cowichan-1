#include "cowichan_tbb.hpp"

/**
 * This class does the game of life, and facilitates a ping-pong memory model.
 */
class GameOfLife {

private:

  BoolMatrix _first, _second;
  index_t nr, nc;

  /**
   * Calculate number of peers.
   */
  index_t sumNeighbours(index_t x, index_t y) const {

    index_t peers = 0;

    // calculate possible neighbour positions
    bool l = (x > 0);
    bool r = (x < (nc - 1));
    bool u = (y > 0);
    bool d = (y < (nr - 1));    

    // calculate no. of neighbours
    if (l &&       MATRIX_RECT(_first, y    , x - 1)) ++peers;
    if (l && u &&  MATRIX_RECT(_first, y - 1, x - 1)) ++peers;
    if (u &&       MATRIX_RECT(_first, y - 1, x    )) ++peers;
    if (r && u &&  MATRIX_RECT(_first, y - 1, x + 1)) ++peers;
    if (r &&       MATRIX_RECT(_first, y    , x + 1)) ++peers;
    if (r && d &&  MATRIX_RECT(_first, y + 1, x + 1)) ++peers;
    if (d &&       MATRIX_RECT(_first, y + 1, x    )) ++peers;
    if (l && d &&  MATRIX_RECT(_first, y + 1, x - 1)) ++peers;
    
    return peers;
    
  }

public:
  
  GameOfLife(BoolMatrix first, BoolMatrix second, index_t nr, index_t nc):
    _first(first), _second(second), nr(nr), nc(nc) { }

  void swap() {
    BoolMatrix temp = _first;
    _first = _second;
    _second = temp;
  }

  /**
   * Performs the game of life operation over the given range.
   */
  void operator()(const Range2D& range) const {
    
    BoolMatrix first = _first;
    BoolMatrix second = _second;
    
    const Range& rows = range.rows();
    const Range& cols = range.cols();
    
    for (index_t y = rows.begin(); y != rows.end(); ++y) {
      for (index_t x = cols.begin(); x != cols.end(); ++x) {
        
        index_t peers = sumNeighbours(x, y);
        if (peers < 2 || peers > 3) {
          MATRIX_RECT(second, y, x) = false; // hunger/overcrowding
        } else if (peers == 3) {
          MATRIX_RECT(second, y, x) = true; // breeding
        } else {
          MATRIX_RECT(second, y, x) = MATRIX_RECT(first, y, x); // nothing
        }
        
      }
    }
    
  }
};

/*****************************************************************************/

void CowichanTBB::life(BoolMatrix input, BoolMatrix output) {
  GameOfLife game(input, output, nr, nc);

  for (index_t i = 0; i < LIFE_ITERATIONS; ++i) {

    // update CA simulation
    parallel_for(Range2D(0, nr, 0, nc), game, auto_partitioner());

    // swap arrays (ping-pong approach)
    game.swap();

  }

  // final result is in input - copy to output
  if (LIFE_ITERATIONS % 2 == 0) {
    for (index_t r = 0; r < nr; r++) {
      for (index_t c = 0; c < nc; c++) {
        MATRIX_RECT(output, r, c) = MATRIX_RECT(input, r, c);
      }
    }
  }
}

