#include "cowichan.hpp"

#ifdef GRAPHICS
	#include "sdl.hpp"
#endif

/**
 * This class does the game of life, and facilitates a ping-pong memory model.
 */
class GameOfLife {

	int sumNeighbours(int x, int y) const {

		int peers = 0;

		// calculate possible neighbour positions
		bool l = (x > 0);
		bool r = (x < (Cowichan::NELTS - 1));
		bool u = (y > 0);
		bool d = (y < (Cowichan::NELTS - 1));		

		// calculate no. of neighbours
		if (l && 		MATRIX(_first, y,  x-1))	++peers;
		if (l && u &&	MATRIX(_first, y-1,x-1)) 	++peers;
		if (u && 		MATRIX(_first, y-1,x  ))	++peers;
		if (r && u &&	MATRIX(_first, y-1,x+1)) 	++peers;
		if (r && 		MATRIX(_first, y,  x+1))	++peers;
		if (r && d &&	MATRIX(_first, y+1,x+1))	++peers;
		if (d && 		MATRIX(_first, y+1,x  )) 	++peers;
		if (l && d &&	MATRIX(_first, y+1,x-1)) 	++peers;		
		
		return peers;
		
	}

public:
	
	BoolMatrix _first, _second;

	GameOfLife(BoolMatrix first, BoolMatrix second):
		_first(first), _second(second) { }

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
		
		for (size_t y = rows.begin(); y != rows.end(); ++y) {
			for (size_t x = cols.begin(); x != cols.end(); ++x) {
				
				int peers = sumNeighbours(x, y);
				if (peers < 2 || peers > 3) {
					MATRIX(second, y,x) = false;	// hunger/overcrowding
				} else if (peers == 3) {
					MATRIX(second, y,x) = true;		// breeding
				} else {
					MATRIX(second, y,x) = MATRIX(first, y,x);	// nothing.
				}
				
			}
		}
		
	}
};

/*****************************************************************************/

void print_array(BoolMatrix board) {
	for (int y = 0; y < Cowichan::NELTS; ++y) {
		for (int x = 0; x < Cowichan::NELTS; ++x) {
			if (MATRIX(board, y,x)) {
				std::cout << "x ";
			} else {
				std::cout << ". ";
			}
		}
		std::cout << "\n";
	}
	for (int i = 0; i < Cowichan::NELTS; ++i) {
		std::cout << "--";
	}
	std::cout << "\nPress enter.";
}

int main(int argc, char** argv) {
	
	BoolMatrix board1 = NEW_MATRIX_SQUARE(bool);
	BoolMatrix board2 = NEW_MATRIX_SQUARE(bool);
	
	// seed the random number generator + init. TBB
	COWICHAN_PARALLEL;
	
	// SERIAL: generate random valuesq for game of life
	for (int y = 0; y < Cowichan::NELTS; ++y) {
		for (int x = 0; x < Cowichan::NELTS; ++x) {
			bool decision = (rand() % 2 == 0);
			MATRIX(board1, y,x) = decision;
		}
	}
	
	// start up the graphics
	#ifdef GRAPHICS
		Graphics::init(Cowichan::NELTS, Cowichan::NELTS);
		Graphics::draw(board1);
		Graphics::delay(20);
	#endif
	
	// play the game of life for a while
	GameOfLife game(board1, board2);
	for (int i = 0; i < Cowichan::NUMGEN; ++i) {

		// clear the screen
		//system("clear");
	
		// update CA simulation
		parallel_for(
			Range2D(0, Cowichan::NELTS, 0, Cowichan::NELTS),
			game, auto_partitioner());

		// Show the current game state in SDL.
		#ifdef GRAPHICS
			Graphics::draw(game._second);
			Graphics::delay(20);
		#endif
		
		// show the current game state and wait for user interaction
		//print_array(game._second);
		//std::cin.get();
		
		// swap arrays (ping-pong approach)
		game.swap();		

	}

	// exit the program.	
	#ifdef GRAPHICS
		Graphics::deinit();
	#endif
	return 0;

}

