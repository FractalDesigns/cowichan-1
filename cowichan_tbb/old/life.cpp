#include <iostream>
#include <cstdlib>
#include <ctime>

#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range2d.h"
#include "tbb/parallel_for.h"
#include "tbb/parallel_reduce.h"
using namespace tbb;

#define BOARD_SIZE 300 	   // The dimension size of the Game of Life board.
#define ITERATIONS 2000    // The number of times to iterate the Game o'.

#ifdef GRAPHICS
	// must be included after BOARD_SIZE... this is UGLY, sorry.
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
		bool r = (x < (BOARD_SIZE - 1));
		bool u = (y > 0);
		bool d = (y < (BOARD_SIZE - 1));		

		// calculate no. of neighbours
		if (l && (*_first)[y][x-1]) 		++peers;
		if (l && u && (*_first)[y-1][x-1]) 	++peers;
		if (u && (*_first)[y-1][x]) 		++peers;
		if (r && u && (*_first)[y-1][x+1]) 	++peers;
		if (r && (*_first)[y][x+1])			++peers;
		if (r && d && (*_first)[y+1][x+1])	++peers;
		if (d && (*_first)[y+1][x]) 		++peers;
		if (l && d && (*_first)[y+1][x-1]) 	++peers;		
		
		return peers;
		
	}

public:
	
	bool (*_first)[BOARD_SIZE][BOARD_SIZE], (*_second)[BOARD_SIZE][BOARD_SIZE];

	GameOfLife(bool (*first)[BOARD_SIZE][BOARD_SIZE], bool (*second)[BOARD_SIZE][BOARD_SIZE]):
		_first(first), _second(second) { }

	void swap() {
		bool (*temp)[BOARD_SIZE][BOARD_SIZE] = _first;
		_first = _second;
		_second = temp;
	}

	/**
	 * Performs the game of life operation over the given range.
	 */
	void operator()(const blocked_range2d<size_t,size_t>& range) const {
		
		bool (*first)[BOARD_SIZE][BOARD_SIZE] = _first;
		bool (*second)[BOARD_SIZE][BOARD_SIZE] = _second;
		const blocked_range<size_t>& rows = range.rows();
		const blocked_range<size_t>& cols = range.cols();
		
		for (size_t y = rows.begin(); y != rows.end(); ++y) {
			for (size_t x = cols.begin(); x != cols.end(); ++x) {
				
				int peers = sumNeighbours(x, y);
				if (peers < 2 || peers > 3) {
					(*second)[y][x] = false;	// hunger/overcrowding
				} else if (peers == 3) {
					(*second)[y][x] = true;		// breeding
				} else {
					(*second)[y][x] = (*first)[y][x];	// nothing.
				}
				
			}
		}
		
	}
};

/*****************************************************************************/

void print_array(bool (*board)[BOARD_SIZE][BOARD_SIZE]) {
	for (int y = 0; y < BOARD_SIZE; ++y) {
		for (int x = 0; x < BOARD_SIZE; ++x) {
			if ((*board)[y][x]) {
				std::cout << "x ";
			} else {
				std::cout << ". ";
			}
		}
		std::cout << "\n";
	}
	for (int i = 0; i < BOARD_SIZE; ++i) {
		std::cout << "--";
	}
	std::cout << "\nPress enter.";
}

int main(int argc, char** argv) {
	
	bool board1[BOARD_SIZE][BOARD_SIZE];
	bool board2[BOARD_SIZE][BOARD_SIZE];
	
	// seed the random number generator.
	srand(time(0));
	
	// SERIAL: generate random values for game of life
	for (int y = 0; y < BOARD_SIZE; ++y) {
		for (int x = 0; x < BOARD_SIZE; ++x) {
			bool decision = (rand() % 2 == 0);
			board1[y][x] = decision;
		}
	}

	// show the initial array
	//print_array(&board1);
	
	// start up TBB
	task_scheduler_init init;
	
	// start up the graphics
	#ifdef GRAPHICS
		Graphics::init(BOARD_SIZE, BOARD_SIZE);
	#endif
	
	// play the game of life for a while
	GameOfLife game(&board1, &board2);
	for (int i = 0; i < ITERATIONS; ++i) {

		// clear the screen
		//system("clear");
	
		// update CA simulation
		parallel_for(
			blocked_range2d<size_t,size_t>(0, BOARD_SIZE, 0, BOARD_SIZE),
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

