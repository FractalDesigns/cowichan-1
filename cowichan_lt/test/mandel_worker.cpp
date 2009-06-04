#include <iostream>
#include <cstdio>
extern "C" {
	#include "tuple.h"
}

using std::cout;
using std::endl;

typedef double real;

#define MANDEL_INFINITY	2.0
#define MANDEL_MAX_ITER	150

int mandelCalc(real x, real y) {

	real r = 0.0, i = 0.0;
	real rs = 0.0, is = 0.0;
	int numIterations = 0;		
	do {

		// calculate the complex value according to the mandelbrot set specs.
		i = (2.0 * r * i) + x;
		r = (rs - is) + y;
	
		// calculate squared complex value
		rs = r * r;
		is = i * i;			
	
		// "step" the simulation for this co-ordinate.
		++numIterations;			
	
	} while ((numIterations < MANDEL_MAX_ITER) && ((rs + is) < MANDEL_INFINITY));

	// we are interested if the series converges or diverges. Return the
	// number of iterations before such an event (divergence).
	return numIterations;

}

int main(int argc, char** argv) {

	// connect to the tuple server.
	struct context ctx;
	if (get_server_portnumber(&ctx)) {
		if (argc < 3) {
			/* help message */
			fprintf(stderr, "Usage: %s <server> <portnumber>\n", argv[0]);
			exit(1);
		}
		strcpy(ctx.peername, argv[1]);
		ctx.portnumber = atoi(argv[2]);
	}
	tuple *recv = make_tuple("s????", "mandel request");
	tuple *send = make_tuple("siii", "mandel done");
	
	// send off a mandelbrot request for each grid index.
	while (1) {

		// block until we receieve a tuple.
		tuple* received = get_tuple(recv, &ctx);

		// copy over grid co-ordinates of the computation.
		send->elements[1].data.i = received->elements[1].data.i;
		send->elements[2].data.i = received->elements[2].data.i;

		// perform the actual computation.
		send->elements[3].data.i = mandelCalc(
			recv->elements[3].data.d, // x co-ordinate
			recv->elements[4].data.d  // y co-ordinate
		);

		// send off the new tuple and purge local memory of the one we received
		put_tuple(send, &ctx);
		destroy_tuple(received);

	}

	// destroy the template tuples
	destroy_tuple(send);
	destroy_tuple(recv);

	return 0;	
}

