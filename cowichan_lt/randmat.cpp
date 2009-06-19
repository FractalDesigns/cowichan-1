#include <iostream>
#include <cstdio>
#include "randmat.hpp"

void CowichanLinuxTuples::randmat(IntMatrix matrix) {
	LTRandmat app;
	app.addOutput(0, matrix, sizeof(matrix));
	app.setup(); // create the first, seed column
	app.start(SERVER, PORT, NUM_WORKERS);
}

/**
 * Create the first column of random values (starting seed for each row).
 */
void LTRandmat::setup() {

	IntMatrix output = (IntMatrix) outputs[0];

	// set up the first column of matrix values
	MATRIX_RECT_NC(output, 0, 0, RANDMAT_NC) = RAND_SEED % RAND_M;
	aPrime = RANDMAT_A;
	cPrime = 1;
	for (size_t r = 1; r < RANDMAT_NR; r++) {
		MATRIX_RECT_NC(output, r, 0, RANDMAT_NC) = (RANDMAT_A * MATRIX_RECT_NC(output, r-1, 0, RANDMAT_NC) + RANDMAT_C) % RAND_M;
		cPrime = (cPrime + aPrime) % RAND_M;
		aPrime = (aPrime * RANDMAT_A) % RAND_M;
	}
	cPrime = (cPrime * RANDMAT_C) % RAND_M;

}

void LTRandmat::consumeInput() {

	// tuple template
	tuple *send = make_tuple("si", "randmat request");

	// send off a request for each grid row.
	for (size_t y = 0; y < RANDMAT_NR; ++y) {
		send->elements[1].data.i = y;
		put_tuple(send, &ctx);
	}
	
	// destroy the template tuple
	destroy_tuple(send);

}

void LTRandmat::work() {

	tuple *recv = make_tuple("s?", "randmat request");
	tuple *send = make_tuple("sis", "randmat done");
	
	// grab output pointer locally.
	IntMatrix output = (IntMatrix) outputs[0];

	// satisfy mandelbrot requests.
	while (1) {

		// block until we receieve a tuple.
		tuple* gotten = get_tuple(recv, &ctx);

		// copy over row co-ordinate of the computation; create
		// a buffer for the results of the computation.
		size_t row = gotten->elements[1].data.i;
		send->elements[1].data.i = row;
		int* buffer = (int*) malloc(sizeof(int) * RANDMAT_NC);
		send->elements[2].data.s.len = sizeof(int) * RANDMAT_NC;
		send->elements[2].data.s.ptr = (char*) buffer;

		// perform the actual computation for this row.
		buffer[0] = MATRIX_RECT_NC(output, row, 0, RANDMAT_NC);
		for (int x = 1; x < RANDMAT_NC; ++x) {
			buffer[x] = (aPrime * buffer[x-1] + cPrime) % RAND_M;
		}
	
		// send off the new tuple and purge local memory of the one we got
		put_tuple(send, &ctx);
		destroy_tuple(gotten);

	}

	// TODO destroy the template tuples; must send tuples for this
//	destroy_tuple(send);
//	destroy_tuple(recv);

}

void LTRandmat::produceOutput() {

	// tuple template
	tuple *recv = make_tuple("s??", "randmat done");

	// grab output pointer locally.
	IntMatrix output = (IntMatrix) outputs[0];

	// grab all of the mandelbrot computations from the workers,
	// in an unspecified order.
	int computations = RANDMAT_NR;
	while (computations > 0) {

		// get the tuple and copy it into the matrix.
		tuple* received = get_tuple(recv, &ctx);
		memcpy(
			&MATRIX_RECT_NC(output, received->elements[1].data.i, 0, RANDMAT_NC),
			received->elements[2].data.s.ptr,
			received->elements[2].data.s.len);
		computations--;
		destroy_tuple(received);

	}

	// destroy the template tuple
	destroy_tuple(recv);

}

