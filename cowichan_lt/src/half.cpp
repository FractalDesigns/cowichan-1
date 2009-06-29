#include <iostream>
#include <cstdio>
#include "half.hpp"

void CowichanLinuxTuples::half(IntMatrix matrixIn, IntMatrix matrixOut) {
	LTHalf app;
	app.addInput(0, matrixIn);
	app.addOutput(0, matrixOut, sizeof(matrixOut));
	app.start(SERVER, PORT, NUM_WORKERS);
}

void LTHalf::consumeInput() {

	// tuple template
	tuple *send = make_tuple("si", "half request");

	// send off a request for each grid row.
	for (size_t y = 0; y < HALF_NR; ++y) {
		send->elements[1].data.i = y;
		put_tuple(send, &ctx);
	}
	
	// destroy the template tuple
	destroy_tuple(send);

}

void LTHalf::work() {

	tuple *recv = make_tuple("s?", "half request");
	tuple *send = make_tuple("sis", "half done");
	
	// grab pointers locally.
	IntMatrix input = (IntMatrix) inputs[0];
	
	// satisfy mandelbrot requests.
	while (1) {

		// block until we receieve a tuple.
		tuple* gotten = get_tuple(recv, &ctx);

		// copy over row co-ordinate of the computation; create
		// a buffer for the results of the computation.
		size_t y = gotten->elements[1].data.i;
		send->elements[1].data.i = y;
		int* buffer = (int*) malloc(sizeof(int) * HALF_NC);
		send->elements[2].data.s.len = sizeof(int) * HALF_NC;
		send->elements[2].data.s.ptr = (char*) buffer;

		// perform the actual computation for this row.
		for (int x = 0; x < HALF_NC; ++x) {
			buffer[x] = MATRIX_RECT_NC(input, (y^1),(x^1), HALF_NC);
		}
	
		// send off the new tuple and purge local memory of the one we got
		put_tuple(send, &ctx);
		destroy_tuple(gotten);

	}

	// TODO destroy the template tuples; must send tuples for this
//	destroy_tuple(send);
//	destroy_tuple(recv);

}

void LTHalf::produceOutput() {

	// tuple template
	tuple *recv = make_tuple("s??", "half done");

	// grab output pointer locally.
	IntMatrix output = (IntMatrix) outputs[0];

	// grab all of the mandelbrot computations from the workers,
	// in an unspecified order.
	int computations = RANDMAT_NR;
	while (computations > 0) {

		// get the tuple and copy it into the matrix.
		tuple* received = get_tuple(recv, &ctx);
		memcpy(
			&MATRIX_RECT_NC(output, received->elements[1].data.i, 0, HALF_NC),
			received->elements[2].data.s.ptr,
			received->elements[2].data.s.len);
		computations--;
		destroy_tuple(received);

	}

	// destroy the template tuple
	destroy_tuple(recv);

}

