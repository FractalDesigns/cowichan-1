#include <iostream>
#include <cstdio>
#include "thresh.hpp"

void CowichanLinuxTuples::thresh(IntMatrix matrix, BoolMatrix outMatrix) {
	LTThresh app;
	app.addInput(0, matrix);
	app.addOutput(0, outMatrix, sizeof(outMatrix));
	app.start(SERVER, PORT, NUM_WORKERS);
}

/**
 * THE PLAN
 * 1. calculate max value in matrix per row, communicate results
 *    and then flag consumeinput to continue from produceOutput
 * 2. 
 * this will req. change over to threads 
 */

void LTThresh::consumeInput() {


}

void LTThresh::work() {

	

}

void LTThresh::produceOutput() {

	// tuple template
	tuple *recv = make_tuple("s??", "thresh final");

	// grab output pointer locally.
//	IntMatrix output = (IntMatrix) outputs[0];

	// grab all of the mandelbrot computations from the workers,
	// in an unspecified order.
	int computations = THRESH_NR;
	while (computations > 0) {

	}

	// destroy the template tuple
	destroy_tuple(recv);

}

