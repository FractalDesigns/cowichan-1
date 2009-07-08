#include <iostream>
#include <cstdio>
#include "thresh.hpp"

void CowichanLinuxTuples::thresh(IntMatrix matrix, BoolMatrix outMatrix) {

	// calculate the frequency breaking-point
	LTFrequency freq;
	freq.addInput(0, matrix);
	freq.start(SERVER, PORT, NUM_WORKERS);

	// calculate
	LTThresh thresh;
	thresh.addInput(0, matrix);
	thresh.addOutput(0, outMatrix);
	thresh.start(SERVER, PORT, NUM_WORKERS);

}

void LTThresh::consumeInput() {

	// create a tuple synch lock
	tuple *synchLock = make_tuple("s", SYNCH_LOCK);
	put_tuple(synchLock, &ctx);

	// create a "rows reporting" tuple, so that we
	// know when the computation should end.
	tuple *rowsReporting = make_tuple("si", ROWS_DONE, 0);
	put_tuple(rowsReporting, &ctx);

	// TODO communicate all of the rows

}

void LTThresh::work() {

	// tuple templates
	tuple *synchLock = make_tuple("s", SYNCH_LOCK);
	tuple *recv = make_tuple("s?", "thresh request");
	tuple *send = make_tuple("sis", "thresh done");

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

		// perform the actual computation for this row (counting)
		for (int x = 0; x < THRESH_NC; ++x) {
			// TODO create a map starting at all 0s and increment frequency
		}

		// purge local memory of the tuple we received
		destroy_tuple(gotten);

		// Now, we combine the results from this row with every other row.
		// enter the critical section
		get_tuple(synchLock, &ctx);

		// combine results with master copy
		for (std::iterator on map returning pairs) {
			tuple *templateHist = make_tuple("sii", "thresh hist", n);
			tuple *hist = get_nb_tuple(templateHist, &ctx);
			if (hist == NULL) {
				// use only this row's frequency
				templateHist->elements[2].data.i = freq[n];
			} else {
				// combine this row's frequency and existing frequency
				templateHist->elements[2].data.i = freq[n] + hist->elements[2].data.i;
			}
			put_tuple(templateHist, &ctx);
			destroy_tuple(hist);
		}

		// record the number of rows reporting
		tuple *templateRowsReporting = make_tuple("si", ROWS_DONE);
		tuple *rowsReporting = get_tuple(templateRowsReporting, &ctx);
		rowsReporting->elements[1].data.i += 1;
		put_tuple(rowsReporting, &ctx);

		// leave the critical section
		put_tuple(synchLock, &ctx);

	}

}

void LTThresh::produceOutput() {

	// wait for all rows to be done.
	tuple *allRowsReporting = make_tuple("si", ROWS_DONE, THRESH_NR);
	get_tuple(allRowsReporting, &ctx);

	// go through all tuple values until we have reached the threshold point
	size_t freq = 0, n;
	for (n = 0; n < MAX_INT; ++n) {
		tuple *templateHist = make_tuple("sii", "thresh hist", n);
		tuple *hist = get_nb_tuple(templateHist, &ctx);
		if (hist != NULL) {
			freq += hist->elements[2].data.i;
		}
		if (freq >= ...) break;
	}

	// n now holds the threshold point. communicate this into the tuple space
	tuple *threshPoint = make_tuple(si, "thresh point", n);
	put_tuple(threshPoint, &ctx);

	// remove the tuple synch lock from tuple space
	tuple *synchLock = make_tuple("s", SYNCH_LOCK);
	get_tuple(synchLock, &ctx);

}

