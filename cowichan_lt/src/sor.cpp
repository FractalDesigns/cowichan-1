#include <iostream>
#include <cstdio>
#include <cmath>
#include "sor.hpp"

void CowichanLinuxTuples::sor(Matrix matrix, Vector target, Vector solution) {

	// calculate the 2D bounds of the point cloud
	LTSor app;
	app.addInput(0, matrix);
	app.addInput(0, target);
	app.addOutput(0, solution);
	program.start(SERVER, PORT, NUM_WORKERS);

}

//===========================================================================//

void LTSor::consumeInput() {

	// create a tuple synch lock
	tuple *synchLock = make_tuple("s", SYNCH_LOCK);
	put_tuple(synchLock, &ctx);

	// tuple template.
	tuple *send = make_tuple("si", "sor request");

	// put the max-diff tuple into tuple space.
	// we start it off as 2 * tolerance to forestall early exit
	tuple *maxDiff = make_tuple("sd", MAX_DIFF, 2 * SOR_TOLERANCE);
	put_tuple(maxDiff, &ctx);

	// loop until we get the desired tolerance
	for (index_t t = 0; (t < SOR_MAX_ITERS) && (maxDiff->elements[1].data.d >= SOR_TOLERANCE); t++) {

		// TODO put solution tuple in

		// put tuples for the solution vector into tuple space
		tuple *rowTuple = make_tuple("sid", SOLUTION_ROW);
		for (index_t r = 0; r < SOR_NR; ++r) {
			rowTuple->elements[1].data.i = r;
			rowTuple->elements[2].data.d = 1.0;
		}

		// create an "rows reporting" tuple, so that we
		// know when the computation should end.
		tuple *rowsReporting = make_tuple("si", ROWS_DONE, 0);
		put_tuple(rowsReporting, &ctx);

		// go row-by-row
		for (index_t r = 0; r < SOR_NR; ++r) {
			send->elements[1].data.i = r;
			put_tuple(send, &ctx);
		}

		// wait for all the rows to be consumed
		rowsReporting->elements[1].data.i = SOR_NR;
		get_tuple(rowsReporting);

		// get the max-diff from that run, so that
		// we can decide whether or not to exit the loop
		destroy_tuple(maxDiff);
		maxDiff = make_tuple("s?", MAX_DIFF);
		read_tuple(maxDiff, &ctx); // doesn't leave tuple space

	}

}

void LTSor::work() {

	// tuple templates
	tuple *synchLock = make_tuple("s", SYNCH_LOCK);
	tuple *recv = make_tuple("s?", "sor request");

	// grab pointers locally.
	Matrix matrix = (Matrix) inputs[0];
	Vector target = (Vector) inputs[1];

	while (1) {

		// block until we receieve a tuple.
		tuple* gotten = get_tuple(recv, &ctx);
		index_t r = gotten->elements[1].data.i;

		// grab the solution tuple
		// XXX don't we change the solution tuple *after* everything?
		tuple *templateSolutionTuple = make_tuple("s?", SOLUTION, r);
		tuple *solutionTuple = read_tuple(templateSolutionTuple, &ctx);
		destroy_tuple(templateSolutionTuple);
		Vector solution = (Vector) solutionTuple->elements[1].s.ptr;

		// perform the actual SOR computation for this row;
		// compute sum
		sum = 0.0;
		for (c = 0; c < r; c++) {
			sum += MATRIX_SQUARE(matrix, r, c) * VECTOR(solution, c)[c];
		}
		for (c = r + 1; c < n; c++) {
			sum += MATRIX_SQUARE(matrix, r, c) * solution[c];
		}

		// calculate new solution
		oldSolution = rowTuple->elements[2].data.d;
		real solution = (real)((1.0 - SOR_OMEGA) * oldSolution + SOR_OMEGA *
				(VECTOR(target, r) - sum) / MATRIX_SQUARE(matrix, r, r));

		// compute difference
		real diff = (real)fabs((double)(oldSolution - solution[r]));

		// purge local memory of the tuple we received
		destroy_tuple(gotten);

		// Now, we combine the results from these points with the "world".
		// enter the critical section
		get_tuple(synchLock, &ctx);

			// combine results with master copy (max element difference)
			tuple *tmpMax = make_tuple("s?", MAX_DIFF);
			tuple *tupleMax = get_nb_tuple(tmpMax, &ctx);
			if (tupleMax != NULL) {
				diff = max(diff, tupleMax->elements[1].data.d);
			}
			tmpMax->elements[1].data.d = diff;
			put_tuple(tmpMax, &ctx);
			destroy_tuple(tmpMax);

			// record the number of rows reporting
			tuple *templateRowsReporting = make_tuple("s?", ROWS_DONE);
			tuple *rowsReporting = get_tuple(templateRowsReporting, &ctx);
			rowsReporting->elements[1].data.i++;
			put_tuple(rowsReporting, &ctx);

		// leave the critical section
		put_tuple(synchLock, &ctx);

	}

}

void LTSor::produceOutput() {

	// wait for the input producer to flag us.

	// pass the solution from tuple space into the given output

	// remove the tuple synch lock from tuple space
	tuple *synchLock = make_tuple("s", SYNCH_LOCK);
	get_tuple(synchLock, &ctx);

}
