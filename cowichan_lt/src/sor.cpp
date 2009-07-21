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

	// initialize
	for (index_t r = 0; r < SOR_NR; ++r) {
		VECTOR(solution, r) = 1.0;
	}

	// put a tuple for the solution vector into tuple space
	Vector solution = (Vector) outputs[0];
	tuple *solutionTuple = make_tuple("ss", SOLUTION_VECTOR);
	solutionTuple->elements[1].data.s.len = sizeof(solution);
	solutionTuple->elements[1].data.s.ptr = solution;

	// loop until we get the desired tolerance
	real maxDiff = (real)(2 * SOR_TOLERANCE);
	for (index_t t = 0; (t < SOR_MAX_ITERS) && (maxDiff >= SOR_TOLERANCE); t++) {

		maxDiff = 0.0;
		for (index_t r = 0; r < SOR_NR; ++r) {

			// compute sum
			real sum = solutionSum(r);

			// calculate new solution
			real oldSolution = VECTOR(solution, r)
			VECTOR(solution, r) = (real)((1.0 - SOR_OMEGA) * oldSolution + SOR_OMEGA *
					(VECTOR(target, r) - sum) / MATRIX_SQUARE(matrix, r, r));

			// refresh the solution vector in tuple-space
			get_tuple(make_tuple("s?", SOLUTION_VECTOR), &ctx);
			put_tuple(solutionTuple, &ctx);

			// compute difference
			real diff = (real) fabs((double)(oldSolution - VECTOR(solution, r)));
			if (diff > maxDiff) {
				maxDiff = diff;
			}
		}
	}

	// flag the output producer.
	put_tuple(make_tuple("s", SOR_FLAG), &ctx);

}

real LTSor::solutionSum(index_t row) {

	// tuple template.
	tuple *send = make_tuple("sii", "sor request");

	// create an "rows reporting" tuple, so that we
	// know when the computation should end.
	tuple *rowsReporting = make_tuple("si", ROWS_DONE, 0);
	put_tuple(rowsReporting, &ctx);

	// create a sum tuple
	tuple *sumTuple = make_tuple("sd", SOLUTION_SUM, 0.0);
	put_tuple(sumTuple, &ctx);

	// split points, based on a cluster size of the square-root of the
	// number of elements in the solution vector.
	size_t skip = (size_t) sqrt((real) SOR_NR);
	for (size_t pos = 0; pos < SOR_NR; pos += skip) {
		send->elements[1].data.i = pos;
		send->elements[2].data.i = min(pos + skip, SOR_NR);
		put_tuple(send, &ctx);
	}

	// wait for all the rows to be consumed
	rowsReporting->elements[1].data.i = SOR_NR;
	get_tuple(rowsReporting);

	// get the sum of the tuple-space op
	destroy_tuple(sumTuple);
	sumTuple = make_tuple("s?", SOLUTION_SUM);
	get_tuple(sumTuple, &ctx);

	// return the result
	return sumTuple->elements[1].data.d;

}

void LTSor::work() {

	// tuple templates
	tuple *synchLock = make_tuple("s", SYNCH_LOCK);
	tuple *recv = make_tuple("s??", "sor request");

	// grab pointers locally.
	Matrix matrix = (Matrix) inputs[0];

	while (1) {

		// block until we receieve a tuple.
		tuple* gotten = get_tuple(recv, &ctx);
		index_t row = gotten->elements[1].data.i;
		index_t start = gotten->elements[2].data.i;
		index_t stop = gotten->elements[3].data.i;

		// grab the solution tuple
		tuple *templateSolutionTuple = make_tuple("s?", SOLUTION_VECTOR);
		tuple *solutionTuple = read_tuple(templateSolutionTuple, &ctx);
		destroy_tuple(templateSolutionTuple);
		Vector solution = (Vector) solutionTuple->elements[1].s.ptr;

		// sum part of the matrix row with the corresponding elements
		// in the solution vector.
		real sum = 0.0;
		for (index_t col = start; col < stop; ++col) {
			if (col != row) {
				sum += MATRIX_SQUARE(matrix, row, col) * VECTOR(solution, col);
			}
		}

		// purge local memory of the tuple we received
		destroy_tuple(gotten);

		// Now, we combine the results from these elements with the "world".
		// enter the critical section
		get_tuple(synchLock, &ctx);

			// combine results with master copy (sum)
			tuple *tmpSum = make_tuple("s?", SOLUTION_SUM);
			tuple *tupleSum = get_tuple(tmpSum, &ctx);
			tmpMax->elements[1].data.d = sum + tupleMax->elements[1].data.d;
			put_tuple(tmpMax, &ctx);
			destroy_tuple(tmpMax);

			// record the number of rows reporting
			tuple *templateRowsReporting = make_tuple("s?", ROWS_DONE);
			tuple *rowsReporting = get_tuple(templateRowsReporting, &ctx);
			rowsReporting->elements[1].data.i += (stop - start);
			put_tuple(rowsReporting, &ctx);

		// leave the critical section
		put_tuple(synchLock, &ctx);

	}

}

void LTSor::produceOutput() {

	// wait for the input producer to flag us.
	tuple* tmpFlag = make_tuple("s", SOR_FLAG);
	tuple* flag = get_tuple(tmpFlag, &ctx);

	// grab the solution tuple and copy it to the output
	tuple *templateSolutionTuple = make_tuple("s?", SOLUTION_VECTOR);
	tuple *solutionTuple = read_tuple(templateSolutionTuple, &ctx);
	memcpy(outputs[0],
		solutionTuple->elements[1].s.ptr,
		solutionTuple->elements[1].s.len);

	// remove the tuple synch lock from tuple space
	tuple *synchLock = make_tuple("s", SYNCH_LOCK);
	get_tuple(synchLock, &ctx);

	// clean-up
	destroy_tuple(tmpFlag);
	destroy_tuple(flag);
	destroy_tuple(templateSolutionTuple);
	destroy_tuple(solutionTuple);

}
