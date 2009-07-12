#include <iostream>
#include <cstdio>
#include <cmath>
#include "norm.hpp"

void CowichanLinuxTuples::gauss(Matrix matrix, Vector target, Vector solution) {

	// forward-elimination.
	// forward puts the target and matrix into tuple space.
	LTForward forward;
	forward.addInput(0, matrix);
	forward.addInput(1, target);
	forward.start(SERVER, PORT, NUM_WORKERS);

	// backward-substitution and solution creation.
	// backward takes the target and matrix vectors from tuple space.
	LTBackward backward;
	backward.addOutput(0, solution);
	backward.start(SERVER, PORT, NUM_WORKERS);

}

//===========================================================================//

void LTForward::consumeInput() {

	// grab pointers locally.
	Matrix matrix = (Matrix) inputs[0];
	Vector target = (Vector) inputs[1];

	// tuple templates.
	tuple *send = make_tuple("sii", "forward request");
	tuple *row = make_tuple("sis", "gauss matrix");
	tuple *target = make_tuple("ss", "gauss target");

	// put the matrix (row-by-row) and the target vector into the tuple space.
	for (size_t r = 0; r < GAUSS_NR; ++r) {
		row->elements[1].data.i = r;
		row->elements[2].data.s.len = sizeof(matrix) / GAUSS_NR;
		row->elements[2].data.s.ptr = MATRIX_NC(matrix, row, 0, GAUSS_NC);
		put_tuple(row, &ctx);
	}
	target->elements[1].data.s.len = sizeof(target);
	target->elements[1].data.s.ptr = (void*) target;
	put_tuple(target, &ctx);

	// one column at-a-time.
	for (index_t c = 0; c < GAUSS_NC; ++c) {

		// create a "rows reporting" tuple, so that we
		// know when the computation has ended (workers are done)
		tuple *rowsReporting = make_tuple("si", ROWS_DONE, 0);
		put_tuple(rowsReporting, &ctx);

	    // get row with maximum column i
	    index_t max = c;
	    for (index_t r = i + 1; r < GAUSS_NR; ++r) {
	    	// TODO grab the two rows (r and max)
			if (fabs(MATRIX_NC(matrix, r, c, GAUSS_NC)) > fabs(MATRIX_NC(matrix, max, c, GAUSS_NC))) {
				max = j;
			}
			// TODO put the two rows back (r and max)
		}

		// swap max row with row c
	    real tmp;
		for (index_t r = c; r < GAUSS_NR; ++r) {
			// TODO tuple space swap
		}
		// TODO grab target, swap, put it back

		// create a forward request for each row under the diagonal
		for (index_t r = c + 1; r < GAUSS_NR; ++r) {
			send->elements[1].data.i = r; // row
			send->elements[2].data.i = c; // column
			put_tuple(send, &ctx);
		}

		// wait for the workers to finish this column
		size_t rowsToBeDone = max(0, GAUSS_NR - (c + 1));
		rowsReporting->elements[1].data.i = rowsToBeDone;
		get_tuple(rowsReporting, &ctx);

	}

}

void LTForward::work() {

	// tuple templates
	tuple *recv = make_tuple("s??", "forward request");

	while (1) {

		// block until we receive a tuple.
		tuple* gotten = get_tuple(recv, &ctx);
		index_t row = gotten->elements[1].data.i;
		index_t col = gotten->elements[2].data.i;

		// FIXME actual computation.
		real factor = -(MATRIX(matrix, j, i) / column_i);
		for (k = n - 1; k >= i; k--) {
			MATRIX(matrix, j, k) += MATRIX(matrix, i, k) * factor;
		}
		target[j] += target[i] * factor;

		// purge local memory of the tuple we received
		destroy_tuple(gotten);

		// increment the number of points reporting by one.
		tuple *templateRowsReporting = make_tuple("s?", ROWS_DONE);
		tuple *rowsReporting = get_tuple(templateRowsReporting, &ctx);
		rowsReporting->elements[1].data.i++;
		put_tuple(rowsReporting, &ctx);

	}

}

void LTForward::produceOutput() {

	// wait for columns to be done.
	tuple *forwardDone = make_tuple("s", FORWARD_DONE);
	get_tuple(forwardDone, &ctx);

}

//===========================================================================//

void LTNorm::consumeInput() {

	// tuple template
	tuple *send = make_tuple("sii", "norm request");

	// split points, based on a cluster size of the square-root of the
	// number of points given.
	size_t skip = (size_t) sqrt((real) NORM_N);
	for (size_t pos = 0; pos < NORM_N; pos += skip) {
		send->elements[1].data.i = pos;
		send->elements[2].data.i = min(pos + skip, NORM_N);
		put_tuple(send, &ctx);
	}

	// destroy the template tuple
	destroy_tuple(send);

}

void LTNorm::work() {

	tuple *recv = make_tuple("s??", "norm request");
	tuple *send = make_tuple("siis", "norm done");

	// grab pointers locally.
	PointVector input = (PointVector) inputs[0];

	// get the min/max points from the bounds computation
	tuple *tmpMin = make_tuple("s?", MIN_POINT);
	tuple *tmpMax = make_tuple("s?", MAX_POINT);
	tuple *tupleMin = get_tuple(tmpMin, &ctx);
	tuple *tupleMax = get_tuple(tmpMax, &ctx);
	Point minPoint = *((Point*) tupleMin->elements[1].data.s.ptr);
	Point maxPoint = *((Point*) tupleMax->elements[1].data.s.ptr);

	// satisfy norm requests.
	while (1) {

		// block until we receive a tuple.
		tuple* gotten = get_tuple(recv, &ctx);
		size_t start = gotten->elements[1].data.i;
		size_t stop = gotten->elements[2].data.i;

		// copy over row co-ordinate of the computation; create
		// a buffer for the results of the computation.
		send->elements[1].data.i = start;
		send->elements[2].data.i = stop;
		Point* buffer = (Point*) malloc(sizeof(Point) * (stop - start));
		send->elements[3].data.s.len = sizeof(Point) * (stop - start);
		send->elements[3].data.s.ptr = (char*) buffer;

		// compute scaling factors
		real sclX = (real)((maxPoint.x == minPoint.x) ?
			0.0 : 1 / (maxPoint.x - minPoint.x));
		real sclY = (real)((maxPoint.y == minPoint.y) ?
			0.0 : 1 / (maxPoint.y - minPoint.y));

		// scale (perform the actual computation for this row)
		size_t outPos = 0;
		for (size_t inPos = start; inPos < stop; ++inPos) {
			buffer[outPos].x = sclX * (input[inPos].x - minPoint.x);
			buffer[outPos].y = sclY * (input[inPos].y - minPoint.y);
			outPos++;
		}

		// send off the new tuple and purge local memory of the one we got
		put_tuple(send, &ctx);
		destroy_tuple(gotten);

	}

	// TODO destroy the template tuples; must send tuples for this
//	destroy_tuple(send);
//	destroy_tuple(recv);

}

void LTNorm::produceOutput() {

	// tuple template
	tuple *recv = make_tuple("s???", "norm done");

	// grab output pointer locally.
	PointVector output = (PointVector) outputs[0];

	// grab all of the norm computations from the workers,
	// in an unspecified order.
	int computations = NORM_N;
	while (computations > 0) {

		// get the tuple and copy it into the matrix.
		tuple* received = get_tuple(recv, &ctx);
		memcpy(output + received->elements[1].data.i,
			received->elements[3].data.s.ptr,
			received->elements[3].data.s.len);

		// figure out how many computations that was.
		size_t start = received->elements[1].data.i;
		size_t stop = received->elements[2].data.i;
		computations -= (stop - start);
		destroy_tuple(received);

	}

	// destroy the template tuple
	destroy_tuple(recv);

}
