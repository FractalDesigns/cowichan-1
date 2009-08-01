#include <iostream>
#include <cstdio>
#include <cmath>
#include "hull.hpp"

const char* REQUEST = "hull request";
const char* REQUEST_MINMAX = "hull request min/max";
const char* REQUEST_CROSS = "hull request cross-product";

const char* SYNCH_LOCK = "hull synch lock";
const char* POINTS_DONE = "hull points reporting";

const char* MIN_X_POINT = "hull minPoint";
const char* MAX_X_POINT = "hull maxPoint";
const char* MAX_CROSS = "hull max cross-product";
const char* MAX_POINT = "hull furthest point";

const char* MASKED_POINT = "hull masked point";
const char* NUM_POINTS = "hull # of points in convex hull"

const char* FLAG_OUTPUT = "hull flag output";
const char* FINISHED_MINMAX = "hull finshed min/max";


void CowichanLinuxTuples::hull(PointVector pointsIn, PointVector pointsOut) {
	real order = 0;

	// while not all points are used up then run quickhull on the rest of points
	while (order < HULL_N) {

		// Run quickhull on the decided points as a tuple-space problem.
		// TODO figure out this MESS with the inputs and the outputs
		LTHull app;
		app.addInput(0, pointsIn);
		app.start(SERVER, PORT, NUM_WORKERS);

		// get and remove the num-points "order" tuple from tuple space
		// add it to the order so we can keep track of the number of
		// points that we have left to process.
		order += app.getNumPoints();

	}

}

//===========================================================================//

void LTHull::consumeInput() {

	tuple* flagOutput = make_tuple("s", FLAG_OUTPUT);

	// create a tuple synch lock
	tuple *synchLock = make_tuple("s", SYNCH_LOCK);
	put_tuple(synchLock, &ctx);

	// base case
	if (n == 1) {

		// emit the only point
		tuple* hullPoint = make_tuple("sis", (*order)++);
		hullPoint->elements[2].data.s.len = sizeof(Point);
		hullPoint->elements[2].data.s.ptr = &p1;
		put_tuple(hullPoint, &ctx);

		// put the number of points (1) in tuple space
		tuple *order = make_tuple("si", NUM_POINTS, 1);
		put_tuple(order, &ctx);

	} else {

		// figure out the points with minimum and maximum x values
		Point minPoint, maxPoint;
		computeMinMax(n, &minPoint, &maxPoint);

		// use these as initial pivots
		index_t order = 0;
		split(minPoint, maxPoint, &order);
		split(maxPoint, minPoint, &order);

		// put the number of points in tuple space
		tuple *order = make_tuple("si", NUM_POINTS, order);
		put_tuple(order, &ctx);

	}

	// flag the output producer
	put_tuple(flagOutput, &ctx);

}

/**
 * Compute hull on one side of the splitting line.
 * @param pointsIn hull input points.
 * @param n number of input points.
 * @param pointsOut hull output points.
 * @param p1 boundary point #1.
 * @param p2 boundary point #2.
 */
void LTHull::split(const Point& p1, const Point& p2, index_t *order) {

	PointVector pointsIn = (PointVector) inputs[0];
	size_t n = (size_t) inputs[1];

	Point* maxPoint = NULL;
	real maxCross;

	// compute the signed distances from the line for each point
	for (index_t i = 0; i < n; i++) {
		if (!isMasked(i)) {
			real currentCross = Point::cross(p1, p2, pointsIn[i]);
			if (maxPoint == NULL || currentCross > maxCross) {
				maxPoint = &pointsIn[i];
				maxCross = currentCross;
			}
		}
	}

	// is there a point in the positive half-space?
	// if so, it has maximal distance, and we must recurse based on that point.
	if (maxPoint != NULL && maxCross > 0.0) {
		// recurse on the new set with the given far point
		split(p1, maxPoint, order);
		split(maxPoint, p2, order);
		return;
	}

	// otherwise, it's not on the right side; we don't need to split anymore.
	// this is because all points are inside the hull when we use this
	// half-space. add the first point and return.

 	// emit p1 to tuple space using order index
	tuple* hullPoint = make_tuple("sis", (*order)++);
	hullPoint->elements[2].data.s.len = sizeof(Point);
	hullPoint->elements[2].data.s.ptr = &p1;
	put_tuple(hullPoint, &ctx);

}

void LTHull::computeMinMax(size_t n, Point* minPoint, Point* maxPoint) {

	PointVector pointsIn = (PointVector) inputs[0];

	// create a "points reporting" tuple, so that we
	// know when the computation should end.
	tuple *pointsReporting = make_tuple("si", POINTS_DONE, 0);
	put_tuple(pointsReporting, &ctx);

	// tuple template.
	tuple *send = make_tuple("ssiiss", REQUEST, REQUEST_MINMAX);

	// split points, based on a cluster size of the square-root of the
	// number of points given.
	size_t skip = (size_t) sqrt((real) n);
	for (size_t pos = 0; pos < NORM_N; pos += skip) {
		send->elements[1].data.i = pos;
		send->elements[2].data.i = MIN(pos + skip, n);
		put_tuple(send, &ctx);
	}

	// wait for all points to report
	pointsReporting->elements[1].data.i = n;
	destroy_tuple(get_tuple(pointsReporting, &ctx));

	// grab the min-x/max-x points from tuple-space
	tuple *templateMax = make_tuple("s?", MAX_X_POINT);
	tuple *templateMin = make_tuple("s?", MIN_X_POINT);
	tuple *tupleMax = get_tuple(templateMax, &ctx);
	tuple *tupleMin = get_tuple(templateMin, &ctx);
	*minPoint = *tupleMin->elements[1].data.ptr;
	*maxPoint = *tupleMax->elements[1].data.ptr;
	destroy_tuple(templateMin);
	destroy_tuple(templateMax);
	destroy_tuple(tupleMin);
	destroy_tuple(tupleMax);

}

void LTHull::work() {

	// tuple templates
	tuple *recv = make_tuple("s?????", REQUEST);

	while (!get_tuple_nb(finishedMinMax, &ctx)) {

		// block until we receive a tuple.
		tuple* gotten = get_tuple(recv, &ctx);

		// Dispatch based on type of request
		if (!strcmp(gotten->elements[1].data.s.ptr, REQUEST_MINMAX)) {
			serviceMinMaxRequest(gotten);
		} else {
			serviceCrossRequest(gotten);
		}

		// purge local memory of the tuple we received
		destroy_tuple(gotten);

	}

}

void LTHull::serviceMinMaxRequest(tuple* gotten) {

	tuple *synchLock = make_tuple("s", SYNCH_LOCK);

	// grab pointers locally.
	PointVector pointsIn = (PointVector) inputs[0];

	// extract data from tuple
	size_t start = gotten->elements[2].data.i;
	size_t stop = gotten->elements[3].data.i;

	// perform the actual computation for this row (min/max)
	bool first = true;
	Point minPoint, maxPoint;
	for (size_t pos = start; pos < stop; ++pos) {

		// make sure we only look at non-masked points
		if (isMasked(pos)) continue;

		// figure out the points with minimum and maximum x values
		if (minPoint.x > pointsIn[pos].x || first) {
			minPoint = pointsIn[pos];
		}
		if (maxPoint.x < pointsIn[pos].x || first) {
			maxPoint = pointsIn[pos];
		}

		// do not trigger the "first" rules again.
		if (first) first = false;

	}

	// Now, we combine the results from these points with the "world".
	// enter the critical section
	get_tuple(synchLock, &ctx);

		// combine results with master copy (minPoint)
		tuple *tmpMin = make_tuple("s?", MIN_X_POINT);
		tuple *tupleMin = get_nb_tuple(tmpMin, &ctx);
		if (tupleMin != NULL) {
			Point* worldMin = (Point*) tupleMin->elements[1].data.s;
			if (minPoint.x > worldMin->x) {
				minPoint = worldMin;
			}
			destroy_tuple(tupleMin);
		}
		tmpMin->elements[1].data.s.len = sizeof(Point);
		tmpMin->elements[1].data.s.ptr = (char*) &minPoint;
		put_tuple(tmpMin, &ctx);
		destroy_tuple(tmpMin);

		// combine results with master copy (maxPoint)
		tuple *tmpMax = make_tuple("s?", MAX_X_POINT);
		tuple *tupleMax = get_nb_tuple(tmpMax, &ctx);
		if (tupleMax != NULL) {
			Point* worldMax = (Point*) tupleMax->elements[1].data.s;
			if (maxPoint.x < worldMax->x) {
				maxPoint = worldMax;
			}
			destroy_tuple(tupleMax);
		}
		tmpMax->elements[1].data.s.len = sizeof(Point);
		tmpMax->elements[1].data.s.ptr = (char*) &maxPoint;
		put_tuple(tmpMax, &ctx);
		destroy_tuple(tmpMax);

		// record the number of points reporting
		tuple *templatePointsReporting = make_tuple("s?", POINTS_DONE);
		tuple *pointsReporting = get_tuple(templatePointsReporting, &ctx);
		pointssReporting->elements[1].data.i += (stop - start);
		put_tuple(pointsReporting, &ctx);

	// leave the critical section
	put_tuple(synchLock, &ctx);

}

void LTHull::serviceCrossRequest(tuple* gotten) {

	tuple *synchLock = make_tuple("s", SYNCH_LOCK);

	// grab pointers locally.
	PointVector pointsIn = (PointVector) inputs[0];

	// extract data from tuple
	size_t start = gotten->elements[2].data.i;
	size_t stop = gotten->elements[3].data.i;
	Point* p1 = (Point*) gotten->elements[4].data.s.ptr;
	Point* p2 = (Point*) gotten->elements[5].data.s.ptr;

	// perform the actual computation for these elements
	Point* maxPoint = NULL;
	real maxCross = MINIMUM_REAL;
	for (size_t pos = start; pos < stop; ++pos) {

		// make sure we only look at non-masked points
		if (isMasked(pos)) continue;

		// compute the signed distances from the line for each point
		real currentCross = Point::cross(*p1, *p2, pointsIn[pos]);
		if (currentCross > maxCross) {
			maxPoint = &pointsIn[i];
			maxCross = currentCross;
		}

	}

	// Now, we combine the results from these points with the "world".
	// enter the critical section
	get_tuple(synchLock, &ctx);

		bool updateWorldPoint = true;

		// figure out if we should combine our results with the world
		tuple *tmpCross = make_tuple("s?", MAX_CROSS);
		tuple *tupleMax = get_nb_tuple(tmpCross, &ctx);
		if (tupleMax != NULL) {
			real worldMax = tupleMin->elements[1].data.d;
			if (worldMax > maxCross) {
				// don't update if the world has a farther point
				updateWorldPoint = false;
			}
			destroy_tuple(tupleMax);
		}

		// combine results with master copy (world)
		if (updateWorldPoint) {
			tuple *tmpPoint = make_tuple("s?", MAX_POINT);

			// destroy the max point tuple if it already exists
			tuple *tuplePoint = get_tuple_nb(tmpPoint, &ctx);
			if (tuplePoint != NULL) destroy_tuple(tuplePoint);

			// fill in tuple information
			tmpPoint->elements[1].data.s.len = sizeof(Point);
			tmpPoint->elements[1].data.s.ptr = maxPoint;
			tmpCross->elements[1].data.d = maxCross;

			// put in tuples and remove from local memory
			put_tuple(tmpPoint, &ctx);
			put_tuple(tmpCross, &ctx);
			destroy_tuple(tmpCross);
			destroy_tuple(tmpPoint);
		}

		// record the number of elements reporting
		tuple *templatePointsReporting = make_tuple("s?", POINTS_DONE);
		tuple *pointsReporting = get_tuple(templatePointsReporting, &ctx);
		pointssReporting->elements[1].data.i += (stop - start);
		put_tuple(pointsReporting, &ctx);

	// leave the critical section
	put_tuple(synchLock, &ctx);

}

bool LTHull::isMasked(index_t position) {

	tuple* masked = make_tuple("si", MASKED_POINT, position);
	if (read_tuple_nb(masked, &ctx) != NULL) return true;
	return false;

}

void LTHull::mask(index_t position) {

	tuple* masked = make_tuple("si", MASKED_POINT, position);
	put_tuple(masked, &ctx);

}

int LTHull::getNumPoints() {

	tuple *templateOrder = make_tuple("s?", NUM_POINTS);
	tuple *order = get_tuple(order, &ctx);

	int ret = order->elements[1].data.i;

	destroy_tuple(order);
	destroy_tuple(templateOrder);

	return ret;

}

void LTHull::produceOutput() {

	// wait for output flag
	tuple *flag = make_tuple("s", FLAG_OUTPUT);
	destroy_tuple(get_tuple(flag, &ctx));

	// TODO bring in all of the emitted points in order

	// delete all mask tuples
	tuple* maskTemplate = make_tuple("s?", MASKED_POINT);
	while (true) {
		if (get_tuple_nb(masked, &ctx) == NULL) break;
	}

	// remove the tuple synch lock from tuple space
	tuple *synchLock = make_tuple("s", SYNCH_LOCK);
	get_tuple(synchLock, &ctx);

}
