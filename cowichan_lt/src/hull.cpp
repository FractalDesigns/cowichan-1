#include <iostream>
#include <cstdio>
#include <cmath>
#include "hull.hpp"

void CowichanLinuxTuples::hull(PointVector pointsIn, PointVector pointsOut)
{
	index_t hn = 0;
	index_t previous_hn = 0;

	// while not all points are used up then run quickhull on the rest of points
	while (n != hn) {
		// exclude added points from pointsIn by swapping them with points from the
		// end of pointsIn vector in range (0, n - nused)
		index_t added_i;
		for (added_i = previous_hn; added_i < hn; added_i++) {
			// search for the added point
			for (index_t i = 0; i < n - previous_hn; i++) {
				if ((pointsIn[i].x == pointsOut[added_i].x)
						&& (pointsIn[i].y == pointsOut[added_i].y)) {
					Point tmp = pointsIn[i];
					pointsIn[i] = pointsIn[n - added_i - 1];
					pointsIn[n - added_i - 1] = tmp;
					break;
				}
			}
		}
		previous_hn = hn;

		// Run quickhull on the decided points as a tuple-space problem.
		LTHull app;
		app.addInput(0, pointsIn);
		app.addInput(1, n - hn);
		app.addOutput(0, pointsOut);
		app.addOutput(1, &hn);
		app.start(SERVER, PORT, NUM_WORKERS);
	}
}

//===========================================================================//

void LTHull::consumeInput() {

	// TODO compute n from inputs[]
	// TODO emit n into tuple space

	// create a tuple synch lock
	tuple *synchLock = make_tuple("s", SYNCH_LOCK);
	put_tuple(synchLock, &ctx);

	// base case
	if (n == 1) {
		// flag output producer immediately
		// TODO emit the only point
		// TODO flag
		return;
	}

	// figure out the points with minimum and maximum x values
	Point minPoint, maxPoint;
	computeMinMax(n, &minPoint, &maxPoint);

	// use these as initial pivots
	split(pointsIn, n, minPoint, maxPoint);
	split(pointsIn, n, maxPoint, minPoint);

}

/**
 * Compute hull on one side of the splitting line.
 * @param pointsIn hull input points.
 * @param n number of input points.
 * @param pointsOut hull output points.
 * @param hn pointer to the number of output points.
 * @param p1 boundary point #1.
 * @param p2 boundary point #2.
 */
void LTHull::split(PointVector pointsIn, index_t n, index_t* hn, const Point& p1, const Point& p2) {

	Point* maxPoint = &pointsIn[0];
	real maxCross = Point::cross(p1, p2, pointsIn[0]);

	// compute the signed distances from the line for each point
	for (index_t i = 1; i < n; i++) {
		real currentCross = Point::cross(p1, p2, pointsIn[i]);
		if (currentCross > maxCross) {
			maxPoint = &pointsIn[i];
			maxCross = currentCross;
		}
	}

	// is there a point in the positive half-space?
	// if so, it has maximal distance, and we must recurse based on that point.
	if (maxCross > 0.0) {
		// recurse on the new set with the given far point
		split(pointsIn, n, hn, p1, maxPoint);
		split(pointsIn, n, hn, maxPoint, p2);
		return;
	}

	// otherwise, it's not on the right side; we don't need to split anymore.
	// this is because all points are inside the hull when we use this
	// half-space. add the first point and return.

 	// TODO emit p1 to tuple space using *(hn)++ as order

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
		send->elements[2].data.i = min(pos + skip, n);
		put_tuple(send, &ctx);
	}

	// wait for all points to report
	pointsReporting->elements[1].data.i = n;
	destroy_tuple(get_tuple(pointsReporting, &ctx));

	// TODO grab the min-x/max-x points from tuple-space

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
	Point minPoint = input[start];
	Point maxPoint = input[start];
	for (size_t pos = start + 1; pos < stop; ++pos) {
		// figure out the points with minimum and maximum x values
		if (minPoint.x > pointsIn[pos].x) {
			minPoint = pointsIn[pos];
		}
		if (maxPoint.x < pointsIn[pos].x) {
			maxPoint = pointsIn[pos];
		}
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
	Point* maxPoint = &pointsIn[pos];
	real maxCross = Point::cross(*p1, *p2, pointsIn[pos]);
	for (size_t pos = start + 1; pos < stop; ++pos) {

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

void LTHull::produceOutput() {

	// wait for output flag
	tuple *flag = make_tuple("s", FLAG_OUTPUT);
	destroy_tuple(get_tuple(flag, &ctx));

	// TODO bring in all of the emitted points in order

	// remove the tuple synch lock from tuple space
	tuple *synchLock = make_tuple("s", SYNCH_LOCK);
	get_tuple(synchLock, &ctx);

}
