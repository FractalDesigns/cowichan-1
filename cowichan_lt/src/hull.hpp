/**
 * \file cowichan_lt/src/hull.hpp
 * \brief LinuxTuples iterative convex hull header file.
 * \see CowichanLinuxTuples::hull
 */

#ifndef __HULL_PRIVATE_HPP__
#define __HULL_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	/**
	 * Indicator value to denote no valid point is stored.
	 */
	#define NO_POINT -1

	/**
	 * Performs an iterative convex hull in tuple space.
	 * Iterative convex hull means that the output contains the convex hull of
	 * all points, followed by the convex hull of those points not on the first
	 * convex hull, followed by the convex hull of those points not on the first
	 * or second, etc. etc. until no points remain. Thus the iterative convex hull
	 * can be seen as a spiralling shape, or a permutation of the input points.
	 */
	class LTHull: public TupleApplication {
	public:

		/**
		 * Returns the number of points in the convex hull.
		 */
		int getNumPoints();

		/**
		 * Constructor.
		 * \param numLeft the number of points left out of the original batch.
		 */
		LTHull(size_t numLeft);

		// public, so we can clean-up outside of LTHull's tuple app.
		static const char* MASKED_POINT;

	protected:

		size_t numLeft;

		void consumeInput();
		void work();
		void produceOutput();

		static const char* REQUEST;
		static const char* REQUEST_MINMAX;
		static const char* REQUEST_CROSS;

		static const char* SYNCH_LOCK;
		static const char* POINTS_DONE;

		static const char* MIN_X_POINT;
		static const char* MAX_X_POINT;
		static const char* MAX_CROSS;
		static const char* MAX_POINT;

		static const char* HULL_POINT;
		static const char* NUM_POINTS;

		static const char* FLAG_OUTPUT;
		static const char* FINISHED_MINMAX;

		/**
		 * Analogous to split in other versions of quickhull.
		 * split again based on the furthest point from the line denoted
		 * by the two points given (p1 and p2).
		 */
		void split(const index_t p1, const index_t p2, index_t *order);

		/**
		 * Initiate a tuple-space cross-product over multiple points.
		 */
		void computeCross(index_t p1, index_t p2, index_t* maxPoint, real* maxCross);

		/**
		 * Initiate a tuple-space min/max routine over multiple points.
		 */
		void computeMinMax(index_t* minPoint, index_t* maxPoint);

		/**
		 * Worker helper to service a min/max request.
		 */
		void serviceMinMaxRequest(tuple* gotten);

		/**
		 * Worker helper to server a cross-product request.
		 */
		void serviceCrossRequest(tuple* gotten);

		/**
		 * Is the given point "masked", i.e., should we skip it?
		 * \param position the point to check.
		 * \return true if the point is to be skipped, false otherwise.
		 */
		bool isMasked(index_t position);

		/**
		 * Mask the given point (make sure we skip it next time we do a convex hull
		 * computation).
		 * \param position the point to mask.
		 */
		void mask(index_t position);

	};

#endif

