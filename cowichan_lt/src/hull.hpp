#ifndef __HULL_PRIVATE_HPP__
#define __HULL_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	#define NO_POINT -1

	class LTHull: public TupleApplication {
	public:

		int getNumPoints();
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

		void split(const index_t p1, const index_t p2, index_t *order);

		void computeCross(index_t p1, index_t p2, index_t* maxPoint, real* maxCross);
		void computeMinMax(index_t* minPoint, index_t* maxPoint);

		void serviceMinMaxRequest(tuple* gotten);
		void serviceCrossRequest(tuple* gotten);

		bool isMasked(index_t position);
		void mask(index_t position);

	};

#endif

