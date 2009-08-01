#ifndef __HULL_PRIVATE_HPP__
#define __HULL_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTHull: public TupleApplication {
	protected:

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

		static const char* MASKED_POINT;
		static const char* NUM_POINTS;

		static const char* FLAG_OUTPUT;
		static const char* FINISHED_MINMAX;

		void computeMinMax(size_t n, Point* minPoint, Point* maxPoint);
		void split(PointVector pointsIn, index_t n, index_t* hn, const Point& p1, const Point& p2);
		void serviceMinMaxRequest(tuple* gotten);
		void serviceCrossRequest(tuple* gotten);

		int getNumPoints();
		bool isMasked(index_t position);

	};

#endif

