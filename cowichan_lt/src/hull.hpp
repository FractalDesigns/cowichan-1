#ifndef __HULL_PRIVATE_HPP__
#define __HULL_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	#define NO_POINT -1

	class LTHull: public TupleApplication {
	public:

		int getNumPoints();
		LTHull(size_t numLeft);

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
		static const char* MASKED_POINT;
		static const char* NUM_POINTS;

		static const char* FLAG_OUTPUT;
		static const char* FINISHED_MINMAX;

		void split(const Point& p1, const Point& p2, index_t *order);

		void computeCross(Point* p1, Point* p2, Point* maxPoint, real* maxCross);
		void computeMinMax(Point* minPoint, Point* maxPoint);

		void serviceMinMaxRequest(tuple* gotten);
		void serviceCrossRequest(tuple* gotten);

		bool isMasked(index_t position);

	};

#endif

