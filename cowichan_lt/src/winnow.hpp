#ifndef __WINNOW_PRIVATE_HPP__
#define __WINNOW_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTWinnow: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		Point nextWeightedPoint(tuple *recv, INT_TYPE* order);

		static const char* SYNCH_LOCK;
		static const char* REQUEST;
		static const char* WEIGHTED_POINT;
		static const char* ROWS_REPORTING;
		static const char* COUNT;

	};

#endif

