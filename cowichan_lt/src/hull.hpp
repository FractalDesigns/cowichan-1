#ifndef __HULL_PRIVATE_HPP__
#define __HULL_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTHull: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		const char* REQUEST = "hull request";
		const char* REQUEST_MINMAX = "hull request min/max";
		const char* REQUEST_CROSS = "hull request cross-product";

		const char* SYNCH_LOCK = "hull synch lock";
		const char* POINTS_DONE = "hull points reporting";

		const char* MIN_X_POINT = "hull minPoint";
		const char* MAX_X_POINT = "hull maxPoint";
		const char* MAX_CROSS = "hull max cross-product";
		const char* MAX_POINT = "hull furthest point";

		const char* FLAG_OUTPUT = "hull flag output";
		const char* FINISHED_MINMAX = "hull finshed min/max";

		void computeMinMax(size_t n);

	};

#endif

