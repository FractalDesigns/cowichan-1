#ifndef __THRESH_PRIVATE_HPP__
#define __THRESH_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTBounds: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		const char* SYNCH_LOCK = "norm synch lock";
		const char* POINTS_DONE = "norm points reporting";
		const char* MIN_POINT = "norm minPoint";
		const char* MAX_POINT = "norm maxPoint";

	};

	class LTNorm: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		const char* MIN_POINT = "norm minPoint";
		const char* MAX_POINT = "norm maxPoint";

	};

#endif

