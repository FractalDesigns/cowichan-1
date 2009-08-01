#ifndef __NORM_PRIVATE_HPP__
#define __NORM_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTBounds: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		static const char* SYNCH_LOCK;
		static const char* POINTS_DONE;
		static const char* MIN_POINT;
		static const char* MAX_POINT;

	};

	class LTNorm: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		static const char* MIN_POINT;
		static const char* MAX_POINT;

	};

#endif

