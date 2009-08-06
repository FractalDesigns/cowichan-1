#ifndef __HALF_PRIVATE_HPP__
#define __HALF_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTHalf: public TupleApplication {
	protected:
		void consumeInput();
		void work();
		void produceOutput();
		static const char *REQUEST, *DONE;

	};

#endif

