#ifndef __NORM_PRIVATE_HPP__
#define __NORM_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTVecdiff: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		const char* SYNCH_LOCK = "vecdiff synch lock";
		const char* ELEMENTS_DONE = "vecdiff elements reporting";
		const char* MAX_DIFF = "vecdiff max difference";

	};

#endif

