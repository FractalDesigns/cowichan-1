#ifndef __THRESH_PRIVATE_HPP__
#define __THRESH_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTThresh: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		const char* SYNCH_LOCK = "thresh synch lock";
		const char* ROWS_DONE = "thresh rows reporting";

	};

#endif

