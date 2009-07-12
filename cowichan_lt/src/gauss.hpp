#ifndef __GAUSS_PRIVATE_HPP__
#define __GAUSS_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTForward: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		const char* ROWS_DONE = "forward rows reporting";
		const char* FORWARD_DONE = "forward done";
	};

	class LTBackward: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		const char* SYNCH_LOCK = "";


	};

#endif

