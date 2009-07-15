#ifndef __SOR_PRIVATE_HPP__
#define __SOR_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTSor: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		const char* SYNCH_LOCK = "sor synch lock";
		const char* ROWS_DONE = "sor rows reporting";
		const char* MAX_DIFF = "sor max difference";
		const char* SOLUTION_ROW = "sor solution row";

	};

#endif

