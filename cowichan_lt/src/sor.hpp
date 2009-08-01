#ifndef __SOR_PRIVATE_HPP__
#define __SOR_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTSor: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		static const char* SYNCH_LOCK = "sor synch lock";
		static const char* ROWS_DONE = "sor rows reporting";

		static 	const char* SOLUTION_VECTOR = "sor solution row";
		static const char* SOLUTION_SUM = "sor inner sum";

		static const char* SOR_FLAG = "sor input consumed";

		real solutionSum(index_t row);

	};

#endif

