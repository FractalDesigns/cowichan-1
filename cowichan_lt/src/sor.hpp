#ifndef __SOR_PRIVATE_HPP__
#define __SOR_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTSor: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		static const char* SYNCH_LOCK;
		static const char* ROWS_DONE;

		static const char* SOLUTION_VECTOR;
		static const char* SOLUTION_SUM;

		static const char* SOR_FLAG;

		real solutionSum(index_t row);

	};

#endif

