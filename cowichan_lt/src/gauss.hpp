#ifndef __GAUSS_PRIVATE_HPP__
#define __GAUSS_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTForward: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		static const char* ROWS_DONE;
		static const char* FORWARD_DONE;
		static const char* REQUEST;

		static const char* TARGET;
		static const char* MATRIX_ROW;

	};

#endif

