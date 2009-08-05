#ifndef __VECDIFF_PRIVATE_HPP__
#define __VECDIFF_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTOuter: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		static const char* SYNCH_LOCK;
		static const char* REQUEST;

		static const char* MAX_DISTANCE;
		static const char* MATRIX_ENTRY;

	};

#endif

