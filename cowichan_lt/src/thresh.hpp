#ifndef __THRESH_PRIVATE_HPP__
#define __THRESH_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTFrequency: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		static const char* SYNCH_LOCK;
		static const char* ROWS_DONE;

	};

	class LTThresh: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

	};

#endif

