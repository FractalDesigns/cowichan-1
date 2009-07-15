#ifndef __HALF_PRIVATE_HPP__
#define __HALF_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTLife: public TupleApplication {
	protected:
		void consumeInput();
		void work();
		void produceOutput();

		index_t sumNeighbours();
	};

#endif

