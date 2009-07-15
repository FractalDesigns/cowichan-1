#ifndef __PRODUCT_PRIVATE_HPP__
#define __PRODUCT_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTProduct: public TupleApplication {
	protected:
		void consumeInput();
		void work();
		void produceOutput();
	};

#endif

