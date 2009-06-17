#ifndef __RANDMAT_PRIVATE_HPP__
#define __RANDMAT_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTRandmat: public TupleApplication {
	protected:

		INT_TYPE aPrime, cPrime;
		void setup();

		void consumeInput();
		void work();
		void produceOutput();
	};

#endif

