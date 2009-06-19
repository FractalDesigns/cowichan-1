#ifndef __RANDMAT_PRIVATE_HPP__
#define __RANDMAT_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTRandmat: public TupleApplication {
	public:
	
		void setup();

	protected:

		INT_TYPE aPrime, cPrime;

		void consumeInput();
		void work();
		void produceOutput();
	};

#endif

