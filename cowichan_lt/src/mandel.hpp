#ifndef __MANDEL_PRIVATE_HPP__
#define __MANDEL_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	class LTMandel: public TupleApplication {
	protected:
		void consumeInput();
		void work();
		void produceOutput();
		int mandelCalc(real x, real y);
	};

#endif

