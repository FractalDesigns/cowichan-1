#ifndef __MANDEL_PRIVATE_HPP__
#define __MANDEL_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	const real width = 1.0;
	const real height = 1.0;
	const real baseX = -0.5;
	const real baseY = -0.5;

	class LTMandel: public TupleApplication {
	protected:
		void consumeInput();
		void work();
		void produceOutput();
		int mandelCalc(real x, real y);
	};

#endif

