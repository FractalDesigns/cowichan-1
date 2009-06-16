#ifndef __MANDEL_PRIVATE_HPP__
#define __MANDEL_PRIVATE_HPP__

	#include "tuple_common.hpp"

	typedef double real;

	#define NROWS 1000
	#define NCOLS 1000

	#define MANDEL_INFINITY	2.0
	#define MANDEL_MAX_ITER	150

	const real width = 1.0;
	const real height = 1.0;
	const real baseX = -0.5;
	const real baseY = -0.5;

	class LTmandel: public TupleApplication {
	protected:
		void consumeInput();
		void work();
		void produceOutput();
		int mandelCalc(real x, real y);
	};

#endif

