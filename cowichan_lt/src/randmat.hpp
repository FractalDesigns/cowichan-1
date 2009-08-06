/**
 * \file randmat.hpp
 * \brief LinuxTuples header file for the randmat cowichan problem.
 */
#ifndef __RANDMAT_PRIVATE_HPP__
#define __RANDMAT_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	/**
	 * Tuple application to solve the bulk of the randmat cowichan problem.
	 */
	class LTRandmat: public TupleApplication {
	protected:

		INT_TYPE aPrime, cPrime;

		void consumeInput();
		void work();
		void produceOutput();

		/**
		 * Create the first, seed column.
		 */
		void setup();

		/**
		 * Computes the next value in the random sequence.
		 * \param current the previous value in the random sequence.
		 * \return the next value in the random sequence.
		 */
		INT_TYPE next(INT_TYPE& current) const;

		static const char *REQUEST;

	};

#endif

