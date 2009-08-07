/**
 * \file cowichan_lt/src/sor.hpp
 * \brief LinuxTuples successive over-relaxation header file.
 * \see CowichanLinuxTuples::sor
 */

#ifndef __SOR_PRIVATE_HPP__
#define __SOR_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	/**
	 * Performs a successive over-relaxation to solve a matrix
	 * problem Ax = b. The solution is approximate, but it can be made
	 * arbitrarily accurate. The computation is done in tuple space.
	 */
	class LTSor: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		static const char* SYNCH_LOCK;
		static const char* ROWS_DONE;

		static const char* SOLUTION_VECTOR;
		static const char* SOLUTION_SUM;

		static const char* SOR_FLAG;

		/**
		 * Performs the sum portion of the SOR processor.
		 * \param row the row to perform on.
		 * \return the sum.
		 */
		real solutionSum(index_t row);

	};

#endif

