/**
 * \file cowichan_lt/src/gauss.hpp
 * \brief LinuxTuples gauss header file.
 * \see CowichanLinuxTuples::gauss
 */

#ifndef __GAUSS_PRIVATE_HPP__
#define __GAUSS_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	/**
	 * Forward-elimination tuple application. Performs
	 * forward elimination on a square matrix in parallel.
	 */
	class LTForward: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		static const char* ROWS_DONE;
		static const char* FORWARD_DONE;
		static const char* REQUEST;

		static const char* TARGET;
		static const char* MATRIX_ROW;

	};

#endif

