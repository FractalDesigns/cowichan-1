/**
 * \file cowichan_lt/src/outer.hpp
 * \brief LinuxTuples outer product header file.
 * \see CowichanLinuxTuples::outer
 */

#ifndef __OUTER_PRIVATE_HPP__
#define __OUTER_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	/**
	 * Performs the outer product with LinuxTuples.
	 */
	class LTOuter: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		static const char* SYNCH_LOCK;
		static const char* REQUEST;

		static const char* MAX_DISTANCE;
		static const char* MATRIX_ENTRY;

	};

#endif

