/**
 * \file cowichan_lt/src/vecdiff.hpp
 * \brief LinuxTuples vector difference header file.
 * \see CowichanLinuxTuples::vecdiff
 */

#ifndef __VECDIFF_PRIVATE_HPP__
#define __VECDIFF_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	/**
	 * Performs the vector difference with LinuxTuples.
	 */
	class LTVecdiff: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		static const char* SYNCH_LOCK;
		static const char* ELEMENTS_DONE;
		static const char* MAX_DIFF;

	};

#endif

