/**
 * \file cowichan_lt/src/thresh.hpp
 * \brief LinuxTuples histogram thresholding header file.
 * \see CowichanLinuxTuples::thresh
 */

#ifndef __THRESH_PRIVATE_HPP__
#define __THRESH_PRIVATE_HPP__

	#include "tuple_common.hpp"
	#include "cowichan_lt.hpp"

	/**
	 * Counts the frequency of different values in a matrix;
	 * figures out the breaking point so that a certain percentage
	 * lay under the mark.
	 */
	class LTFrequency: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		static const char* SYNCH_LOCK;
		static const char* ROWS_DONE;
		static const char* REQUEST;
		static const char* POINT;

	};

	/**
	 * Uses the breaking-point discovered with LTFrequency to
	 * create a boolean matrix of true (above the breaking point) or
	 * false (not).
	 */
	class LTThresh: public TupleApplication {
	protected:

		void consumeInput();
		void work();
		void produceOutput();

		static const char* REQUEST;
		static const char* DONE;
		static const char* POINT;

	};

#endif

