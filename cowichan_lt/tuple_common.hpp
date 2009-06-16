#ifndef __TUPLE_COMMON_HPP__
#define __TUPLE_COMMON_HPP__

#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <string.h>
#include <sys/wait.h>
#include <cassert>

extern "C" {
	#include "tuple.h"
}

#include <map>

class TupleApplication {
protected:

	std::map<int, void*> inputs;
	std::map<int, void*> outputs;
	std::map<int, void*> originalOutputs;
	std::map<int, int> sizes;

	/**
	 * Processes will be forked and spawned here.
	 */
	virtual void consumeInput()		= 0;
	virtual void work()				= 0;
	virtual void produceOutput()	= 0;

public:

	/**
	 * Set up input/output pointers.
	 */
	void addInput(int name, void* data);
	void addOutput(int name, void* data, int size);

	/**
 	 * Starts the tuple-space job.
	 */
	int start(const char* host, int portNumber, int numWorkers);

};

TUPLE_DO()

#define TUPLE_DO(DATA, )

#endif

