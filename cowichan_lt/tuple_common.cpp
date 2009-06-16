#include "tuple_common.hpp"

void TupleApplication::addInput(int name, void* data) {
	inputs[name] = data;
}

void TupleApplication::addOutput(int name, void* data, int size) {
	assert(data != NULL);
	assert(size != 0);

	// Assign a patch of shared memory to communicate the output
	// of the tuple-space program's calculations.
	void* ptr = mmap(NULL, size, PROT_READ | PROT_WRITE,
		MAP_SHARED | MAP_ANON, -1, 0);
	memcpy(ptr, data, size);
	outputs[name] = ptr;

	// Store variables needed to communicate back the output.
	sizes[name] = size;
	originalOutputs[name] = data;
}

int TupleApplication::main(const char* host, int portNumber, int numWorkers) {

	// connect to the tuple server.
	if (get_server_portnumber(&ctx)) {
		if (argc < 3) {
			/* help message */
			fprintf(stderr, "Usage: %s <server> <portnumber>\n", argv[0]);
			return 1;
		}
		strcpy(ctx.peername, host);
		ctx.portnumber = portNumber;
	}

	// spawn worker processes and record their PIDs
	int workers[numWorkers];
	for (int i = 0; i < numWorkers; ++i) {		
		pid_t pid = fork();
		if (pid == 0) {
			this->work();
			exit(0);
		}
		workers[i] = pid;
	}

	// spawn input consumer
	if (fork() == 0) {
		this->consumeInput();
		exit(0);
	}

	// spawn output producer and wait for it to finish
	pid_t outputPID = fork();
	if (outputPID == 0) {
		this->produceOutput();
		exit(0);
	}
	waitpid(outputPID, NULL, 0);

	// kill all of the worker processes (they spin)
	// TODO send them tuple-space quit messages
	for (int i = 0; i < numWorkers; ++i) {
		kill(workers[i], SIGKILL);
	}

	// copy all of the mmaped outputs; delete them.
	if (this->dataPackage != NULL) {
		for (int i = 0; i < outputs.size(); ++i) {
			memcpy(this->originalOutput[i], this->output[i], this->sizes[i]);
			munmap(this->output[i]);
		}
	}

	// everything was successful.
	return 0;

}

