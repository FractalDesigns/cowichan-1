#include "cowichan_lt.hpp"

int main(int argc, char* argv[]) {
	Cowichan* linuxTuples = new CowichanLinuxTuples();
	linuxTuples->main(argc, argv, false, true);
	return 0;
}

const char* CowichanLinuxTuples::SERVER = "localhost";

void CowichanLinuxTuples::chain(bool /* use_randmat */, bool /* use_thresh */) {}
