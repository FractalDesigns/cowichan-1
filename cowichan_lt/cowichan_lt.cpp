#include "cowichan_lt.hpp"

int main(int argc, char* argv[]) {
	Cowichan* linuxTuples = new CowichanLinuxTuples();
	linuxTuples->main(argc, argv, false, true);
	return 0;
}

const char* CowichanLinuxTuples::SERVER = "localhost";

void CowichanLinuxTuples::life(BoolMatrix /* matrixIn */, BoolMatrix /* matrixOut */) {}
void CowichanLinuxTuples::winnow(IntMatrix /* matrix */, BoolMatrix /* mask */, PointVector /* points */) {}
void CowichanLinuxTuples::norm(PointVector /* pointsIn */, PointVector /* pointsOut */) {}
void CowichanLinuxTuples::hull(PointVector /* pointsIn */, PointVector /* pointsOut */) {}
void CowichanLinuxTuples::outer(PointVector /* points */, Matrix /* matrix */, Vector /* vector */) {}
void CowichanLinuxTuples::gauss(Matrix /* matrix */, Vector /* target */, Vector /* solution */) {}
void CowichanLinuxTuples::sor(Matrix /* matrix */, Vector /* target */, Vector /* solution */) {}
void CowichanLinuxTuples::product(Matrix /* matrix */, Vector /* candidate */, Vector /* solution */) {}
real CowichanLinuxTuples::vecdiff(Vector /* actual */, Vector /* computed */) {return (real)0.0;}

  void CowichanLinuxTuples::half(IntMatrix matrixIn, IntMatrix matrixOut) {}
  void CowichanLinuxTuples::invperc(IntMatrix matrix, BoolMatrix mask) {}
  void CowichanLinuxTuples::thresh(IntMatrix matrix, BoolMatrix mask) {}

void CowichanLinuxTuples::chain(bool /* use_randmat */, bool /* use_thresh */) {}
