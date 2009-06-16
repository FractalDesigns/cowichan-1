#include "cowichan_lt.hpp"

int main(int argc, char* argv[]) {
	Cowichan* linuxTuples = new CowichanLinuxTuples();
	linuxTuples->main(argc, argv, false, true);
	return 0;
}

void CowichanOpenMP::life(BoolMatrix /* matrixIn */, BoolMatrix /* matrixOut */) {}
void CowichanOpenMP::winnow(IntMatrix /* matrix */, BoolMatrix /* mask */, PointVector /* points */) {}
void CowichanOpenMP::norm(PointVector /* pointsIn */, PointVector /* pointsOut */) {}
void CowichanOpenMP::hull(PointVector /* pointsIn */, PointVector /* pointsOut */) {}
void CowichanOpenMP::outer(PointVector /* points */, Matrix /* matrix */, Vector /* vector */) {}
void CowichanOpenMP::gauss(Matrix /* matrix */, Vector /* target */, Vector /* solution */) {}
void CowichanOpenMP::sor(Matrix /* matrix */, Vector /* target */, Vector /* solution */) {}
void CowichanOpenMP::product(Matrix /* matrix */, Vector /* candidate */, Vector /* solution */) {}
real CowichanOpenMP::vecdiff(Vector /* actual */, Vector /* computed */) {return (real)0.0;}

void CowichanOpenMP::chain(bool /* use_randmat */, bool /* use_thresh */) {}
