#include "cowichan_openmp.hpp"

int main(int argc, char* argv[])
{
  Cowichan* openmp = new CowichanOpenMP ();

  //omp_set_num_threads(1);

  openmp->main(argc, argv, false, true);

  return 0;
}

void CowichanOpenMP::half(IntMatrix /* matrixIn */, IntMatrix /* matrixOut */) {}
void CowichanOpenMP::invperc(IntMatrix /* matrix */, BoolMatrix /* mask */) {}
void CowichanOpenMP::thresh(IntMatrix /* matrix */, BoolMatrix /* mask */) {}
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
