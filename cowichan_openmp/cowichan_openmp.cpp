#include "cowichan_openmp.hpp"

int main(int argc, char* argv[])
{
  Cowichan* openmp = new CowichanOpenMP ();

  omp_set_num_threads(2);

  openmp->main(argc, argv, false, true);

  return 0;
}

void CowichanOpenMP::hull(PointVector /* pointsIn */, PointVector /* pointsOut */) {}
void CowichanOpenMP::outer(PointVector /* points */, Matrix /* matrix */, Vector /* vector */) {}
void CowichanOpenMP::gauss(Matrix /* matrix */, Vector /* target */, Vector /* solution */) {}
void CowichanOpenMP::sor(Matrix /* matrix */, Vector /* target */, Vector /* solution */) {}
void CowichanOpenMP::product(Matrix /* matrix */, Vector /* candidate */, Vector /* solution */) {}
real CowichanOpenMP::vecdiff(Vector /* actual */, Vector /* computed */) {return (real)0.0;}

void CowichanOpenMP::chain(bool /* use_randmat */, bool /* use_thresh */) {}
