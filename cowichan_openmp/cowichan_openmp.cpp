#include "cowichan_openmp.hpp"

int main(int argc, char* argv[])
{
  int th_id, nthreads;
#pragma omp parallel private(th_id)
  {
    th_id = omp_get_thread_num();
    std::cout << "Hello World from thread" << th_id << "\n";
#pragma omp barrier
    if ( th_id == 0 ) {
      nthreads = omp_get_num_threads();
      std::cout << "There are " << nthreads << " threads\n";
    }
  }

  Cowichan* openmp = new CowichanOpenMP ();

  openmp->main(argc, argv, false, true);

  return 0;
}

void CowichanOpenMP::randmat(IntMatrix /* matrix */) {}
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
