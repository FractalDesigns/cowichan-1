#include "cowichan_openmp.hpp"

int main(int argc, char* argv[])
{
  Cowichan* openmp = new CowichanOpenMP ();

  omp_set_num_threads(2);

  openmp->main(argc, argv, false, true);

  return 0;
}

real CowichanOpenMP::vecdiff(Vector /* actual */, Vector /* computed */) {return (real)0.0;}

void CowichanOpenMP::chain(bool /* use_randmat */, bool /* use_thresh */) {}
