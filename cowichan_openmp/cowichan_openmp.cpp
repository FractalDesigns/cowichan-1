#include "cowichan_openmp.hpp"

int main(int argc, char* argv[])
{
  Cowichan* openmp = new CowichanOpenMP ();

  omp_set_num_threads(2);

  openmp->main(argc, argv, false, true);

  return 0;
}

